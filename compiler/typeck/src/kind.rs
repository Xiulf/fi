use crate::ctx::*;
use crate::error::*;
use crate::ty::*;
use hir::ir::Span;
use std::collections::HashMap;

impl<'db> Ctx<'db> {
    crate fn infer_kind(&mut self, ty: Ty) -> Result<(Ty, Ty)> {
        match &*ty {
            Type::Error => Ok((ty.clone(), Ty::error(ty.span(), ty.file()))),
            _ => unimplemented!(),
        }
    }

    crate fn check_kind(&mut self, ty: Ty, k2: Ty) -> Result<Ty> {
        let (ty, k1) = self.infer_kind(ty)?;
        let k1 = self.subst_type(k1);
        let k2 = self.subst_type(k2);

        self.instantiate_kind(ty, k1, k2)
    }

    crate fn check_type_kind(&mut self, k: Ty) -> Result<()> {
        let ty_kind = self.ty_kind(k.span(), k.file());

        self.unify_kinds(k, ty_kind)
    }

    crate fn instantiate_kind(&mut self, ty: Ty, k1: Ty, k2: Ty) -> Result<Ty> {
        match &*k1 {
            Type::ForAll(vars, ret, _) if k2.is_mono_type() => {
                let args = vars
                    .into_iter()
                    .map(|(_, k)| self.fresh_type_with_kind(ty.span(), ty.file(), k.unwrap()))
                    .collect::<List<_>>();

                let us = vars.into_iter().map(|v| v.0).zip(args.clone()).collect();
                let a = ret.clone().replace_vars(us);
                let ty = Ty::kind_app(ty.span(), ty.file(), ty, args);

                self.instantiate_kind(ty, a, k2)
            }
            _ => {
                self.subsumes_kind(k1, k2)?;

                Ok(ty)
            }
        }
    }

    crate fn subsumes_kind(&mut self, k1: Ty, k2: Ty) -> Result<()> {
        match (&*k1, &*k2) {
            (Type::App(f1, a1), Type::App(f2, a2))
                if self.is_func(f1) && self.is_func(f2) && a1.len() == 2 && a2.len() == 2 =>
            {
                self.subsumes_kind(a2[0].clone(), a1[0].clone())?;
                self.subsumes_kind(
                    self.subst_type(a1[1].clone()),
                    self.subst_type(a2[1].clone()),
                )
            }
            (_, Type::ForAll(vars, ret, sc)) => {
                let sc = sc.unwrap_or_else(|| self.new_skolem_scope());
                let skolems = (0..vars.len())
                    .map(|_| self.new_skolem_constant())
                    .collect();
                let sk = self.skolemize(k2.span(), k2.file(), vars, skolems, ret.clone(), sc);

                self.subsumes_kind(k1, sk)
            }
            (Type::ForAll(vars, ret, _), _) => {
                let subst = vars
                    .into_iter()
                    .map(|(v, k)| {
                        (
                            v,
                            self.fresh_type_with_kind(k1.span(), k1.file(), k.unwrap()),
                        )
                    })
                    .collect();

                self.subsumes_kind(ret.clone().replace_vars(subst), k2)
            }
            (Type::Unknown(u), Type::App(f, a)) if self.is_func(f) && a.len() == 2 => {
                let f = self.solve_unknown_as_func(k1.span(), k1.file(), *u)?;

                self.subsumes_kind(f, k2)
            }
            (Type::App(f, a), Type::Unknown(u)) if self.is_func(f) && a.len() == 2 => {
                let f = self.solve_unknown_as_func(k2.span(), k2.file(), *u)?;

                self.subsumes_kind(k1, f)
            }
            (_, _) => self.unify_kinds(k1, k2),
        }
    }

    crate fn unify_kinds(&mut self, k1: Ty, k2: Ty) -> Result<()> {
        match (&*k1, &*k2) {
            (Type::App(a1, b1), Type::App(a2, b2))
            | (Type::KindApp(a1, b1), Type::KindApp(a2, b2)) => {
                self.unify_kinds(a1.clone(), a2.clone())?;

                b1.into_iter()
                    .zip(b2)
                    .map(|(a, b)| self.unify_kinds(a, b))
                    .collect()
            }
            (_, _) if k1.equal(&k2) => Ok(()),
            (Type::Unknown(u), _) => {
                self.solve(*u, k2);
                Ok(())
            }
            (_, Type::Unknown(u)) => {
                self.solve(*u, k1);
                Ok(())
            }
            (_, _) => Err(TypeError::KindMismatch(k1, k2)),
        }
    }

    crate fn elaborate_kind(&mut self, ty: &Ty) -> Result<Ty> {
        match &**ty {
            Type::Error => Ok(Ty::error(ty.span(), ty.file())),
            Type::String(_) => Ok(self.symbol_kind(ty.span(), ty.file())),
            Type::Int(_) => Ok(self.figure_kind(ty.span(), ty.file())),
            Type::ForAll(..) => Ok(self.ty_kind(ty.span(), ty.file())),
            Type::Ctnt(..) => Ok(self.ty_kind(ty.span(), ty.file())),
            Type::Tuple(_) => Ok(self.ty_kind(ty.span(), ty.file())),
            Type::Row(fields, _) => {
                if fields.is_empty() {
                    unimplemented!();
                } else {
                    let k1 = self.elaborate_kind(&fields[0].ty)?;
                    let kind_row = self.row_kind(ty.span(), ty.file());

                    Ok(Ty::app(ty.span(), ty.file(), kind_row, List::from([k1])))
                }
            }
            Type::Skolem(_, kind, _, _) => {
                if let Some(kind) = kind {
                    Ok(self.subst_type(kind.clone()))
                } else {
                    panic!("skolem has no kind");
                }
            }
            Type::Unknown(u) => {
                let kind = self.subst.unsolved[u].1.clone();

                Ok(self.subst_type(kind))
            }
            Type::Var(v) => {
                let kind = self.tys[&v.0].clone();
                let kind = self.subst_type(kind);

                Ok(kind ^ ty.loc())
            }
            Type::Ctor(id) => Ok(self.db.typecheck(*id).ty.clone()),
            Type::App(base, _) => {
                let k1 = self.elaborate_kind(base)?;

                match &*k1 {
                    Type::App(f, a) if self.is_func(f) && a.len() == 2 => {
                        Ok(a[1].clone() ^ ty.loc())
                    }
                    Type::Unknown(u) => {
                        self.solve_unknown_as_func(k1.span(), k1.file(), *u)?;
                        self.elaborate_kind(ty)
                    }
                    _ => panic!("cannot apply types"),
                }
            }
            Type::KindApp(base, args) => {
                let k1 = self.elaborate_kind(base)?;

                if let Type::ForAll(vars, ret, _) = &*k1 {
                    let repl = vars
                        .into_iter()
                        .zip(args)
                        .map(|((v, _), arg)| (v, self.subst_type(arg)))
                        .collect();

                    Ok(ret.clone().replace_vars(repl) ^ ty.loc())
                } else {
                    panic!("cannot apply kind to type");
                }
            }
        }
    }

    crate fn kind_of(&mut self, ty: Ty) -> Result<(Ty, Ty)> {
        let (_, ty, kind) = self.kind_of_with_vars(ty)?;

        Ok((ty, kind))
    }

    crate fn kind_of_with_vars(&mut self, ty: Ty) -> Result<(HashMap<TypeVar, Ty>, Ty, Ty)> {
        let (ty, kind) = self.infer_kind(ty)?;
        let ty = self.subst_type(ty);
        let kind = self.subst_type(kind);
        let vars = self.complete_var_list(&ty);

        Ok((vars, ty, kind))
    }

    crate fn complete_var_list(&self, ty: &Ty) -> HashMap<TypeVar, Ty> {
        if let Type::ForAll(vars, ..) = &**ty {
            vars.into_iter()
                .filter_map(|(v, k)| Some((v, k?)))
                .collect()
        } else {
            HashMap::new()
        }
    }

    fn solve_unknown_as_func(
        &mut self,
        span: Span,
        file: source::FileId,
        u: Unknown,
    ) -> Result<Ty> {
        let lvl = self.subst.unsolved[&u].0.clone();
        let u1 = self.fresh_unknown();
        let u2 = self.fresh_unknown();

        self.add_unsolved(Some(lvl.clone()), u1, self.ty_kind(span, file));
        self.add_unsolved(Some(lvl), u2, self.ty_kind(span, file));

        let func = self.func_ty(span, file);
        let args = Ty::tuple(span, file, List::from([Ty::unknown(span, file, u1)]));
        let uarr = Ty::app(
            span,
            file,
            func,
            List::from([args, Ty::unknown(span, file, u2)]),
        );

        self.solve(u, uarr.clone());

        Ok(uarr)
    }

    fn add_unsolved(&mut self, lvl: Option<UnkLevel>, u: Unknown, kind: Ty) {
        let mut new_lvl = lvl.unwrap_or(UnkLevel(Vec::new()));

        new_lvl.0.push(u);
        self.subst.unsolved.insert(u, (new_lvl, kind));
    }

    fn solve(&mut self, u: Unknown, s: Ty) {
        self.subst.tys.insert(u, s);
    }
}
