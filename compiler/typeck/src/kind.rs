use crate::ctx::*;
use crate::error::*;
use crate::ty::*;
use hir::ir::Span;

impl<'db> Ctx<'db> {
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
            (_, _) => self.unify_kinds(k1, k2),
        }
    }

    crate fn unify_kinds(&mut self, k1: Ty, k2: Ty) -> Result<()> {
        match (&*k1, &*k2) {
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
            Type::Var(_) => unimplemented!(),
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
