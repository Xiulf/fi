use crate::ctx::*;
use crate::error::*;
use crate::ty::*;
use hir::ir::Span;
use std::collections::HashMap;

impl<'db> Ctx<'db> {
    crate fn infer_kind(&mut self, ty: Ty) -> Result<(Ty, Ty)> {
        match &*ty {
            Type::Error => Ok((ty.clone(), Ty::error(ty.span(), ty.file()))),
            Type::Ctor(id) => Ok((ty.clone(), self.db.typecheck(*id).ty.clone() ^ ty.loc())),
            Type::Int(_) => Ok((ty.clone(), self.figure_kind(ty.span(), ty.file()))),
            Type::String(_) => Ok((ty.clone(), self.symbol_kind(ty.span(), ty.file()))),
            Type::Tuple(tys) => {
                for ty in tys {
                    let ty_kind = self.ty_kind(ty.span(), ty.file());

                    self.check_kind(ty, ty_kind)?;
                }

                Ok((ty.clone(), self.ty_kind(ty.span(), ty.file())))
            }
            Type::Var(v) => {
                let kind = self.tys[&v.0].clone();

                Ok((ty.clone(), kind ^ ty.loc()))
            }
            Type::Skolem(_, None, _, _) => Err(TypeError::Internal("skolem has no kind".into())),
            Type::Skolem(_, Some(k), _, _) => {
                let kind = self.subst_type(k.clone());

                Ok((ty.clone(), kind ^ ty.loc()))
            }
            Type::Unknown(u) => {
                let kind = self.subst.unsolved[u].1.clone();
                let kind = self.subst_type(kind);

                Ok((ty.clone(), kind ^ ty.loc()))
            }
            Type::App(t1, t2) => {
                let (t1, k1) = self.infer_kind(t1.clone())?;

                self.infer_app_kind(ty.span(), ty.file(), t1, k1, t2)
            }
            Type::KindApp(t1, t2) => {
                let (t1, kind) = self.infer_kind(t1.clone())?;
                let t1 = self.subst_type(t1);
                let kind = self.subst_type(kind);

                if let Type::ForAll(vars, ret, _) = &*kind {
                    let args = vars
                        .into_iter()
                        .zip(t2)
                        .map(|((_v, k), t2)| {
                            if let Some(k) = k {
                                self.check_kind(t2, k)
                            } else {
                                Err(TypeError::Internal("unkinded forall binder".into()))
                            }
                        })
                        .collect::<Result<List<_>>>()?;

                    let repl = (&args)
                        .into_iter()
                        .zip(vars)
                        .map(|(k, (v, _))| (v, k))
                        .collect();

                    let kind = ret.clone().replace_vars(repl);
                    let ty = Ty::kind_app(ty.span(), ty.file(), t1, args);

                    Ok((ty, kind))
                } else {
                    Err(TypeError::Internal("unkinded forall binder".into()))
                }
            }
            Type::ForAll(vars, ret, sc) => {
                let var_kinds = vars
                    .into_iter()
                    .map(|(v, k)| {
                        Ok((
                            v,
                            match k {
                                Some(k) => Some(
                                    self.check_kind(k.clone(), self.ty_kind(k.span(), k.file()))?,
                                ),
                                None => Some(self.fresh_kind(ty.span(), ty.file())),
                            },
                        ))
                    })
                    .collect::<Result<List<_>>>()?;

                for (v, k) in &var_kinds {
                    self.tys.insert(v.0, k.unwrap());
                }

                let ty2 = self.check_kind(ret.clone(), self.ty_kind(ret.span(), ret.file()))?;
                let ty2 = self.subst_type(ty2);
                let unks = self.unknowns_with_kinds(ty2.unknowns());

                for (u, k) in unks {
                    self.add_unsolved(None, u, k);
                }

                let ty = Ty::forall(ty.span(), ty.file(), var_kinds, ty2, *sc);

                Ok((ty.clone(), self.ty_kind(ty.span(), ty.file())))
            }
            Type::Ctnt(ctnt, ret) => {
                // @TODO: check/apply constraint
                let ty_kind = self.ty_kind(ret.span(), ret.file());
                let ret = self.check_kind(ret.clone(), ty_kind)?;
                let kind = self.ty_kind(ty.span(), ty.file());
                let ty = Ty::ctnt(ty.span(), ty.file(), ctnt.clone(), ret);

                Ok((ty, kind))
            }
            _ => unimplemented!("infer kind: {}", crate::display::Typed(self.db, &(), &ty)),
        }
    }

    fn unknowns_with_kinds(
        &mut self,
        uk: std::collections::HashSet<Unknown>,
    ) -> Vec<(Unknown, Ty)> {
        let mut uk = uk
            .into_iter()
            .map(|u| {
                let (lvl, k) = self.subst.unsolved[&u].clone();
                let k = self.subst_type(k);

                (lvl, u, k)
            })
            .collect::<Vec<_>>();

        uk.sort_by(|(a, _, _), (b, _, _)| a.cmp(b));

        let mut uk = uk.into_iter().map(|(_, u, k)| (u, k)).collect::<Vec<_>>();

        uk.dedup();
        uk
    }

    fn fresh_kind(&mut self, span: Span, file: source::FileId) -> Ty {
        self.fresh_kind_with_kind(span, file, self.ty_kind(span, file))
    }

    fn fresh_kind_with_kind(&mut self, span: Span, file: source::FileId, kind: Ty) -> Ty {
        let u = self.fresh_unknown();

        self.add_unsolved(None, u, kind);

        Ty::unknown(span, file, u)
    }

    crate fn infer_app_kind(
        &mut self,
        span: Span,
        file: source::FileId,
        fn_ty: Ty,
        fn_kind: Ty,
        args: &List<Ty>,
    ) -> Result<(Ty, Ty)> {
        match &*fn_kind {
            Type::App(f, targs) if self.is_func(f) && targs.len() == 2 => {
                if let Type::Tuple(params) = &*targs[0] {
                    assert_eq!(args.len(), params.len());
                    let args = args
                        .into_iter()
                        .zip(params)
                        .map(|(arg_ty, arg_kind)| {
                            // println!(
                            //     "{} :: {}",
                            //     crate::display::Typed(self.db, &(), &arg_ty),
                            //     crate::display::Typed(self.db, &(), &arg_kind)
                            // );
                            self.check_kind(arg_ty, arg_kind)
                        })
                        .collect::<Result<List<_>>>()?;

                    let ty = Ty::app(span, file, fn_ty, args);

                    Ok((ty, self.subst_type(targs[1].clone())))
                } else {
                    unreachable!();
                }
            }
            Type::Unknown(u) => {
                let lvl = self.subst.unsolved[u].0.clone();
                let ret = self.fresh_unknown();
                let ty_kind = self.ty_kind(span, file);
                let args2 = (0..args.len())
                    .map(|_| {
                        let u = self.fresh_unknown();

                        self.add_unsolved(Some(lvl.clone()), u, ty_kind.clone());

                        Ty::unknown(span, file, u)
                    })
                    .collect::<List<_>>();

                self.add_unsolved(Some(lvl), ret, ty_kind);

                let ret = Ty::unknown(span, file, ret);
                let fn_ty = self.func_ty(span, file);
                let args3 = Ty::tuple(span, file, args2.clone());
                let ty1 = Ty::app(span, file, fn_ty.clone(), List::from([args3, ret.clone()]));

                self.solve(*u, ty1);

                let args3 = args
                    .into_iter()
                    .zip(args2)
                    .map(|(arg, kind)| self.check_kind(arg, kind))
                    .collect::<Result<List<_>>>()?;

                let args3 = Ty::tuple(span, file, args3);
                let ty2 = Ty::app(span, file, fn_ty, List::from([args3]));

                Ok((ty2, ret))
            }
            Type::ForAll(..) => unimplemented!(),
            _ => Err(self.cant_apply_types(fn_ty, args)),
        }
    }

    fn cant_apply_types(&mut self, fn_ty: Ty, args: &List<Ty>) -> TypeError {
        let arg_kinds = args
            .into_iter()
            .filter_map(|a| self.infer_kind(a).ok().map(|(_, k)| k))
            .collect::<List<_>>();
        let args2 = Ty::tuple(Default::default(), self.file, arg_kinds);
        let ret = self.fresh_kind(Default::default(), self.file);
        let func_ty = self.func_ty(Default::default(), self.file);
        let fn_ty2 = Ty::app(
            Default::default(),
            self.file,
            func_ty,
            List::from([args2, ret]),
        );

        let _ = self.check_kind(fn_ty.clone(), fn_ty2);
        let ty = Ty::app(Default::default(), self.file, fn_ty, args.clone());
        let msg = format!(
            "cannot apply types to type: `{}`",
            crate::display::Typed(self.db, &(), &ty)
        );

        TypeError::Internal(msg)
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
            (Type::Unknown(u), _) => self.solve_unknown(*u, k2),
            (_, Type::Unknown(u)) => self.solve_unknown(*u, k1),
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
                    Ok(self.subst_type(kind.clone()) ^ ty.loc())
                } else {
                    panic!("skolem has no kind");
                }
            }
            Type::Unknown(u) => {
                let kind = self.subst.unsolved[u].1.clone();

                Ok(self.subst_type(kind) ^ ty.loc())
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

    crate fn promote_kind(&mut self, u2: Unknown, ty: Ty) -> Result<Ty> {
        let lvl2 = match self.subst.unsolved.get(&u2) {
            Some((lvl2, _)) => lvl2.clone(),
            None => {
                return Err(TypeError::Internal(format!(
                    "unsolved unification variable ?{} is not bound",
                    u2.0
                )))
            }
        };

        ty.everywhere_result(&mut |t| match &*t {
            Type::Unknown(u1) => {
                if *u1 == u2 {
                    panic!("infinite kind");
                }

                let (lvl1, k) = self.subst.unsolved[&u1].clone();

                if lvl1 < lvl2 {
                    Ok(t)
                } else {
                    let k = self.promote_kind(u2, self.subst_type(k))?;
                    let u1_ = self.fresh_unknown();
                    let ty = Ty::unknown(t.span(), t.file(), u1_);

                    self.add_unsolved(Some(lvl2.clone()), u1_, k);
                    self.solve(*u1, ty.clone());

                    Ok(ty)
                }
            }
            _ => Ok(t),
        })
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

    fn solve_unknown(&mut self, u: Unknown, p1: Ty) -> Result<()> {
        let p2 = self.promote_kind(u, p1)?;
        let w1 = self.subst.unsolved[&u].1.clone();
        let k = self.elaborate_kind(&p2)?;
        let _ = self.unify_kinds(self.subst_type(w1), k)?;

        Ok(self.solve(u, p2))
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
