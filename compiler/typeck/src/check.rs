use crate::ctx::*;
use crate::error::*;
use crate::ty::*;
use hir::ir;

impl<'db> Ctx<'db> {
    crate fn check_body(&mut self, span: ir::Span, body: &ir::Body, ty: Ty) -> Result<()> {
        let (elab_ty, kind) = self.kind_of(ty)?;

        self.check_type_kind(kind)?;
        self.check_body_(span, body, elab_ty)
    }

    fn check_body_(&mut self, span: ir::Span, body: &ir::Body, ty: Ty) -> Result<()> {
        match &*ty {
            Type::Ctnt(_ctnt, ty) => self.check_body_(span, body, ty.clone()),
            Type::ForAll(vars, ret, _) => {
                let scope = self.new_skolem_scope();
                let skolems = (0..vars.len())
                    .map(|_| self.new_skolem_constant())
                    .collect();
                let sk = self.skolemize(ty.span(), ty.file(), vars, skolems, ret.clone(), scope);

                self.check_body_(span, body, sk)
            }
            Type::App(f, a) if self.is_func(f) && a.len() == 2 => {
                if let Type::Tuple(params) = &*a[0] {
                    let ret = a[1].clone();

                    for (param, ty) in body.params.iter().zip(params) {
                        self.tys.insert(param.id, ty);
                    }

                    self.check_expr(&body.value, ret)
                } else {
                    unreachable!();
                }
            }
            _ => unreachable!(),
        }
    }

    crate fn check_pat(&mut self, pat: &ir::Pat, ty: Ty) -> Result<()> {
        let ty = match (&pat.kind, &*ty) {
            (_, Type::Unknown(_)) => {
                let infer = self.infer_pat(pat)?;
                let infer = self.instantiate(infer);
                let _ = self.unify_types(infer, ty.clone())?;

                ty
            }
            (ir::PatKind::Error, _) => {
                self.unify_types(ty.clone(), Ty::error(pat.span, self.file))?;
                ty
            }
            (_, _) => {
                let infer = self.infer_pat(pat)?;
                let _ = self.subsumes(infer, ty.clone())?;

                ty
            }
        };

        self.tys.insert(pat.id, ty);

        Ok(())
    }

    crate fn check_expr(&mut self, expr: &ir::Expr, ty: Ty) -> Result<()> {
        let ty = match (&expr.kind, &*ty) {
            (_, Type::ForAll(vars, t1, _)) => {
                let scope = self.new_skolem_scope();
                let skolems = (0..vars.len())
                    .map(|_| self.new_skolem_constant())
                    .collect();
                let sk = self.skolemize(t1.span(), t1.file(), vars, skolems, t1.clone(), scope);
                let _ = self.check_expr(expr, sk.clone())?;

                Ty::forall(ty.span(), ty.file(), vars.clone(), t1.clone(), scope)
            }
            (_, Type::Unknown(_)) => {
                let infer = self.infer_expr(expr)?;
                let infer = self.instantiate(infer);
                let _ = self.unify_types(infer.clone(), ty)?;

                infer
            }
            (ir::ExprKind::Error, _) => {
                self.unify_types(ty.clone(), Ty::error(expr.span, self.file))?;
                ty
            }
            (ir::ExprKind::Ident { res, .. }, _) => match res {
                ir::Res::Error => {
                    self.unify_types(ty.clone(), Ty::error(expr.span, self.file))?;
                    ty
                }
                ir::Res::Def(_, id) => {
                    let lhs = self.db.typecheck(*id).ty.clone();
                    let lhs = self.introduce_skolem_scope(lhs);
                    let ty = self.introduce_skolem_scope(ty);
                    let elaborate = self.subsumes(lhs, ty.clone())?;
                    let _ = elaborate(expr);

                    ty
                }
                ir::Res::Local(id) => {
                    let lhs = self.tys[id].clone();
                    let lhs = self.introduce_skolem_scope(lhs);
                    let ty = self.introduce_skolem_scope(ty);
                    let elaborate = self.subsumes(lhs, ty.clone())?;
                    let _ = elaborate(expr);

                    ty
                }
            },
            (ir::ExprKind::Tuple { exprs }, Type::Tuple(tys)) => {
                assert_eq!(exprs.len(), tys.len());

                for (expr, ty) in exprs.iter().zip(tys) {
                    self.check_expr(expr, ty)?;
                }

                ty
            }
            (ir::ExprKind::App { base, args }, _) => {
                let base_ty = self.infer_expr(base)?;
                let ret = self.check_func_app(base_ty, args)?;
                let elaborate = self.subsumes(ret, ty.clone())?;
                let _ = elaborate(expr);

                ty
            }
            (ir::ExprKind::Do { block }, _) => {
                for (i, stmt) in block.stmts.iter().enumerate() {
                    if i == block.stmts.len() - 1 {
                        match &stmt.kind {
                            ir::StmtKind::Discard { expr } => {
                                self.check_expr(expr, ty.clone())?;
                            }
                            ir::StmtKind::Bind { .. } => {
                                let unit = Ty::tuple(expr.span, self.file, List::empty());

                                self.unify_types(ty.clone(), unit)?;
                            }
                        }
                    } else {
                        self.infer_stmt(stmt)?;
                    }
                }

                ty
            }
            (ir::ExprKind::If { cond, then, else_ }, _) => {
                let bool_ty = self.db.lang_items().bool();
                let bool_ty = Ty::ctor(cond.span, self.file, bool_ty.owner);

                self.check_expr(cond, bool_ty)?;
                self.check_expr(then, ty.clone())?;
                self.check_expr(else_, ty.clone())?;

                ty
            }
            (ir::ExprKind::Case { pred, arms }, _) => {
                let tys = self.instantiate_for_binders(pred, arms)?;
                let _ = self.check_binders(tys, ty.clone(), arms)?;

                ty
            }
            (
                ir::ExprKind::Typed {
                    expr: expr2,
                    ty: ty1,
                },
                _,
            ) => {
                let ty1 = self.hir_ty(ty1);
                let (elab_ty1, kind1) = self.kind_of(ty1)?;
                let (elab_ty2, kind2) = self.kind_of(ty.clone())?;
                let _ = self.unify_kinds(kind1.clone(), kind2)?;
                let _ = self.check_type_kind(kind1)?;
                let ty1 = self.introduce_skolem_scope(elab_ty1);
                let ty2 = self.introduce_skolem_scope(elab_ty2);
                let elaborate = self.subsumes(ty1.clone(), ty2.clone())?;
                let _ = self.check_expr(expr2, ty1)?;
                let _ = elaborate(expr);

                ty2
            }
            (_, _) => {
                let infer = self.infer_expr(expr)?;
                let elaborate = self.subsumes(infer, ty.clone())?;

                elaborate(expr);
                ty
            }
        };

        self.tys.insert(expr.id, ty);

        Ok(())
    }

    crate fn check_func_app(&mut self, f_ty: Ty, args: &[ir::Expr]) -> Result<Ty> {
        let f_ty = self.subst_type(f_ty);

        match &*f_ty {
            Type::App(f, targs) if targs.len() == 2 => {
                let fn_ty = self.func_ty(f.span(), f.file());
                let _ = self.unify_types(f.clone(), fn_ty)?;

                if let Type::Tuple(params) = &*targs[0] {
                    assert_eq!(params.len(), args.len());

                    for (param, arg) in params.into_iter().zip(args) {
                        self.check_expr(arg, param)?;
                    }
                } else {
                    let params = args
                        .iter()
                        .map(|a| self.infer_expr(a))
                        .collect::<Result<List<_>>>()?;

                    let params = Ty::tuple(targs[0].span(), targs[0].file(), params);

                    self.unify_types(targs[0].clone(), params)?;
                }

                Ok(targs[1].clone())
            }
            Type::ForAll(vars, ret, _) => {
                let repl = vars
                    .into_iter()
                    .map(|(v, k)| match k {
                        Some(k) => (v, self.fresh_type_with_kind(f_ty.span(), f_ty.file(), k)),
                        None => (v, self.fresh_type(f_ty.span(), f_ty.file())),
                    })
                    .collect();

                let repl = ret.clone().replace_vars(repl);

                self.check_func_app(repl, args)
            }
            Type::Ctnt(..) => unimplemented!(),
            _ => {
                let params = args
                    .iter()
                    .map(|a| {
                        let ty = self.infer_expr(a)?;

                        Ok(self.instantiate(ty))
                    })
                    .collect::<Result<List<_>>>()?;

                let params = Ty::tuple(f_ty.span(), f_ty.file(), params);
                let ty_kind = self.ty_kind(f_ty.span(), f_ty.file());
                let ret = self.fresh_type_with_kind(f_ty.span(), f_ty.file(), ty_kind);
                let func_ty = self.func_ty(f_ty.span(), f_ty.file());
                let inferred = Ty::app(
                    f_ty.span(),
                    f_ty.file(),
                    func_ty,
                    List::from([params, ret.clone()]),
                );

                self.unify_types(f_ty, inferred)?;

                Ok(ret)
            }
        }
    }

    crate fn check_binders(&mut self, tys: List<Ty>, ret: Ty, arms: &[ir::CaseArm]) -> Result<()> {
        if arms.is_empty() {
            Ok(())
        } else {
            arms.iter()
                .map(|arm| {
                    for (pat, ty) in arm.pats.iter().zip(&tys) {
                        self.check_pat(pat, ty)?;
                    }

                    self.check_guarded(&arm.val, ret.clone())
                })
                .collect()
        }
    }

    crate fn check_guarded(&mut self, guarded: &ir::Guarded, ty: Ty) -> Result<()> {
        match guarded {
            ir::Guarded::Unconditional(expr) => self.check_expr(expr, ty),
            ir::Guarded::Guarded(exprs) => {
                for expr in exprs {
                    let bool_ty = self.db.lang_items().bool();
                    let bool_ty = Ty::ctor(expr.guard.span, self.file, bool_ty.owner);

                    self.check_expr(&expr.guard, bool_ty)?;
                    self.check_expr(&expr.val, ty.clone())?;
                }

                Ok(())
            }
        }
    }
}
