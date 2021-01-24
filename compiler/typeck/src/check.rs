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
            | Type::Ctnt(ctnt, ty) => {
                self.ctnt_ctx.push(ctnt.clone());
                self.check_body_(span, body, ty.clone())?;
                self.ctnt_ctx.pop().unwrap();
                Ok(())
            },
            | Type::ForAll(var, kind, ret, _) => {
                let scope = self.new_skolem_scope();
                let skolem = self.new_skolem_constant();
                let sk = self.skolemize(ty.span(), ty.file(), *var, kind.clone(), skolem, ret.clone(), scope);

                self.check_body_(span, body, sk)
            },
            | _ => {
                let (args, ret) = self.args(ty.clone());

                if args.len() != body.params.len() {
                    return Err(TypeError::IncorrectArity(self.file, span, ty));
                }

                for (param, ty) in body.params.iter().zip(args) {
                    self.tys.insert(param.id, ty);
                }

                self.check_expr(&body.value, ret)
            },
        }
    }

    crate fn check_pat(&mut self, pat: &ir::Pat, ty: Ty) -> Result<()> {
        let ty = match (&pat.kind, &*ty) {
            | (_, Type::Unknown(_)) => {
                let infer = self.infer_pat(pat)?;
                let infer = self.instantiate(pat.id, infer);
                let _ = self.unify_types(infer, ty.clone())?;

                ty
            },
            | (ir::PatKind::Error, _) => {
                self.unify_types(ty.clone(), Ty::error(pat.span, self.file))?;
                ty
            },
            | (_, _) => {
                let infer = self.infer_pat(pat)?;
                let _ = self.subsumes(infer, ty.clone())?;

                ty
            },
        };

        self.tys.insert(pat.id, ty);

        Ok(())
    }

    crate fn check_expr(&mut self, expr: &ir::Expr, ty: Ty) -> Result<()> {
        let ty = match (&expr.kind, &*ty) {
            | (_, Type::ForAll(var, k1, t1, _)) => {
                let scope = self.new_skolem_scope();
                let skolem = self.new_skolem_constant();
                let sk = self.skolemize(t1.span(), t1.file(), *var, k1.clone(), skolem, t1.clone(), scope);
                let _ = self.check_expr(expr, sk.clone())?;

                Ty::forall(ty.span(), ty.file(), *var, k1.clone(), t1.clone(), scope)
            },
            | (_, Type::Ctnt(ctnt, t1)) => {
                self.ctnt_ctx.push(ctnt.clone());
                self.check_expr(expr, t1.clone())?;
                self.ctnt_ctx.pop().unwrap();
                ty
            },
            | (_, Type::Unknown(_)) => {
                let infer = self.infer_expr(expr)?;
                let infer = self.instantiate(expr.id, infer);
                let _ = self.unify_types(infer.clone(), ty)?;

                infer
            },
            | (ir::ExprKind::Error, _) => {
                self.unify_types(ty.clone(), Ty::error(expr.span, self.file))?;
                ty
            },
            | (ir::ExprKind::Ident { res, .. }, _) => match res {
                | ir::Res::Error => {
                    self.unify_types(ty.clone(), Ty::error(expr.span, self.file))?;
                    ty
                },
                | ir::Res::Def(_, id) => {
                    let lhs = self.typeck_def(*id);
                    let lhs = self.introduce_skolem_scope(lhs);
                    let ty = self.introduce_skolem_scope(ty);
                    let elaborate = self.subsumes(lhs, ty.clone())?;
                    let _ = elaborate(self, expr);

                    ty
                },
                | ir::Res::Local(id) => {
                    let lhs = self.tys[id].clone();
                    let lhs = self.introduce_skolem_scope(lhs);
                    let ty = self.introduce_skolem_scope(ty);
                    let elaborate = self.subsumes(lhs, ty.clone())?;
                    let _ = elaborate(self, expr);

                    ty
                },
            },
            | (ir::ExprKind::Tuple { exprs }, Type::Tuple(tys)) => {
                assert_eq!(exprs.len(), tys.len());

                for (expr, ty) in exprs.iter().zip(tys) {
                    self.check_expr(expr, ty)?;
                }

                ty
            },
            | (ir::ExprKind::App { base, arg }, _) => {
                let base_ty = self.infer_expr(base)?;
                let ret = self.check_func_app(base.id, base_ty, arg)?;
                let elaborate = self.subsumes(ret, ty.clone())?;
                let _ = elaborate(self, expr);

                ty
            },
            | (ir::ExprKind::Index { base, index }, _) => {
                let figure_kind = self.figure_kind(expr.span, self.file);
                let len = self.fresh_type_with_kind(expr.span, self.file, figure_kind);
                let arr_ty = self.array_ty(base.span, self.file);
                let arr_ty = Ty::app(base.span, self.file, Ty::app(base.span, self.file, arr_ty, ty.clone()), len);
                let uint_ty = self.db.lang_items().uint();
                let uint_ty = self.typeck_def(uint_ty.owner);

                self.check_expr(index, uint_ty)?;
                self.check_expr(base, arr_ty)?;

                ty
            },
            | (ir::ExprKind::Do { block }, _) => {
                for (i, stmt) in block.stmts.iter().enumerate() {
                    if i == block.stmts.len() - 1 {
                        match &stmt.kind {
                            | ir::StmtKind::Discard { expr } => {
                                self.check_expr(expr, ty.clone())?;
                            },
                            | ir::StmtKind::Bind { .. } => {
                                let unit = Ty::tuple(expr.span, self.file, List::empty());

                                self.infer_stmt(stmt)?;
                                self.unify_types(ty.clone(), unit)?;
                            },
                        }
                    } else {
                        self.infer_stmt(stmt)?;
                    }
                }

                ty
            },
            | (ir::ExprKind::If { cond, then, else_ }, _) => {
                let bool_ty = self.db.lang_items().bool();
                let bool_ty = Ty::ctor(cond.span, self.file, bool_ty.owner);

                self.check_expr(cond, bool_ty)?;
                self.check_expr(then, ty.clone())?;
                self.check_expr(else_, ty.clone())?;

                ty
            },
            | (ir::ExprKind::Case { pred, arms }, _) => {
                let tys = self.instantiate_for_binders(pred, arms)?;
                let _ = self.check_binders(tys, ty.clone(), arms)?;

                ty
            },
            | (ir::ExprKind::Typed { expr: expr2, ty: ty1 }, _) => {
                let ty1 = self.hir_ty(ty1);
                let (elab_ty1, kind1) = self.kind_of(ty1)?;
                let (elab_ty2, kind2) = self.kind_of(ty.clone())?;
                let _ = self.unify_kinds(kind1.clone(), kind2)?;
                let _ = self.check_type_kind(kind1)?;
                let ty1 = self.introduce_skolem_scope(elab_ty1);
                let ty2 = self.introduce_skolem_scope(elab_ty2);
                let elaborate = self.subsumes(ty1.clone(), ty2.clone())?;
                let _ = self.check_expr(expr2, ty1)?;
                let _ = elaborate(self, expr);

                ty2
            },
            | (_, _) => {
                let infer = self.infer_expr(expr)?;
                let elaborate = self.subsumes(infer, ty.clone())?;

                elaborate(self, expr);
                ty
            },
        };

        self.tys.insert(expr.id, ty);

        Ok(())
    }

    #[track_caller]
    crate fn check_func_app(&mut self, f_id: ir::HirId, f_ty: Ty, arg: &ir::Expr) -> Result<Ty> {
        let f_ty = self.subst_type(f_ty);

        match &*f_ty {
            | Type::App(b, r) => match &**b {
                | Type::App(f, a) => {
                    let func_ty = self.func_ty(f.span(), f.file());
                    let _ = self.unify_types(f.clone(), func_ty)?;
                    let _ = self.check_expr(arg, a.clone())?;

                    Ok(r.clone())
                },
                | _ => {
                    let param = self.infer_expr(arg)?;
                    let param = self.instantiate(arg.id, param);
                    let ty_kind = self.ty_kind(f_ty.span(), f_ty.file());
                    let ret = self.fresh_type_with_kind(f_ty.span(), f_ty.file(), ty_kind);
                    let func_ty = self.func_ty(f_ty.span(), f_ty.file());
                    let inferred = Ty::app(f_ty.span(), f_ty.file(), Ty::app(f_ty.span(), f_ty.file(), func_ty, param), ret.clone());

                    self.unify_types(f_ty, inferred)?;

                    Ok(ret)
                },
            },
            | Type::ForAll(var, kind, ret, _) => {
                let mut repl = std::collections::HashMap::new();
                let u = match kind {
                    | Some(k) => self.fresh_type_with_kind(f_ty.span(), f_ty.file(), k.clone()),
                    | None => return Err(TypeError::Internal("unelaborated forall".into())),
                };

                repl.insert(*var, u);

                let repl = ret.clone().replace_vars(repl);

                self.check_func_app(f_id, repl ^ f_ty.loc(), arg)
            },
            | Type::Ctnt(ctnt, ret) => {
                self.ctnts.push((f_id, ctnt.clone() ^ f_ty.loc(), self.ctnt_ctx.clone()));

                self.check_func_app(f_id, ret.clone() ^ f_ty.loc(), arg)
            },
            | _ => {
                let param = self.infer_expr(arg)?;
                let param = self.instantiate(arg.id, param);
                let ty_kind = self.ty_kind(f_ty.span(), f_ty.file());
                let ret = self.fresh_type_with_kind(f_ty.span(), f_ty.file(), ty_kind);
                let func_ty = self.func_ty(f_ty.span(), f_ty.file());
                let inferred = Ty::app(f_ty.span(), f_ty.file(), Ty::app(f_ty.span(), f_ty.file(), func_ty, param), ret.clone());

                self.unify_types(f_ty, inferred)?;

                Ok(ret)
            },
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
            | ir::Guarded::Unconditional(expr) => self.check_expr(expr, ty),
            | ir::Guarded::Guarded(exprs) => {
                for expr in exprs {
                    let bool_ty = self.db.lang_items().bool();
                    let bool_ty = Ty::ctor(expr.guard.span, self.file, bool_ty.owner);

                    self.check_expr(&expr.guard, bool_ty)?;
                    self.check_expr(&expr.val, ty.clone())?;
                }

                Ok(())
            },
        }
    }
}
