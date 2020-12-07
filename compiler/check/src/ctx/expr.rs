use super::*;

impl<'db> Ctx<'db> {
    pub(crate) fn infer_expr(&mut self, expr: &ir::Expr) -> Ty {
        let ty = match &expr.kind {
            ir::ExprKind::Error => Ty::error(),
            ir::ExprKind::Hole { .. } => Ty::infer(self.db.new_infer_var()),
            ir::ExprKind::Int { .. } => Ty::infer(self.db.new_infer_var()),
            ir::ExprKind::Ident { res } => match res {
                ir::Res::Error => Ty::error(),
                ir::Res::Def(_, def) => self.db.typecheck(*def).ty.monomorphize(self.db),
                ir::Res::Local(id) => self.tys[id].0.clone(),
            },
            ir::ExprKind::App { base, args } => {
                let base_ty = self.infer_expr(base);
                let arg_tys = args.iter().map(|a| self.infer_expr(a)).collect();
                let ret_ty = Ty::infer(self.db.new_infer_var());
                let func_ty = Ty::func(arg_tys, ret_ty.clone());

                self.constrain()
                    .equal(base_ty, base.span, func_ty, expr.span);

                ret_ty
            }
            ir::ExprKind::Tuple { exprs } => {
                let tys = exprs.iter().map(|e| self.infer_expr(e)).collect();

                Ty::tuple(tys)
            }
            ir::ExprKind::Field { base, field } => {
                let base_ty = self.infer_expr(base);
                let field_ty = Ty::infer(self.db.new_infer_var());
                let rest_ty = Ty::infer(self.db.new_infer_var());
                let exp_ty = Ty::record(
                    vec![Field {
                        name: field.symbol,
                        span: field.span,
                        ty: field_ty.clone(),
                    }]
                    .into(),
                    Some(rest_ty),
                );

                self.constrain()
                    .equal(exp_ty, expr.span, base_ty, base.span);

                field_ty
            }
            ir::ExprKind::Case { pred, arms } => {
                let ret_ty = Ty::infer(self.db.new_infer_var());
                let pred_tys = pred.iter().map(|e| (self.infer_expr(e), e.span)).collect();

                for arm in arms {
                    self.infer_arm(arm, &pred_tys, ret_ty.clone(), expr.span);
                }

                ret_ty
            }
            ir::ExprKind::Do { block } => {
                for stmt in &block.stmts {
                    self.infer_stmt(stmt);
                }

                match block.stmts.last() {
                    Some(ir::Stmt {
                        kind: ir::StmtKind::Discard { expr },
                        ..
                    }) => self.tys[&expr.id].0.clone(),
                    _ => Ty::tuple(List::new()),
                }
            }
            ir::ExprKind::Typed { expr, ty } => {
                let expr_ty = self.infer_expr(expr);
                let ty_ty = self.hir_ty(ty);

                self.constrain()
                    .equal(expr_ty, expr.span, ty_ty.clone(), ty.span);

                ty_ty
            }
            _ => unimplemented!("{:?}", expr),
        };

        self.tys.insert(expr.id, (ty.clone(), expr.span));
        ty
    }

    fn infer_arm(
        &mut self,
        arm: &ir::CaseArm,
        pred_tys: &List<(Ty, ir::Span)>,
        ret_ty: Ty,
        ret_span: ir::Span,
    ) {
        for (pat, (pred_ty, pred_span)) in arm.pats.iter().zip(pred_tys) {
            let pat_ty = self.infer_pat(pat);

            self.constrain().equal(pat_ty, pat.span, pred_ty, pred_span);
        }

        let val_ty = self.infer_guarded(&arm.val);

        self.constrain().equal(val_ty, arm.span, ret_ty, ret_span);
    }

    fn infer_guarded(&mut self, guarded: &ir::Guarded) -> Ty {
        match guarded {
            ir::Guarded::Unconditional(expr) => self.infer_expr(expr),
            ir::Guarded::Guarded(_) => unimplemented!(),
        }
    }

    fn infer_stmt(&mut self, stmt: &ir::Stmt) {
        match &stmt.kind {
            ir::StmtKind::Bind { binding } => {
                let ty = self.hir_ty(&binding.ty);
                let pat_ty = self.infer_pat(&binding.pat);
                let val_ty = self.infer_expr(&binding.val);

                self.constrain()
                    .equal(pat_ty.clone(), binding.pat.span, ty, stmt.span);

                self.constrain()
                    .equal(val_ty, binding.val.span, pat_ty, binding.pat.span);
            }
            ir::StmtKind::Discard { expr } => {
                self.infer_expr(expr);
            }
        }
    }
}
