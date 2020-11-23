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
                ir::Res::Local(id) => self.tys[id].clone(),
            },
            ir::ExprKind::App { base, args } => {
                let base_ty = self.infer_expr(base);
                let arg_tys = args.iter().map(|a| self.infer_expr(a)).collect();
                let ret_ty = Ty::infer(self.db.new_infer_var());
                let func_ty = Ty::func(arg_tys, ret_ty.clone());

                self.constrain(Constraint::Equal(base_ty, base.span, func_ty, expr.span));

                ret_ty
            }
            ir::ExprKind::Case { pred, arms } => {
                let ret_ty = Ty::infer(self.db.new_infer_var());
                let pred_tys = pred.iter().map(|e| (self.infer_expr(e), e.span)).collect();

                for arm in arms {
                    self.infer_arm(arm, &pred_tys, ret_ty.clone(), expr.span);
                }

                ret_ty
            }
            _ => unimplemented!(),
        };

        self.tys.insert(expr.id, ty.clone());
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

            self.constrain(Constraint::Equal(pat_ty, pat.span, pred_ty, pred_span));
        }

        let val_ty = self.infer_guarded(&arm.val);

        self.constrain(Constraint::Equal(val_ty, arm.span, ret_ty, ret_span));
    }

    fn infer_guarded(&mut self, guarded: &ir::Guarded) -> Ty {
        match guarded {
            ir::Guarded::Unconditional(expr) => self.infer_expr(expr),
            ir::Guarded::Guarded(_) => unimplemented!(),
        }
    }
}
