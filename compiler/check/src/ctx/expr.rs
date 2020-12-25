use super::*;
use crate::constraint::TraitCtnt;

impl<'db> Ctx<'db> {
    pub(crate) fn infer_expr(&mut self, expr: &ir::Expr) -> Ty {
        let ty = match &expr.kind {
            ir::ExprKind::Error => Ty::error(),
            ir::ExprKind::Hole { .. } => Ty::infer(self.db.new_infer_var()),
            ir::ExprKind::Int { .. } => {
                let var = self.db.new_infer_var();
                let ty = Ty::infer(var);

                Ty::ctnt(
                    TraitCtnt {
                        trait_: self.db.lang_items().integer_trait(),
                        tys: vec![ty.clone()].into(),
                    },
                    ty,
                )
            }
            ir::ExprKind::Ident { res } => match res {
                ir::Res::Error => Ty::error(),
                ir::Res::Def(_, def) => {
                    if let Some((ty, _)) = self.tys.get(&ir::HirId {
                        owner: *def,
                        local_id: ir::LocalId(0),
                    }) {
                        ty.clone()
                    } else {
                        self.db.typecheck(*def).ty.monomorphize(self.db)
                    }
                }
                ir::Res::Local(id) => self.tys[id].0.clone(),
            },
            ir::ExprKind::App { base, args } => {
                let base_ty = self.infer_expr(base);

                match &*base_ty {
                    Type::Func(param_tys, ret_ty) => {
                        if param_tys.len() != args.len() {
                            todo!("error on wrong arg count");
                        } else {
                            for (param_ty, arg) in param_tys.into_iter().zip(args) {
                                let arg_ty = self.infer_expr(arg);

                                self.constrain().equal(arg_ty, arg.span, param_ty, arg.span);
                            }

                            ret_ty.clone()
                        }
                    }
                    _ => {
                        let ret_ty = Ty::infer(self.db.new_infer_var());
                        let param_tys = args.iter().map(|a| self.infer_expr(a)).collect();
                        let func_ty = Ty::func(param_tys, ret_ty.clone());

                        self.constrain()
                            .equal(base_ty, base.span, func_ty, base.span);

                        ret_ty
                    }
                }
            }
            ir::ExprKind::Tuple { exprs } => {
                let tys = exprs.iter().map(|e| self.infer_expr(e)).collect();

                Ty::tuple(tys)
            }
            ir::ExprKind::Record { fields } => {
                let fields = fields
                    .iter()
                    .map(|f| Field {
                        span: f.span,
                        name: f.name.symbol,
                        ty: self.infer_expr(&f.val),
                    })
                    .collect();

                Ty::record(fields, None)
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
            ir::ExprKind::Infix { op, lhs, rhs } => {
                let lhs_ty = self.infer_expr(lhs);
                let rhs_ty = self.infer_expr(rhs);

                self.constrain()
                    .equal(rhs_ty, rhs.span, lhs_ty.clone(), lhs.span);

                match op {
                    ir::InfixOp::Eq
                    | ir::InfixOp::Ne
                    | ir::InfixOp::Lt
                    | ir::InfixOp::Le
                    | ir::InfixOp::Gt
                    | ir::InfixOp::Ge => {
                        let bool_ty = self.db.lang_items().bool();

                        Ty::data(bool_ty.owner)
                    }
                    _ => lhs_ty,
                }
            }
            ir::ExprKind::If { cond, then, else_ } => {
                let res = Ty::infer(self.db.new_infer_var());
                let bool_ty = self.db.lang_items().bool();
                let bool_ty = Ty::data(bool_ty.owner);
                let cond_ty = self.infer_expr(cond);
                let then_ty = self.infer_expr(then);
                let else_ty = self.infer_expr(else_);

                self.constrain()
                    .equal(cond_ty, cond.span, bool_ty, cond.span);
                self.constrain()
                    .equal(then_ty, then.span, res.clone(), expr.span);
                self.constrain()
                    .equal(else_ty, else_.span, res.clone(), expr.span);

                res
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
            ir::Guarded::Guarded(guards) => {
                let res = Ty::infer(self.db.new_infer_var());
                let bool_ty = self.db.lang_items().bool();
                let bool_ty = Ty::data(bool_ty.owner);
                let span = guards[0].span.merge(guards[guards.len() - 1].span);

                for guard in guards {
                    let guard_ty = self.infer_expr(&guard.guard);
                    let val_ty = self.infer_expr(&guard.val);

                    self.constrain().equal(
                        guard_ty,
                        guard.guard.span,
                        bool_ty.clone(),
                        guard.guard.span,
                    );

                    self.constrain()
                        .equal(val_ty, guard.val.span, res.clone(), span);
                }

                res
            }
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
