use crate::constraint::Constraint;
use crate::tcx::Tcx;
use crate::ty::*;
use diagnostics::Span;

impl<'tcx> Tcx<'tcx> {
    pub fn infer_block(&self, block: &hir::Block) -> Ty<'tcx> {
        let mut ty = self.builtin.unit;

        for (i, stmt) in block.stmts.iter().enumerate() {
            match &stmt.kind {
                hir::StmtKind::Item(id) => {
                    self.type_of(id);
                }
                hir::StmtKind::Semi(id) => {
                    self.type_of(id);
                }
                hir::StmtKind::Expr(id) => {
                    if i == block.stmts.len() - 1 {
                        ty = self.type_of(id);
                    } else {
                        let expr_ty = self.type_of(id);
                        let expr_span = self.span_of(id);

                        self.constrain(Constraint::Equal(
                            expr_ty,
                            expr_span,
                            self.builtin.unit,
                            expr_span,
                        ));
                    }
                }
            }
        }

        ty
    }

    pub fn infer_expr(&self, id: &hir::Id) -> Ty<'tcx> {
        let expr = &self.package.exprs[id];

        match &expr.kind {
            hir::ExprKind::Err => self.builtin.error,
            hir::ExprKind::Path { res } => match res {
                hir::Res::Item(id) => self.type_of(id),
                hir::Res::Local(id) => self.type_of(id),
                _ => unreachable!(),
            },
            hir::ExprKind::Int { .. } => self.new_int(),
            hir::ExprKind::Float { .. } => self.new_float(),
            hir::ExprKind::Char { .. } => self.builtin.u32,
            hir::ExprKind::String { .. } => self.builtin.str,
            hir::ExprKind::Type { ty } => {
                self.type_of(ty);
                self.builtin.typeid
            }
            hir::ExprKind::Call { func, args } => {
                let ret_ty = self.new_var();
                let func_ty = self.type_of(func);
                let func_span = self.span_of(func);
                let args = args
                    .iter()
                    .map(|a| Param {
                        name: a.name.unwrap_or_else(|| Ident {
                            span: a.span,
                            symbol: hir::Symbol::dummy(),
                        }),
                        ty: self.type_of(&a.value),
                    })
                    .collect::<Vec<_>>();

                self.constrain(Constraint::Call(
                    func_ty, func_span, args, ret_ty, expr.span,
                ));

                ret_ty
            }
            hir::ExprKind::Field { obj, field } => {
                let ty = self.new_var();
                let obj_ty = self.type_of(obj);
                let obj_span = self.span_of(obj);

                self.constrain(Constraint::Field(obj_ty, obj_span, *field, ty, expr.span));
                ty
            }
            hir::ExprKind::Deref { expr: inner } => {
                let ty = self.new_var();
                let ref_ty = self.intern_ty(Type::Ref(false, ty));
                let inner_ty = self.type_of(inner);
                let inner_span = self.span_of(inner);

                self.constrain(Constraint::Equal(inner_ty, inner_span, ref_ty, expr.span));
                ty
            }
            hir::ExprKind::Assign { lhs, rhs } => {
                let lhs_ty = self.type_of(lhs);
                let lhs_span = self.span_of(lhs);
                let rhs_ty = self.type_of(rhs);
                let rhs_span = self.span_of(rhs);

                self.constrain(Constraint::Equal(lhs_ty, lhs_span, rhs_ty, rhs_span));
                lhs_ty
            }
            hir::ExprKind::BinOp { op, lhs, rhs } => {
                let lhs_ty = self.type_of(lhs);
                let lhs_span = self.span_of(lhs);
                let rhs_ty = self.type_of(rhs);
                let rhs_span = self.span_of(rhs);

                match op {
                    hir::BinOp::Add | hir::BinOp::Sub => {
                        self.constrain(Constraint::PtrArith(lhs_ty, lhs_span, rhs_ty, rhs_span));
                        lhs_ty
                    }
                    hir::BinOp::Mul | hir::BinOp::Div => {
                        self.constrain(Constraint::Equal(rhs_ty, rhs_span, lhs_ty, lhs_span));
                        self.constrain(Constraint::IsNum(lhs_ty, lhs_span));
                        self.constrain(Constraint::IsNum(rhs_ty, rhs_span));
                        lhs_ty
                    }
                    hir::BinOp::Rem
                    | hir::BinOp::BitAnd
                    | hir::BinOp::BitOr
                    | hir::BinOp::BitXOr
                    | hir::BinOp::Shl
                    | hir::BinOp::Shr => {
                        self.constrain(Constraint::Equal(rhs_ty, rhs_span, lhs_ty, lhs_span));
                        self.constrain(Constraint::IsInt(lhs_ty, lhs_span));
                        self.constrain(Constraint::IsInt(rhs_ty, rhs_span));
                        lhs_ty
                    }
                    hir::BinOp::Lt | hir::BinOp::Le | hir::BinOp::Gt | hir::BinOp::Ge => {
                        self.constrain(Constraint::PtrArith(rhs_ty, rhs_span, lhs_ty, lhs_span));
                        self.builtin.bool
                    }
                    hir::BinOp::Eq | hir::BinOp::Ne => {
                        self.constrain(Constraint::Equal(rhs_ty, rhs_span, lhs_ty, lhs_span));
                        self.builtin.bool
                    }
                    hir::BinOp::And | hir::BinOp::Or => {
                        self.constrain(Constraint::Equal(
                            lhs_ty,
                            lhs_span,
                            self.builtin.bool,
                            lhs_span,
                        ));

                        self.constrain(Constraint::Equal(
                            rhs_ty,
                            rhs_span,
                            self.builtin.bool,
                            rhs_span,
                        ));

                        self.builtin.bool
                    }
                }
            }
            hir::ExprKind::While { label, cond, body } => {
                let cond_ty = self.type_of(cond);
                let cond_span = self.span_of(cond);
                let _ = self.infer_block(body);

                self.constrain(Constraint::Equal(
                    cond_ty,
                    cond_span,
                    self.builtin.bool,
                    cond_span,
                ));

                let mut breaks = Vec::new();

                self.find_breaks(body, label.as_ref(), &mut breaks);

                if breaks.is_empty() {
                    self.builtin.unit
                } else {
                    let ret = self.new_var();

                    for (ty, span) in breaks {
                        self.constrain(Constraint::Equal(ty, span, ret, expr.span));
                    }

                    ret
                }
            }
            _ => unimplemented!("{}", expr),
        }
    }

    fn find_breaks(
        &self,
        block: &hir::Block,
        lbl: Option<&hir::Id>,
        breaks: &mut Vec<(Ty<'tcx>, Span)>,
    ) {
        for stmt in &block.stmts {
            match &stmt.kind {
                hir::StmtKind::Semi(expr) | hir::StmtKind::Expr(expr) => {
                    self.find_breaks_expr(expr, lbl, breaks);
                }
                _ => {}
            }
        }
    }

    fn find_breaks_expr(
        &self,
        id: &hir::Id,
        lbl: Option<&hir::Id>,
        breaks: &mut Vec<(Ty<'tcx>, Span)>,
    ) {
        let expr = &self.package.exprs[id];

        match &expr.kind {
            hir::ExprKind::Break { label, expr: e } if label.as_ref() == lbl => {
                let (ty, span) = if let Some(e) = e {
                    (self.type_of(e), self.span_of(e))
                } else {
                    (self.builtin.unit, expr.span)
                };

                breaks.push((ty, span));
            }
            _ => {}
        }
    }
}
