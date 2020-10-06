use crate::constraint::Constraint;
use crate::tcx::Tcx;
use crate::ty::*;
use diagnostics::Span;

impl<'tcx> Tcx<'tcx> {
    pub fn infer_block(&self, block: &hir::Block) -> Ty<'tcx> {
        let mut ty = self.builtin.unit;

        for stmt in &block.stmts {
            match &stmt.kind {
                hir::StmtKind::Item(id) => {
                    self.type_of(id);
                }
                hir::StmtKind::Semi(id) => {
                    self.type_of(id);
                }
                hir::StmtKind::Expr(id) => {
                    ty = self.type_of(id);
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
                hir::Res::Item(id) => self.type_of(id).mono(self, Vec::new()),
                hir::Res::Local(id) => self.type_of(id),
                hir::Res::PrimVal(prim) => match prim {
                    hir::PrimVal::True => self.builtin.bool,
                    hir::PrimVal::False => self.builtin.bool,
                    hir::PrimVal::Undefined => self.new_var(),
                },
                _ => unreachable!(),
            },
            hir::ExprKind::Apply { expr, args } => {
                if let hir::ExprKind::Path {
                    res: hir::Res::Item(id),
                } = &self.package.exprs[expr].kind
                {
                    let ty = self.type_of(id);

                    if let Type::Forall(_, _) = ty {
                        let args = args.iter().map(|a| self.type_of(a)).collect();

                        ty.mono(self, args)
                    } else {
                        unreachable!();
                    }
                } else {
                    unreachable!();
                }
            }
            hir::ExprKind::Int { .. } => self.new_int(),
            hir::ExprKind::Float { .. } => self.new_float(),
            hir::ExprKind::Char { .. } => self.builtin.u32,
            hir::ExprKind::String { .. } => self.builtin.str,
            hir::ExprKind::Type { ty } => {
                self.type_of(ty);
                self.builtin.typeid
            }
            hir::ExprKind::Array { exprs } => {
                let ty = self.new_var();

                for exp in exprs {
                    let exp_ty = self.type_of(exp);
                    let exp_span = self.span_of(exp);

                    self.constrain(Constraint::Equal(exp_ty, exp_span, ty, expr.span));
                }

                self.intern_ty(Type::Array(ty, exprs.len()))
            }
            hir::ExprKind::Tuple { exprs } => {
                let tys = exprs.iter().map(|e| self.type_of(e)).collect::<Vec<_>>();
                let tys = self.intern.intern_ty_list(&tys);

                self.intern_ty(Type::Tuple(tys))
            }
            hir::ExprKind::Range { lo, hi } => {
                let lo_ty = self.type_of(lo);
                let hi_ty = self.type_of(hi);

                self.constrain(Constraint::Equal(
                    hi_ty,
                    self.span_of(hi),
                    lo_ty,
                    self.span_of(lo),
                ));

                self.type_of(&self.lang_items.range().unwrap())
                    .mono(self, vec![lo_ty])
            }
            hir::ExprKind::Block { block } => self.infer_block(block),
            hir::ExprKind::Call { func, args } => {
                let ret_ty = self.new_var();
                let func_ty = self.type_of(func);
                let func_span = self.span_of(func);
                let args = args
                    .iter()
                    .map(|a| Param {
                        span: a.span,
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
            hir::ExprKind::Index { list, index } => {
                let ty = self.new_var();
                let list_ty = self.type_of(list);
                let list_span = self.span_of(list);
                let index_ty = self.type_of(index);
                let index_span = self.span_of(index);

                self.constrain(Constraint::Equal(
                    index_ty,
                    index_span,
                    self.builtin.usize,
                    index_span,
                ));

                self.constrain(Constraint::Index(list_ty, list_span, ty, expr.span));

                ty
            }
            hir::ExprKind::Slice { list, low, high } => {
                let ty = self.new_var();
                let list_ty = self.type_of(list);
                let list_span = self.span_of(list);

                if let Some(low) = low {
                    let low_ty = self.type_of(low);
                    let low_span = self.span_of(low);

                    self.constrain(Constraint::Equal(
                        low_ty,
                        low_span,
                        self.builtin.usize,
                        low_span,
                    ));
                }

                if let Some(high) = high {
                    let high_ty = self.type_of(high);
                    let high_span = self.span_of(high);

                    self.constrain(Constraint::Equal(
                        high_ty,
                        high_span,
                        self.builtin.usize,
                        high_span,
                    ));
                }

                self.constrain(Constraint::Index(list_ty, list_span, ty, expr.span));

                self.intern_ty(Type::Slice(ty))
            }
            hir::ExprKind::Ref { expr: inner } => {
                let inner_ty = self.type_of(inner);

                self.intern_ty(Type::Ptr(PtrKind::Single, inner_ty))
            }
            hir::ExprKind::Deref { expr: inner } => {
                let ty = self.new_var();
                let ref_ty = self.intern_ty(Type::Ptr(PtrKind::Single, ty));
                let inner_ty = self.type_of(inner);
                let inner_span = self.span_of(inner);

                self.constrain(Constraint::Equal(inner_ty, inner_span, ref_ty, expr.span));
                ty
            }
            hir::ExprKind::TypeOf { expr: inner } => {
                self.type_of(inner);
                self.builtin.typeid
            }
            hir::ExprKind::Cast { expr, ty } => {
                self.type_of(expr);
                self.type_of(ty)
            }
            hir::ExprKind::Box { expr } => {
                let ty = self.type_of(expr);

                self.intern_ty(Type::Ptr(PtrKind::Single, ty))
            }
            hir::ExprKind::Unbox { expr } => {
                let expr_ty = self.type_of(expr);
                let expr_span = self.span_of(expr);
                let ty = self.new_var();
                let ptr_ty = self.intern_ty(Type::Ptr(PtrKind::Single, ty));
                let span = self.span_of(id);

                self.constrain(Constraint::Equal(ptr_ty, span, expr_ty, expr_span));

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
            hir::ExprKind::UnOp { op, rhs } => match op {
                hir::UnOp::Not => self.type_of(rhs),
                hir::UnOp::Neg => {
                    let rhs_ty = self.type_of(rhs);
                    let rhs_span = self.span_of(rhs);

                    self.constrain(Constraint::IsNum(rhs_ty, rhs_span));
                    rhs_ty
                }
            },
            hir::ExprKind::IfElse { cond, then, else_ } => {
                let cond_ty = self.type_of(cond);
                let cond_span = self.span_of(cond);
                let then_ty = self.infer_block(then);

                self.constrain(Constraint::Equal(
                    cond_ty,
                    cond_span,
                    self.builtin.bool,
                    cond_span,
                ));

                if let Some(else_) = else_ {
                    let ty = self.new_var();
                    let else_ty = self.infer_block(else_);

                    self.constrain(Constraint::Equal(then_ty, then.span, ty, expr.span));
                    self.constrain(Constraint::Equal(else_ty, else_.span, ty, expr.span));

                    ty
                } else {
                    self.builtin.unit
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
            hir::ExprKind::Defer { expr } => {
                self.type_of(expr);
                self.builtin.unit
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
                hir::StmtKind::Expr(expr) => {
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
