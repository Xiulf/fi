use hir_def::expr::{Expr, ExprId, Literal};
use hir_def::id::ValueDefId;

use crate::ctx::{BodyCtx, Expectation};
use crate::ty::{FuncType, Ty, TyKind};

impl BodyCtx<'_, '_> {
    pub fn infer_expr(&mut self, id: ExprId, expected: Expectation) -> Ty {
        let ty = self.infer_expr_inner(id, expected);

        if let Expectation::HasType(expected) = expected {
            self.unify_types(ty, expected, id.into());
        }

        ty
    }

    fn infer_expr_inner(&mut self, id: ExprId, expected: Expectation) -> Ty {
        let body = self.body.clone();

        match &body[id] {
            | Expr::Missing => self.error(),
            | Expr::Lit { lit } => match lit {
                | Literal::Int(_) => {
                    let var = self.ctx.fresh_type(self.level);
                    // TODO: add AnyInt constraint
                    var
                },
                | l => todo!("{l:?}"),
            },
            | Expr::Path { def: None, .. } => self.error(),
            | Expr::Path { def: Some(def), .. } => {
                let ty = match def {
                    | ValueDefId::CtorId(id) => Ty::new(self.db, TyKind::Ctor(id.type_ctor(self.db))),
                    | ValueDefId::PatId(id) => self.result.type_of_pat[*id],
                    | _ => todo!(),
                };

                ty
            },
            | Expr::App { base, args } => {
                let func = self.infer_expr_inner(*base, Expectation::None);
                let params = args
                    .iter()
                    .map(|a| self.infer_expr_inner(*a, Expectation::None))
                    .collect();
                let ret = self.ctx.fresh_type(self.level);
                let new_func = Ty::new(
                    self.db,
                    TyKind::Func(FuncType {
                        params,
                        ret,
                        env: self.ctx.fresh_type(self.level),
                        variadic: false,
                    }),
                );

                self.unify_types(func, new_func, (*base).into());
                ret
            },
            | Expr::If { cond, then, else_ } => {
                let expected = expected.adjust_for_branches(self.db);
                let bool_type = self.bool_type();

                self.infer_expr(*cond, Expectation::HasType(bool_type));

                let result_ty = self.ctx.fresh_type(self.level);
                let then_ty = self.infer_expr_inner(*then, expected);
                let else_ty = match else_ {
                    | Some(else_) => self.infer_expr_inner(*else_, expected),
                    | None => self.unit_type(),
                };

                self.unify_types(then_ty, result_ty, id.into());
                self.unify_types(else_ty, result_ty, id.into());
                result_ty
            },
            | e => todo!("{e:?}"),
        }
    }
}
