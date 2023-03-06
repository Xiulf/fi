use hir_def::expr::{Expr, ExprId, Literal, Stmt};
use hir_def::id::ValueDefId;
use hir_def::pat::PatId;

use crate::ctx::{BodyCtx, Expectation};
use crate::lower::LowerCtx;
use crate::ty::{Constraint, ConstraintOrigin, FuncType, Ty, TyKind};

impl BodyCtx<'_, '_> {
    pub fn infer_expr(&mut self, id: ExprId, expected: Expectation) -> Ty {
        let ty = self.infer_expr_inner(id, expected);

        if let Expectation::HasType(expected) = expected {
            self.unify_types(ty, expected, id.into());

            if matches!(self.resolve_type_shallow(ty).kind(self.db), TyKind::Error) {
                return expected;
            }
        }

        ty
    }

    fn infer_expr_inner(&mut self, id: ExprId, expected: Expectation) -> Ty {
        let body = self.body.clone();
        let ty = match &body[id] {
            | Expr::Missing => self.error(),
            | Expr::Typed { expr, ty } => {
                let (type_map, _, _) = self.owner.type_map(self.db);
                let mut lcx = LowerCtx::new(self, type_map);
                let ty = lcx.lower_type_ref(*ty, false);

                self.infer_expr(*expr, Expectation::HasType(ty))
            },
            | Expr::Lit { lit } => match lit {
                | Literal::Int(_) => {
                    let var = self.ctx.fresh_type(self.level, false);

                    if let Some(any_int) = self.any_int_trait() {
                        self.constrain(
                            Constraint {
                                trait_id: any_int,
                                args: Box::new([var]),
                            },
                            ConstraintOrigin::ExprId(id),
                        );
                    }

                    var
                },
                | Literal::Float(_) => {
                    let var = self.ctx.fresh_type(self.level, false);

                    if let Some(any_float) = self.any_float_trait() {
                        self.constrain(
                            Constraint {
                                trait_id: any_float,
                                args: Box::new([var]),
                            },
                            ConstraintOrigin::ExprId(id),
                        );
                    }

                    var
                },
                | l => todo!("{l:?}"),
            },
            | Expr::Block { stmts, expr } => self.infer_block(stmts, *expr, expected),
            | Expr::Path { def: None, .. } => self.error(),
            | Expr::Path { def: Some(def), .. } => self.infer_value_def_id(id, *def),
            | Expr::Lambda { env, params, body } => self.infer_lambda(id, env, params, *body),
            | Expr::App { base, args } => {
                let func = self.infer_expr_inner(*base, Expectation::None);
                let params = args
                    .iter()
                    .map(|a| self.infer_expr_inner(*a, Expectation::None))
                    .collect();
                let ret = self.ctx.fresh_type(self.level, false);
                let new_func = Ty::new(
                    self.db,
                    TyKind::Func(FuncType {
                        params,
                        ret,
                        env: self.ctx.fresh_type(self.level, false),
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

                let result_ty = self.ctx.fresh_type(self.level, false);
                let then_ty = self.infer_expr_inner(*then, expected);
                let else_ty = match else_ {
                    | Some(else_) => self.infer_expr_inner(*else_, expected),
                    | None => self.unit_type(),
                };

                self.unify_types(then_ty, result_ty, id.into());
                self.unify_types(else_ty, result_ty, id.into());
                result_ty
            },
            | Expr::Match {
                expr,
                branches,
                decision_tree: _,
            } => {
                let expected = expected.adjust_for_branches(self.db);
                let pred = self.infer_expr(*expr, Expectation::None);
                let res = self.ctx.fresh_type(self.level, false);

                // self.infer_decision_tree(decision_tree, Expectation::HasType(pred));

                for &(pat, branch) in branches.iter() {
                    self.infer_pat(pat, Expectation::HasType(pred));
                    let ty = self.infer_expr_inner(branch, expected);
                    self.unify_types(ty, res, branch.into());
                }

                res
            },
            | e => todo!("{e:?}"),
        };

        self.result.type_of_expr.insert(id, ty);
        ty
    }

    fn infer_value_def_id(&mut self, expr: ExprId, def: ValueDefId) -> Ty {
        let (ty, constraints) = match def {
            | ValueDefId::ValueId(id) if self.owner == id.into() => (self.result.ty.clone(), Vec::new()),
            | ValueDefId::ValueId(id) => {
                let infer = crate::infer(self.db, id);
                (infer.ty.clone(), infer.constraints.clone())
            },
            | ValueDefId::FixityId(id) => match hir_def::data::fixity_data(self.db, id).def(self.db) {
                | Some(def) => return self.infer_value_def_id(expr, def.unwrap_left()),
                | None => return self.error(),
            },
            | ValueDefId::CtorId(id) => (crate::ctor_ty(self.db, id), Vec::new()),
            | ValueDefId::PatId(id) => return self.result.type_of_pat[id],
            | d => todo!("{d:?}"),
        };

        let (ty, constraints) = self.instantiate(ty, constraints, false);

        for constraint in constraints {
            self.constrain(constraint, ConstraintOrigin::ExprId(expr));
        }

        ty
    }

    fn infer_lambda(&mut self, _id: ExprId, env: &[PatId], params: &[PatId], body: ExprId) -> Ty {
        let env = env.iter().map(|&p| self.infer_pat(p, Expectation::None)).collect();
        let env = self.tuple_type(env);
        let params = params.iter().map(|&p| self.infer_pat(p, Expectation::None)).collect();
        let ret = self.infer_expr_inner(body, Expectation::None);

        Ty::new(
            self.db,
            TyKind::Func(FuncType {
                variadic: false,
                env,
                params,
                ret,
            }),
        )
    }

    fn infer_block(&mut self, stmts: &[Stmt], expr: Option<ExprId>, expected: Expectation) -> Ty {
        for stmt in stmts {
            match *stmt {
                | Stmt::Expr(e) => {
                    self.infer_expr_inner(e, Expectation::None);
                },
                | Stmt::Let(p, e) => {
                    let ty = self.infer_pat(p, Expectation::None);
                    self.infer_expr(e, Expectation::HasType(ty));
                },
            }
        }

        if let Some(expr) = expr {
            return self.infer_expr(expr, expected);
        }

        self.unit_type()
    }
}
