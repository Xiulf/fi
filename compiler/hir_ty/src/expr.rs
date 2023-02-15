use hir_def::expr::{Expr, ExprId, Literal, Stmt};
use hir_def::id::ValueDefId;
use hir_def::pat::{Case, DecisionTree, VariantTag};

use crate::ctx::{BodyCtx, Expectation};
use crate::ty::{FuncType, Ty, TyKind};

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
            | Expr::Lit { lit } => match lit {
                | Literal::Int(_) => {
                    let var = self.ctx.fresh_type(self.level, false);

                    if let Some(_any_int) = self.any_int_trait() {
                        // TODO: add AnyInt constraint
                    }

                    var
                },
                | l => todo!("{l:?}"),
            },
            | Expr::Block { stmts, expr } => self.infer_block(stmts, *expr, expected),
            | Expr::Path { def: None, .. } => self.error(),
            | Expr::Path { def: Some(def), .. } => self.infer_value_def_id(id, *def),
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
                decision_tree,
            } => {
                let expected = expected.adjust_for_branches(self.db);
                let pred = self.infer_expr(*expr, Expectation::None);
                let res = self.ctx.fresh_type(self.level, false);

                self.infer_decision_tree(decision_tree, Expectation::HasType(pred));

                for &branch in branches.iter() {
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
        let ty = match def {
            | ValueDefId::FixityId(id) => match hir_def::data::fixity_data(self.db, id).def(self.db) {
                | Some(def) => return self.infer_value_def_id(expr, def.unwrap_left()),
                | None => return self.error(),
            },
            | ValueDefId::ValueId(id) if self.owner == id.into() => self.result.ty.clone(),
            | ValueDefId::ValueId(id) => crate::infer(self.db, id).ty.clone(),
            | ValueDefId::CtorId(id) => crate::ctor_ty(self.db, id),
            | ValueDefId::PatId(id) => return self.result.type_of_pat[id],
            | d => todo!("{d:?}"),
        };

        self.instantiate(ty, false)
    }

    fn infer_block(&mut self, stmts: &[Stmt], expr: Option<ExprId>, expected: Expectation) -> Ty {
        for stmt in stmts {
            match *stmt {
                | Stmt::Expr(e) => {
                    self.infer_expr_inner(e, Expectation::None);
                },
                | Stmt::Let(p, e) => {
                    let ty = self.infer_pat(p, Expectation::None);
                    self.infer_expr_inner(e, Expectation::HasType(ty));
                },
            }
        }

        if let Some(expr) = expr {
            return self.infer_expr(expr, expected);
        }

        self.unit_type()
    }

    fn infer_decision_tree(&mut self, tree: &DecisionTree, expected: Expectation) {
        match tree {
            | DecisionTree::Guard(_guard, _next) => todo!("guards"),
            | DecisionTree::Switch(pat, cases) => {
                let ty = self.infer_pat(*pat, expected);

                for case in cases {
                    self.infer_case(case, ty);
                }
            },
            | _ => {},
        }
    }

    fn infer_case(&mut self, case: &Case, expected: Ty) {
        if let Some(tag) = &case.tag {
            match tag {
                | VariantTag::Literal(_lit) => {},
                | VariantTag::Ctor(id) => {
                    let ty = crate::ctor_ty(self.db, *id);
                    let ty = self.instantiate(ty, false);

                    if let TyKind::Func(func) = ty.kind(self.db) {
                        assert_eq!(case.fields.len(), func.params.len());

                        for (fields, &param) in case.fields.iter().zip(func.params.iter()) {
                            for &field in fields {
                                self.infer_pat(field, Expectation::HasType(param));
                            }
                        }
                    } else {
                        assert!(case.fields.is_empty());
                    }

                    self.infer_decision_tree(&case.branch, Expectation::None);
                    return;
                },
            }
        }

        assert_eq!(case.fields.len(), 1);
        for &field in &case.fields[0] {
            self.result.type_of_pat.insert(field, expected);
        }

        self.infer_decision_tree(&case.branch, Expectation::None);
    }
}
