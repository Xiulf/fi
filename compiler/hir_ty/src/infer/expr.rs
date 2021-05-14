use super::{BodyInferenceContext, InferenceDiagnostic};
use crate::display::HirDisplay;
use crate::lower::LowerCtx;
use crate::ty::*;
use hir_def::expr::{Expr, ExprId, Literal, Stmt};
use hir_def::id::TypeVarOwner;
use hir_def::resolver::{Resolver, ValueNs};
use std::sync::Arc;

impl BodyInferenceContext<'_> {
    pub fn infer_expr(&mut self, expr: ExprId) -> Ty {
        self.db.check_canceled();

        let body = Arc::clone(&self.body);
        let ty = match &body[expr] {
            | Expr::Missing => self.error(),
            | Expr::Typed { expr, ty } => self.owner.with_type_map(self.db.upcast(), |type_map| {
                let mut lcx = LowerCtx::new(type_map, self);
                let ty = lcx.lower_ty(*ty);

                self.check_expr(*expr, ty);
                ty
            }),
            | Expr::Path { path } => match self.resolver.resolve_value_fully(self.db.upcast(), path) {
                | Some(res) => 't: {
                    let id = match res {
                        | ValueNs::Local(pat) => break 't self.result.type_of_pat[pat],
                        | ValueNs::Fixity(id) => unimplemented!(),
                        | ValueNs::Func(id) => {
                            if self.owner == TypeVarOwner::DefWithBodyId(id.into()) {
                                break 't self.subst_type(self.self_type);
                            } else {
                                id.into()
                            }
                        },
                        | ValueNs::Static(id) => id.into(),
                        | ValueNs::Const(id) => id.into(),
                        | ValueNs::Ctor(id) => id.into(),
                    };

                    let ty = self.db.value_ty(id);

                    self.instantiate(ty)
                },
                | None => {
                    self.report(InferenceDiagnostic::UnresolvedValue { id: expr.into() });
                    self.error()
                },
            },
            | Expr::Lit { lit } => match lit {
                | Literal::Int(_) => {
                    let integer = self.lang_class("integer-class");
                    let ty = self.fresh_type();

                    self.constrain(expr.into(), Constraint {
                        class: integer,
                        types: vec![ty].into(),
                    });

                    ty
                },
                | Literal::Float(_) => {
                    let decimal = self.lang_class("decimal-class");
                    let ty = self.fresh_type();

                    self.constrain(expr.into(), Constraint {
                        class: decimal,
                        types: vec![ty].into(),
                    });

                    ty
                },
                | Literal::Char(_) => self.lang_type("char-type"),
                | Literal::String(_) => self.lang_type("str-type"),
            },
            | Expr::App { base, arg } => {
                let arg_ty = self.infer_expr(*arg);
                let ret_ty = self.fresh_type();
                let fn_ty = self.fn_type(arg_ty, ret_ty);

                self.check_expr(*base, fn_ty);
                ret_ty
            },
            | Expr::Deref { expr: inner } => {
                let pointer = self.lang_class("pointer-class");
                let expr_ty = self.infer_expr(*inner);
                let ty = self.fresh_type();

                self.constrain(expr.into(), Constraint {
                    class: pointer,
                    types: vec![expr_ty, ty].into(),
                });

                ty
            },
            | Expr::Tuple { exprs } => {
                let tys = exprs.iter().map(|&e| self.infer_expr(e)).collect();

                TyKind::Tuple(tys).intern(self.db)
            },
            | Expr::Do { stmts } => {
                if let TypeVarOwner::DefWithBodyId(def) = self.owner {
                    let resolver = Resolver::for_expr(self.db.upcast(), def, expr);
                    let old_resolver = std::mem::replace(&mut self.resolver, resolver);
                    let ty = self.infer_block(stmts);

                    self.resolver = old_resolver;
                    ty
                } else {
                    self.error()
                }
            },
            | Expr::If { cond, then, else_, .. } => {
                self.check_expr(*cond, self.lang_type("bool-type"));

                if let Some(else_) = else_ {
                    let then_ty = self.infer_expr(*then);

                    self.check_expr(*else_, then_ty);
                    then_ty
                } else {
                    self.infer_expr(*then);
                    self.unit()
                }
            },
            | e => unimplemented!("{:?}", e),
        };

        // eprintln!("{:?} :: {}", body[expr], ty.display(self.db));

        self.result.type_of_expr.insert(expr, ty);
        ty
    }

    pub fn infer_block(&mut self, stmts: &[Stmt]) -> Ty {
        let last = stmts.len() - 1;

        for (i, stmt) in stmts.iter().enumerate() {
            match *stmt {
                | Stmt::Expr { expr } if i == last => return self.infer_expr(expr),
                | Stmt::Expr { expr } => {
                    self.infer_expr(expr);
                },
                | Stmt::Bind { pat, val } => {
                    self.infer_pat(pat);
                    self.infer_expr(val);
                },
                | Stmt::Let { pat, val } => {
                    self.infer_pat(pat);
                    self.infer_expr(val);
                },
            }
        }

        self.unit()
    }

    pub fn check_expr(&mut self, expr: ExprId, expected: Ty) {
        let body = Arc::clone(&self.body);

        self.result.type_of_expr.insert(expr, expected);

        match (&body[expr], expected.lookup(self.db)) {
            | (Expr::Tuple { exprs }, TyKind::Tuple(tys)) => {
                if exprs.len() != tys.len() {
                    // @TODO: report error
                }

                for (&expr, &exp) in exprs.iter().zip(tys.iter()) {
                    self.check_expr(expr, exp);
                }
            },
            | (_, _) => {
                let infer = self.infer_expr(expr);

                if !self.unify_types(infer, expected) {
                    self.report(InferenceDiagnostic::MismatchedType {
                        id: expr.into(),
                        expected,
                        found: infer,
                    });
                }
            },
        }
    }

    pub fn check_block(&mut self, stmts: &[Stmt], expected: Ty) {
        let last = stmts.len() - 1;

        for (i, stmt) in stmts.iter().enumerate() {
            match *stmt {
                | Stmt::Expr { expr } if i == last => self.check_expr(expr, expected),
                | Stmt::Expr { expr } => self.check_expr(expr, self.unit()),
                | Stmt::Bind { pat, val } => {
                    self.infer_pat(pat);
                    self.infer_expr(val);
                },
                | Stmt::Let { pat, val } => {
                    self.infer_pat(pat);
                    self.infer_expr(val);
                },
            }
        }
    }
}
