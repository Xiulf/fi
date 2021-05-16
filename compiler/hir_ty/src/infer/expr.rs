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
                let ty_ = lcx.lower_ty(*ty);

                self.check_kind_type(ty_, *ty);
                self.check_expr(*expr, ty_);
                ty_
            }),
            | Expr::Path { path } => match self.resolver.resolve_value_fully(self.db.upcast(), path) {
                | Some(res) => 't: {
                    let id = match res {
                        | ValueNs::Local(pat) => break 't self.result.type_of_pat[pat],
                        | ValueNs::Fixity(id) => unimplemented!(),
                        | ValueNs::Func(id) => {
                            if self.owner == TypeVarOwner::DefWithBodyId(id.into()) {
                                break 't self.subst_type(self.result.self_type);
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
            | Expr::Field { base, field } => {
                let row_kind = self.lang_type("row-kind");
                let type_kind = self.lang_type("type-kind");
                let record_type = self.lang_type("record-type");
                let kind = TyKind::App(row_kind, type_kind).intern(self.db);
                let tail = self.fresh_type_with_kind(kind);
                let res = self.fresh_type();
                let fields = vec![Field {
                    name: field.clone(),
                    ty: res,
                }]
                .into();

                let row = TyKind::Row(fields, Some(tail)).intern(self.db);
                let record = TyKind::App(record_type, row).intern(self.db);

                self.check_expr(*base, record);
                res
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
            | Expr::Cast { expr, ty } => {
                self.infer_expr(*expr);

                self.owner.with_type_map(self.db.upcast(), |type_map| {
                    let mut lcx = LowerCtx::new(type_map, self);
                    let ty_ = lcx.lower_ty(*ty);

                    self.check_kind_type(ty_, *ty);
                    ty_
                })
            },
            | Expr::Tuple { exprs } => {
                let tys = exprs.iter().map(|&e| self.infer_expr(e)).collect();

                TyKind::Tuple(tys).intern(self.db)
            },
            | Expr::Record { fields } => {
                let record_type = self.lang_type("record-type");
                let fields = fields
                    .iter()
                    .map(|f| Field {
                        name: f.name.clone(),
                        ty: self.infer_expr(f.val),
                    })
                    .collect();

                let row = TyKind::Row(fields, None).intern(self.db);

                TyKind::App(record_type, row).intern(self.db)
            },
            | Expr::Array { exprs } => {
                let array_type = self.lang_type("array-type");
                let len = TyKind::Figure(exprs.len() as i128).intern(self.db);
                let elem_ty = self.fresh_type();

                for &expr in exprs {
                    self.check_expr(expr, elem_ty);
                }

                let base = TyKind::App(array_type, elem_ty).intern(self.db);

                TyKind::App(base, len).intern(self.db)
            },
            | Expr::Do { stmts } => self.infer_block(stmts, expr),
            | Expr::Clos { pats, stmts } => {
                let params = pats.iter().map(|&p| self.infer_pat(p)).collect::<Vec<_>>();
                let mut ty = self.infer_block(stmts, expr);

                for param in params.into_iter().rev() {
                    ty = self.fn_type(param, ty);
                }

                ty
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
            | Expr::While { cond, body, .. } => {
                self.check_expr(*cond, self.lang_type("bool-type"));
                self.infer_expr(*body);
                self.unit()
            },
            | Expr::Loop { body } => {
                self.infer_expr(*body);
                self.lang_type("never-type")
            },
            | Expr::Next => self.lang_type("never-type"),
            | Expr::Break { expr } => {
                if let Some(expr) = expr {
                    self.infer_expr(*expr);
                }

                self.lang_type("never-type")
            },
            | Expr::Yield { expr } => {
                if let Some(expr) = expr {
                    self.check_expr(*expr, self.yield_type);
                }

                self.fresh_type()
            },
            | Expr::Return { expr: inner } => {
                if let Some(inner) = inner {
                    self.check_expr(*inner, self.ret_type);
                } else {
                    let ret_type = self.ret_type;
                    let unit = self.unit();

                    if !self.unify_types(ret_type, unit) {
                        self.report(InferenceDiagnostic::MismatchedType {
                            id: expr.into(),
                            expected: ret_type,
                            found: unit,
                        });
                    }
                }

                self.lang_type("never-type")
            },
            | e => unimplemented!("{:?}", e),
        };

        // eprintln!("{:?} :: {}", body[expr], ty.display(self.db));

        self.result.type_of_expr.insert(expr, ty);
        ty
    }

    pub fn infer_block(&mut self, stmts: &[Stmt], expr: ExprId) -> Ty {
        if let TypeVarOwner::DefWithBodyId(def) = self.owner {
            let new_resolver = Resolver::for_expr(self.db.upcast(), def, expr);
            let old_resolver = std::mem::replace(&mut self.resolver, new_resolver);
            let last = stmts.len() - 1;

            for (i, stmt) in stmts.iter().enumerate() {
                match *stmt {
                    | Stmt::Expr { expr } => {
                        self.resolver = Resolver::for_expr(self.db.upcast(), def, expr);

                        if i == last {
                            return self.infer_expr(expr);
                        } else {
                            self.infer_expr(expr);
                        }
                    },
                    | Stmt::Bind { pat, val } => {
                        self.resolver = Resolver::for_expr(self.db.upcast(), def, val);

                        let ty = self.infer_expr(val);

                        self.check_pat(pat, ty);
                    },
                    | Stmt::Let { pat, val } => {
                        self.resolver = Resolver::for_expr(self.db.upcast(), def, val);

                        let ty = self.infer_expr(val);

                        self.check_pat(pat, ty);
                    },
                }
            }

            self.resolver = old_resolver;
            self.unit()
        } else {
            self.error()
        }
    }

    pub fn check_expr(&mut self, expr: ExprId, expected: Ty) {
        let body = Arc::clone(&self.body);

        self.result.type_of_expr.insert(expr, expected);

        match (&body[expr], expected.lookup(self.db)) {
            | (_, TyKind::ForAll(kind, inner)) => {
                let skolem = self.enter_universe();
                let sk = self.skolemize(skolem, kind, inner);

                self.check_expr(expr, sk);
                self.exit_universe();
                self.result.type_of_expr.insert(expr, expected);
            },
            | (_, TyKind::Ctnt(ctnt, inner)) => {
                // @TODO: check constraint
                self.check_expr(expr, inner);
            },
            | (_, TyKind::Unknown(_)) => {
                let infer = self.infer_expr(expr);

                if !self.unify_types(infer, expected) {
                    self.report(InferenceDiagnostic::MismatchedType {
                        id: expr.into(),
                        expected,
                        found: infer,
                    });
                }
            },
            | (Expr::Typed { expr: inner, ty }, _) => self.owner.with_type_map(self.db.upcast(), |type_map| {
                let mut lcx = LowerCtx::new(type_map, self);
                let ty_ = lcx.lower_ty(*ty);
                let kind = lcx.infer_kind(expected, *ty);

                self.check_kind(ty_, kind, *ty);

                if !self.subsume_types(ty_, expected, expr.into()) {
                    self.report(InferenceDiagnostic::MismatchedType {
                        id: expr.into(),
                        expected,
                        found: ty_,
                    });
                }

                self.check_expr(*inner, ty_);
            }),
            | (Expr::Lit { lit: Literal::Int(_) }, _) => {
                let integer = self.lang_class("integer-class");

                self.constrain(expr.into(), Constraint {
                    class: integer,
                    types: vec![expected].into(),
                });
            },
            | (Expr::Lit { lit: Literal::Float(_) }, _) => {
                let decimal = self.lang_class("decimal-class");

                self.constrain(expr.into(), Constraint {
                    class: decimal,
                    types: vec![expected].into(),
                });
            },
            | (Expr::Tuple { exprs }, TyKind::Tuple(tys)) if exprs.len() == tys.len() => {
                for (&expr, &exp) in exprs.iter().zip(tys.iter()) {
                    self.check_expr(expr, exp);
                }
            },
            | (_, _) => {
                let infer = self.infer_expr(expr);

                if !self.subsume_types(infer, expected, expr.into()) {
                    self.report(InferenceDiagnostic::MismatchedType {
                        id: expr.into(),
                        expected,
                        found: infer,
                    });
                }
            },
        }
    }

    pub fn check_block(&mut self, stmts: &[Stmt], expected: Ty, expr: ExprId) {
        if let TypeVarOwner::DefWithBodyId(def) = self.owner {
            let new_resolver = Resolver::for_expr(self.db.upcast(), def, expr);
            let old_resolver = std::mem::replace(&mut self.resolver, new_resolver);
            let last = stmts.len() - 1;

            for (i, stmt) in stmts.iter().enumerate() {
                match *stmt {
                    | Stmt::Expr { expr } => {
                        self.resolver = Resolver::for_expr(self.db.upcast(), def, expr);

                        if i == last {
                            self.check_expr(expr, expected);
                        } else {
                            self.infer_expr(expr);
                        }
                    },
                    | Stmt::Bind { pat, val } => {
                        self.resolver = Resolver::for_expr(self.db.upcast(), def, val);

                        let ty = self.infer_expr(val);

                        self.check_pat(pat, ty);
                    },
                    | Stmt::Let { pat, val } => {
                        self.resolver = Resolver::for_expr(self.db.upcast(), def, val);

                        let ty = self.infer_expr(val);

                        self.check_pat(pat, ty);
                    },
                }
            }

            self.resolver = old_resolver;
        }
    }
}
