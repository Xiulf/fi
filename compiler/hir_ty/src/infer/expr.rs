use super::{BodyInferenceContext, Breakable, InferenceDiagnostic};
use crate::display::HirDisplay;
use crate::lower::LowerCtx;
use crate::ty::*;
use hir_def::expr::{Expr, ExprId, Literal, Stmt};
use hir_def::id::TypeVarOwner;
use hir_def::resolver::{HasResolver, Resolver, ValueNs};
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

                    self.instantiate(ty, expr.into())
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
            | Expr::Infix { op, lhs, rhs } => match self.resolver.resolve_value_fully(self.db.upcast(), op) {
                | Some(ValueNs::Fixity(id)) => 't: {
                    let data = self.db.fixity_data(id);
                    let resolver = id.resolver(self.db.upcast());
                    let id = match resolver.resolve_value_fully(self.db.upcast(), &data.func) {
                        | Some(ValueNs::Func(id)) => id.into(),
                        | Some(ValueNs::Ctor(id)) => id.into(),
                        | _ => break 't self.error(),
                    };

                    let ty = self.db.value_ty(id);
                    let ty = self.instantiate(ty, expr.into());
                    let mid = self.check_app(ty, *lhs, expr);

                    self.check_app(mid, *rhs, expr)
                },
                | _ => {
                    self.report(InferenceDiagnostic::UnresolvedOperator { id: expr.into() });
                    self.error()
                },
            },
            | Expr::App { base, arg } => {
                let base_ty = self.infer_expr(*base);

                self.check_app(base_ty, *arg, expr)
            },
            | Expr::Field { base, field } => {
                if let Some(idx) = field.as_tuple_index() {
                    let base_ty = self.infer_expr(*base);
                    let base_ty = self.subst_type(base_ty);

                    if let TyKind::Tuple(tys) = base_ty.lookup(self.db) {
                        tys[idx]
                    } else {
                        unimplemented!("{}", base_ty.display(self.db));
                    }
                } else {
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
                }
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
                let ret = self.fresh_type();
                let mut ty = ret;
                let params = pats.iter().map(|&p| self.infer_pat(p)).collect::<Vec<_>>();

                self.clos_ret_type = Some(ret);
                self.check_block(stmts, ret, expr.into());
                self.clos_ret_type = None;

                for param in params.into_iter().rev() {
                    ty = self.fn_type(param, ty);
                }

                ty
            },
            | Expr::If { cond, then, else_, .. } => {
                self.check_expr(*cond, self.lang_type("bool-type"));

                if let Some(else_) = else_ {
                    let then_ty = self.infer_expr(*then);
                    let then_ty = self.subst_type(then_ty);
                    let else_ty = self.infer_expr(*else_);
                    let else_ty = self.subst_type(else_ty);

                    if then_ty == self.lang_type("never-type") {
                        else_ty
                    } else if else_ty == self.lang_type("never-type") {
                        then_ty
                    } else {
                        if !self.unify_types(then_ty, else_ty) {
                            self.report_mismatch(then_ty, else_ty, (*else_).into());
                        }

                        then_ty
                    }
                } else {
                    self.infer_expr(*then);
                    self.unit()
                }
            },
            | Expr::Case { pred, arms } => {
                if let TypeVarOwner::DefWithBodyId(def) = self.owner {
                    let pred_ty = self.infer_expr(*pred);
                    let bool_ty = self.lang_type("bool-type");
                    let never_ty = self.lang_type("never-type");
                    let res = self.fresh_type();

                    for arm in arms {
                        self.check_pat(arm.pat, pred_ty);

                        let new_resolver = Resolver::for_expr(self.db.upcast(), def, arm.expr);
                        let old_resolver = std::mem::replace(&mut self.resolver, new_resolver);

                        if let Some(guard) = arm.guard {
                            self.check_expr(guard, bool_ty);
                        }

                        self.check_expr(arm.expr, res);
                        self.resolver = old_resolver;
                    }

                    res
                } else {
                    unreachable!();
                }
            },
            | Expr::While { cond, body, .. } => {
                self.breakable.push(Breakable::While);
                self.check_expr(*cond, self.lang_type("bool-type"));
                self.infer_expr(*body);
                self.breakable.pop().unwrap();
                self.unit()
            },
            | Expr::Loop { body } => {
                self.breakable.push(Breakable::Loop(self.lang_type("never-type")));
                self.infer_expr(*body);

                match self.breakable.pop().unwrap() {
                    | Breakable::Loop(ty) => ty,
                    | _ => unreachable!(),
                }
            },
            | Expr::Next { expr: inner } => {
                if let Some(inner) = inner {
                    if let Some(block_ret) = self.block_ret_type {
                        self.check_expr(*inner, block_ret);
                    } else if let Some(_) = self.breakable.last() {
                        self.report(InferenceDiagnostic::CannotNextWithValue { id: expr.into() });
                    } else {
                        self.report(InferenceDiagnostic::NextOutsideLoop { id: expr.into() });
                    }
                } else if let None = self.breakable.last() {
                    self.report(InferenceDiagnostic::NextOutsideLoop { id: expr.into() });
                }

                self.lang_type("never-type")
            },
            | Expr::Break { expr: inner } => {
                if let Some(inner) = inner {
                    if let Some(break_type) = self.block_break_type {
                        self.check_expr(*inner, break_type);
                    } else if let Some(&br) = self.breakable.last() {
                        if let Breakable::Loop(ty) = br {
                            if ty == self.lang_type("never-type") {
                                let ty = self.infer_expr(*inner);

                                *self.breakable.last_mut().unwrap() = Breakable::Loop(ty);
                            } else {
                                self.check_expr(*inner, ty);
                            }
                        } else {
                            self.report(InferenceDiagnostic::CannotBreakWithValue { id: expr.into() });
                        }
                    } else {
                        self.report(InferenceDiagnostic::BreakOutsideLoop { id: expr.into() });
                    }
                } else if let Some(&br) = self.breakable.last() {
                    if let Breakable::Loop(ty) = br {
                        let unit = self.unit();

                        if ty == self.lang_type("never-type") {
                            *self.breakable.last_mut().unwrap() = Breakable::Loop(unit);
                        } else if !self.unify_types(ty, unit) {
                            self.report_mismatch(unit, ty, expr.into());
                        }
                    }
                } else {
                    self.report(InferenceDiagnostic::BreakOutsideLoop { id: expr.into() });
                }

                self.lang_type("never-type")
            },
            | Expr::Yield { exprs } => {
                let ret = self.fresh_type();
                let mut ty = ret;

                for &expr in exprs.iter().rev() {
                    let infer = self.infer_expr(expr);

                    ty = self.fn_type(infer, ty);
                }

                if let Some(yield_ty) = self.yield_type {
                    if !self.unify_types(yield_ty, ty) {
                        self.report(InferenceDiagnostic::MismatchedType {
                            id: expr.into(),
                            expected: yield_ty,
                            found: ty,
                        });
                    }
                } else {
                    self.yield_type = Some(ty);
                }

                ret
            },
            | Expr::Return { expr: inner } => {
                let ret = self.clos_ret_type.unwrap_or(self.ret_type);

                if let Some(inner) = inner {
                    self.check_expr(*inner, ret);
                } else {
                    let unit = self.unit();

                    if !self.unify_types(ret, unit) {
                        self.report(InferenceDiagnostic::MismatchedType {
                            id: expr.into(),
                            expected: ret,
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
            let mut diverges = false;

            for (i, stmt) in stmts.iter().enumerate() {
                match *stmt {
                    | Stmt::Expr { expr } => {
                        self.resolver = Resolver::for_expr(self.db.upcast(), def, expr);

                        if i == last {
                            return self.infer_expr(expr);
                        } else {
                            let ty = self.infer_expr(expr);
                            let ty = self.subst_type(ty);

                            if ty == self.lang_type("never-type") {
                                diverges = true;
                            }
                        }
                    },
                    | Stmt::Bind { pat, val } => {
                        let ty = self.infer_pat(pat);

                        self.resolver = Resolver::for_expr(self.db.upcast(), def, val);
                        self.check_expr(val, ty);
                    },
                    | Stmt::Let { pat, val } => {
                        let ty = self.infer_pat(pat);

                        self.resolver = Resolver::for_expr(self.db.upcast(), def, val);
                        self.check_expr(val, ty);
                    },
                }
            }

            self.resolver = old_resolver;

            if diverges {
                self.lang_type("never-type")
            } else {
                self.unit()
            }
        } else {
            self.error()
        }
    }

    pub fn check_expr(&mut self, expr: ExprId, expected: Ty) {
        let body = Arc::clone(&self.body);

        self.result.type_of_expr.insert(expr, expected);

        match (&body[expr], expected.lookup(self.db)) {
            | (_, TyKind::ForAll(kind, inner)) => {
                let sk = self.skolemize(kind, inner);

                self.check_expr(expr, sk);
            },
            | (Expr::Path { .. }, TyKind::Ctnt(ctnt, inner)) => {
                self.class_env.push(ctnt, true);
                self.check_expr(expr, inner);
                self.class_env.pop();
            },
            | (_, TyKind::Ctnt(ctnt, inner)) => {
                self.class_env.push(ctnt, false);
                self.check_expr(expr, inner);
                self.class_env.pop();
            },
            | (_, TyKind::Unknown(_)) => {
                let infer = self.infer_expr(expr);
                let infer = self.instantiate(infer, expr.into());

                if !self.unify_types(infer, expected) {
                    self.report_mismatch(expected, infer, expr.into());
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
            | (Expr::Path { path }, _) => match self.resolver.resolve_value_fully(self.db.upcast(), path) {
                | Some(res) => {
                    let ty = match res {
                        | ValueNs::Local(pat) => self.result.type_of_pat[pat],
                        | ValueNs::Fixity(_) => unimplemented!(),
                        | ValueNs::Func(id) => {
                            if self.owner == TypeVarOwner::DefWithBodyId(id.into()) {
                                self.subst_type(self.result.self_type)
                            } else {
                                self.db.value_ty(id.into())
                            }
                        },
                        | ValueNs::Static(id) => self.db.value_ty(id.into()),
                        | ValueNs::Const(id) => self.db.value_ty(id.into()),
                        | ValueNs::Ctor(id) => self.db.value_ty(id.into()),
                    };

                    if !self.subsume_types(ty, expected, expr.into()) {
                        self.report_mismatch(expected, ty, expr.into());
                    }
                },
                | None => {
                    let error = self.error();

                    self.report(InferenceDiagnostic::UnresolvedValue { id: expr.into() });
                    self.unify_types(expected, error);
                },
            },
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
            | (Expr::App { base, arg }, _) => {
                let base_ty = self.infer_expr(*base);
                let ret = self.check_app(base_ty, *arg, expr);

                if !self.subsume_types(ret, expected, expr.into()) {
                    self.report_mismatch(expected, ret, expr.into());
                }
            },
            | (Expr::Tuple { exprs }, TyKind::Tuple(tys)) if exprs.len() == tys.len() => {
                for (&expr, &exp) in exprs.iter().zip(tys.iter()) {
                    self.check_expr(expr, exp);
                }
            },
            | (Expr::Do { stmts }, _) => self.check_block(stmts, expected, expr),
            | (Expr::Clos { pats, stmts }, _) => {
                use hir_def::id::HasModule;
                let module = self.owner.module(self.db.upcast());
                let block_ty = self.db.lang_item(module.lib, "block-type".into()).unwrap();
                let block_ty = block_ty.as_type_ctor().unwrap();

                if let Some([f_ty, r_ty]) = expected.match_ctor(self.db, block_ty) {
                    let ret = self.fresh_type();
                    let mut ty = ret;

                    self.block_ret_type = Some(ret);
                    self.block_break_type = Some(r_ty);

                    for &pat in pats.iter().rev() {
                        let arg = self.infer_pat(pat);

                        ty = self.fn_type(arg, ty);
                    }

                    self.check_block(stmts, ret, expr.into());
                    self.block_ret_type = None;
                    self.block_break_type = None;

                    if !self.unify_types(f_ty, ty) {
                        self.report_mismatch(f_ty, ty, expr.into());
                    }
                } else {
                    let infer = self.infer_expr(expr);

                    if !self.subsume_types(infer, expected, expr.into()) {
                        self.report(InferenceDiagnostic::MismatchedType {
                            id: expr.into(),
                            expected,
                            found: infer,
                        });
                    }
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

    pub fn check_app(&mut self, base_ty: Ty, arg: ExprId, expr: ExprId) -> Ty {
        let base_ty = self.subst_type(base_ty);

        match base_ty.lookup(self.db) {
            | TyKind::App(b, r) => match b.lookup(self.db) {
                | TyKind::App(f, a) => {
                    let func_ty = self.lang_type("fn-type");

                    if !self.unify_types(f, func_ty) {
                        self.report_mismatch(func_ty, f, expr.into());
                    }

                    self.check_expr(arg, a);
                    r
                },
                | _ => {
                    let param = self.infer_expr(arg);
                    let ret = self.fresh_type();
                    let func_ty = self.fn_type(param, ret);

                    if !self.unify_types(base_ty, func_ty) {
                        self.report_mismatch(base_ty, func_ty, expr.into());
                    }

                    ret
                },
            },
            | TyKind::ForAll(kind, ty) => {
                let repl = self.fresh_type_with_kind(kind);
                let ty = ty.replace_var(self.db, repl);

                self.check_app(ty, arg, expr)
            },
            | TyKind::Ctnt(ctnt, ty) => {
                self.constrain(expr.into(), ctnt);
                self.check_app(ty, arg, expr)
            },
            | _ => {
                let arg = self.infer_expr(arg);
                let ret = self.fresh_type();
                let func_ty = self.fn_type(arg, ret);

                if !self.unify_types(base_ty, func_ty) {
                    self.report_mismatch(base_ty, func_ty, expr.into());
                }

                ret
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
                            self.resolver = old_resolver;
                            return;
                        } else {
                            self.infer_expr(expr);
                        }
                    },
                    | Stmt::Bind { pat, val } => {
                        let ty = self.infer_pat(pat);

                        self.resolver = Resolver::for_expr(self.db.upcast(), def, val);
                        self.check_expr(val, ty);
                    },
                    | Stmt::Let { pat, val } => {
                        let ty = self.infer_pat(pat);

                        self.resolver = Resolver::for_expr(self.db.upcast(), def, val);
                        self.check_expr(val, ty);
                    },
                }
            }

            let unit = self.unit();

            if !self.unify_types(expected, unit) {
                self.report_mismatch(expected, unit, expr.into());
            }

            self.resolver = old_resolver;
        }
    }
}
