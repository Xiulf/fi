use std::sync::Arc;

use hir_def::expr::{Expr, ExprId, Literal, Stmt};
use hir_def::id::{FixityId, TypeVarOwner};
use hir_def::path::Path;
use hir_def::resolver::{HasResolver, Resolver, ValueNs};

use super::diagnostics::{CtntExpected, CtntFound};
use super::{BodyInferenceContext, ExprOrPatId, InferenceContext, InferenceDiagnostic};
// use crate::display::HirDisplay;
use crate::info::{CtntInfo, FieldInfo, ToInfo, TyId, TyInfo};
use crate::lower::LowerCtx;

impl BodyInferenceContext<'_> {
    pub fn infer_expr(&mut self, expr: ExprId) -> TyId {
        self.db.check_canceled();
        log::trace!("infer_expr({:?})", expr);

        let body = Arc::clone(&self.body);
        let src = self.source(expr);
        let ty = match &body[expr] {
            | Expr::Missing => self.error(src),
            | Expr::Typed { expr, ty } => self.owner.with_type_map(self.db.upcast(), |type_map| {
                let mut lcx = LowerCtx::new(type_map, self);
                let ty_ = lcx.lower_ty(*ty);

                self.check_kind_type(ty_);
                self.check_expr(*expr, ty_);
                ty_
            }),
            | Expr::Hole => self.fresh_type(src),
            | Expr::Unit => self.unit(src),
            | Expr::Path { path } => self.icx.infer_path(path, self.resolver.clone(), expr, None),
            | Expr::Lit { lit } => match lit {
                | Literal::Int(_) => {
                    let integer = self.lang_class("integer-class");
                    let ty = self.fresh_type(src);

                    self.constrain(
                        CtntExpected::ExprOrPat(expr.into()),
                        CtntFound::ExprOrPat(expr.into()),
                        CtntInfo {
                            class: integer,
                            types: vec![ty].into(),
                        },
                    );

                    ty
                },
                | Literal::Float(_) => {
                    let decimal = self.lang_class("decimal-class");
                    let ty = self.fresh_type(src);

                    self.constrain(
                        CtntExpected::ExprOrPat(expr.into()),
                        CtntFound::ExprOrPat(expr.into()),
                        CtntInfo {
                            class: decimal,
                            types: vec![ty].into(),
                        },
                    );

                    ty
                },
                | Literal::Char(_) => self.lang_type("char-type", src),
                | Literal::String(_) => self.lang_type("str-type", src),
            },
            | Expr::Infix { exprs, ops } => {
                let exprs = exprs.iter().map(|&e| self.infer_expr(e)).collect::<Vec<_>>();

                self.process_infix(
                    exprs.into_iter(),
                    ops,
                    src,
                    |ctx, i, op, lhs, rhs| {
                        let src = ctx.source((expr, i));
                        let ret = ctx.fresh_type(src);
                        let func_ty = ctx.fn_type([lhs, rhs], ret, src);

                        if !ctx.unify_types(op, func_ty) {
                            ctx.report_mismatch(op, func_ty, expr);
                        }

                        ret
                    },
                    |ctx, i, path, resolver| ctx.infer_path(path, resolver, expr, Some(i)),
                )
            },
            | Expr::App { base, arg } => {
                let base_ty = self.infer_expr(*base);

                self.check_app(base_ty, *arg, expr)
            },
            | Expr::Field { base, field } => {
                let row_kind = self.lang_type("row-kind", src);
                let type_kind = self.lang_type("type-kind", src);
                let record_type = self.lang_type("record-type", src);
                let kind = self.types.insert(TyInfo::App(row_kind, [type_kind].into()), src);
                let tail = self.fresh_type_with_kind(kind, src);
                let res = self.fresh_type(src);
                let fields = vec![FieldInfo {
                    name: field.clone(),
                    ty: res,
                }]
                .into();

                let row = self.types.insert(TyInfo::Row(fields, Some(tail)), src);
                let record = self.types.insert(TyInfo::App(record_type, [row].into()), src);

                self.check_expr(*base, record);
                res
            },
            | Expr::Record { fields } => {
                let record_type = self.lang_type("record-type", src);
                let fields = fields
                    .iter()
                    .map(|f| FieldInfo {
                        name: f.name.clone(),
                        ty: self.infer_expr(f.val),
                    })
                    .collect();

                let row = self.types.insert(TyInfo::Row(fields, None), src);

                self.types.insert(TyInfo::App(record_type, [row].into()), src)
            },
            | Expr::Array { exprs } => {
                let array_type = self.lang_type("array-type", src);
                let len = self.types.insert(TyInfo::Figure(exprs.len() as i128), src);
                let elem_ty = self.fresh_type(src);

                for &expr in exprs.iter() {
                    self.check_expr(expr, elem_ty);
                }

                self.types.insert(TyInfo::App(array_type, [elem_ty, len].into()), src)
            },
            | Expr::Do { stmts } => self.infer_block(stmts, expr),
            | Expr::Try { stmts } => self.infer_try(stmts, expr),
            | Expr::Lambda { pats, body } => {
                let body_src = self.source(*body);
                let ret = self.fresh_type(body_src);
                let params = pats.iter().map(|&p| self.infer_pat(p)).collect::<Vec<_>>();
                let lambda_type = self.fn_type(params, ret, src);
                let old_ret_type = std::mem::replace(&mut self.ret_type, ret);
                let def = self.resolver.body_owner().unwrap();
                let new_resolver = Resolver::for_expr(self.db.upcast(), def, *body);
                let old_resolver = std::mem::replace(&mut self.resolver, new_resolver);

                self.lambda_type.push(lambda_type);
                self.check_expr(*body, ret);
                self.ret_type = old_ret_type;
                self.resolver = old_resolver;
                self.lambda_type.pop().unwrap();
                self.subst_type(lambda_type)
            },
            | Expr::If { cond, then, else_, .. } => {
                let then_src = self.source(*then);
                let cond_src = self.source(*cond);
                let bool_type = self.lang_type("bool-type", cond_src);

                self.check_expr(*cond, bool_type);

                if let Some(else_) = else_ {
                    let _else_src = self.source(*else_);
                    let then_ty = self.infer_expr(*then);
                    let then_ty = self.subst_type(then_ty);
                    let else_ty = self.infer_expr(*else_);
                    let else_ty = self.subst_type(else_ty);
                    let never_ty = self.lang_type("never-type", then_src);

                    if then_ty == never_ty {
                        else_ty
                    } else if else_ty == never_ty {
                        then_ty
                    } else {
                        if !self.unify_types(then_ty, else_ty) {
                            self.report_mismatch(then_ty, else_ty, *else_);
                        }

                        then_ty
                    }
                } else {
                    self.infer_expr(*then);
                    self.unit(src)
                }
            },
            | Expr::Case { pred, arms } => {
                if let TypeVarOwner::DefWithBodyId(def) = self.owner {
                    let pred_ty = self.infer_expr(*pred);
                    let res = self.fresh_type(src);
                    let never_ty = self.lang_type("never-type", src);

                    for arm in arms.iter() {
                        self.check_pat(arm.pat, pred_ty);

                        let new_resolver = Resolver::for_expr(self.db.upcast(), def, arm.expr);
                        let old_resolver = std::mem::replace(&mut self.resolver, new_resolver);

                        if let Some(guard) = arm.guard {
                            let bool_src = self.source(guard);
                            let bool_ty = self.lang_type("bool-type", bool_src);

                            self.check_expr(guard, bool_ty);
                        }

                        let expr = self.infer_expr(arm.expr);
                        let expr = self.subst_type(expr);

                        if self.types[expr] != self.types[never_ty] {
                            if !self.subsume_types(expr, res, arm.expr.into()) {
                                self.report_mismatch(res, expr, arm.expr);
                            }
                        }

                        self.resolver = old_resolver;
                    }

                    res
                } else {
                    unreachable!();
                }
            },
            | Expr::Recur => match self.lambda_type.last() {
                | Some(lambda_type) => *lambda_type,
                | None => todo!(),
            },
            | Expr::Return { expr: inner } => {
                let ret = self.ret_type;

                self.check_expr(*inner, ret);
                self.lang_type("never-type", src)
            },
            | e => unimplemented!("{:?}", e),
        };

        // eprintln!("{:?} :: {}", body[expr], ty.display(self.db));

        self.result.type_of_expr.insert(expr, ty);
        ty
    }

    pub fn infer_block(&mut self, stmts: &[Stmt], expr: ExprId) -> TyId {
        let src = self.source(expr);
        let def = self.resolver.body_owner().unwrap();
        let new_resolver = Resolver::for_expr(self.db.upcast(), def, expr);
        let old_resolver = std::mem::replace(&mut self.resolver, new_resolver);
        let last = stmts.len() - 1;
        let never_ty = self.lang_type("never-type", src);
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

                        if self.types[ty] == self.types[never_ty] {
                            diverges = true;
                        }
                    }
                },
                | Stmt::Let { pat, val } => {
                    let ty = self.infer_pat(pat);

                    self.resolver = Resolver::for_expr(self.db.upcast(), def, val);
                    self.check_expr(val, ty);
                },
                | Stmt::Bind { .. } => unreachable!(),
            }
        }

        self.resolver = old_resolver;

        if diverges {
            never_ty
        } else {
            self.unit(src)
        }
    }

    pub fn infer_try(&mut self, stmts: &[Stmt], expr: ExprId) -> TyId {
        let src = self.source(expr);
        let def = self.resolver.body_owner().unwrap();
        let new_resolver = Resolver::for_expr(self.db.upcast(), def, expr);
        let old_resolver = std::mem::replace(&mut self.resolver, new_resolver);
        let try_class = self.lang_class("try-class");
        let ty_kind = self.type_kind(src);
        let kind = self.fn_type([ty_kind], ty_kind, src);
        let try_container = self.fresh_type_with_kind(kind, src);
        let last = stmts.len() - 1;
        let never_ty = self.lang_type("never-type", src);
        let mut diverges = false;

        self.constrain(
            CtntExpected::ExprOrPat(expr.into()),
            CtntFound::ExprOrPat(expr.into()),
            CtntInfo {
                class: try_class,
                types: [try_container].into(),
            },
        );

        for (i, stmt) in stmts.iter().enumerate() {
            match *stmt {
                | Stmt::Expr { expr } if i == last => {
                    let src = self.source(expr);
                    let val_ty = self.fresh_type(src);
                    let app = self.types.insert(TyInfo::App(try_container, [val_ty].into()), src);

                    self.resolver = Resolver::for_expr(self.db.upcast(), def, expr);
                    self.check_expr(expr, app);
                    return app;
                },
                | Stmt::Expr { expr } => {
                    self.resolver = Resolver::for_expr(self.db.upcast(), def, expr);

                    let ty = self.infer_expr(expr);
                    let ty = self.subst_type(ty);

                    if self.types[ty] == self.types[never_ty] {
                        diverges = true;
                    }
                },
                | Stmt::Bind { pat, val } => {
                    let in_ty = self.infer_pat(pat);
                    let val_src = self.source(val);
                    let app = self.types.insert(TyInfo::App(try_container, [in_ty].into()), val_src);

                    self.resolver = Resolver::for_expr(self.db.upcast(), def, val);
                    self.check_expr(val, app);
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
            never_ty
        } else {
            let unit = self.unit(src);

            self.types.insert(TyInfo::App(try_container, [unit].into()), src)
        }
    }

    pub fn check_expr(&mut self, expr: ExprId, expected: TyId) {
        let src = self.source(expr);
        let body = Arc::clone(&self.body);
        let expected = self.subst_type(expected);

        self.result.type_of_expr.insert(expr, expected);

        match (&body[expr], self.types[expected].clone()) {
            | (_, TyInfo::ForAll(kinds, inner, scope)) => {
                let sk = self.skolemize(&kinds, inner, scope);

                self.check_expr(expr, sk);
            },
            | (Expr::Path { .. }, TyInfo::Where(where_, inner)) => {
                for ctnt in where_.constraints.iter() {
                    self.class_env.push(ctnt.clone(), true);
                }

                self.check_expr(expr, inner);

                for _ in 0..where_.constraints.len() {
                    self.class_env.pop();
                }
            },
            | (_, TyInfo::Where(where_, inner)) => {
                for ctnt in where_.constraints.iter() {
                    self.class_env.push(ctnt.clone(), false);
                }

                self.check_expr(expr, inner);

                for _ in 0..where_.constraints.len() {
                    self.class_env.pop();
                }
            },
            | (Expr::Do { stmts }, _) => self.check_block(stmts, expected, expr),
            | (_, TyInfo::Unknown(_)) => {
                let infer = self.infer_expr(expr);

                if !self.unify_types(infer, expected) {
                    self.report_mismatch(expected, infer, expr);
                }
            },
            | (Expr::Typed { expr: inner, ty }, _) => self.owner.with_type_map(self.db.upcast(), |type_map| {
                let mut lcx = LowerCtx::new(type_map, self);
                let ty_ = lcx.lower_ty(*ty);
                let kind = lcx.infer_kind(expected);

                self.check_kind(ty_, kind);

                if !self.subsume_types(ty_, expected, expr.into()) {
                    self.report_mismatch(expected, ty_, expr);
                }

                self.check_expr(*inner, ty_);
            }),
            | (Expr::Path { path }, _) => match self.resolver.resolve_value_fully(self.db.upcast(), path) {
                | Some((res, vis)) => {
                    if path.segments().len() > 1
                        && !vis.is_visible_from(self.db.upcast(), self.resolver.module().unwrap())
                    {
                        self.report(InferenceDiagnostic::PrivateValue { id: expr.into() });
                    }

                    let ty = match res {
                        | ValueNs::Local(pat) => self.result.type_of_pat[pat],
                        | ValueNs::Fixity(_) => unimplemented!(),
                        | ValueNs::Func(id) => {
                            if self.owner == TypeVarOwner::DefWithBodyId(id.into()) {
                                self.icx
                                    .subst
                                    .subst_type(&mut self.icx.types, self.icx.result.self_type.ty)
                            } else {
                                self.db.value_ty(id.into()).ty.to_info(
                                    self.icx.db,
                                    &mut self.icx.types,
                                    &mut self.icx.type_vars,
                                    src,
                                )
                            }
                        },
                        | ValueNs::Static(id) => self.db.value_ty(id.into()).ty.to_info(
                            self.icx.db,
                            &mut self.icx.types,
                            &mut self.icx.type_vars,
                            src,
                        ),
                        | ValueNs::Const(id) => self.db.value_ty(id.into()).ty.to_info(
                            self.icx.db,
                            &mut self.icx.types,
                            &mut self.icx.type_vars,
                            src,
                        ),
                        | ValueNs::Ctor(id) => self.db.value_ty(id.into()).ty.to_info(
                            self.icx.db,
                            &mut self.icx.types,
                            &mut self.icx.type_vars,
                            src,
                        ),
                    };

                    if !self.subsume_types(ty, expected, expr.into()) {
                        self.report_mismatch(expected, ty, expr);
                    }
                },
                | None => {
                    let error = self.error(src);

                    self.report(InferenceDiagnostic::UnresolvedValue { id: expr.into() });
                    self.unify_types(expected, error);
                },
            },
            | (Expr::Lit { lit: Literal::Int(_) }, _) => {
                let integer = self.lang_class("integer-class");

                self.constrain(
                    CtntExpected::ExprOrPat(expr.into()),
                    CtntFound::ExprOrPat(expr.into()),
                    CtntInfo {
                        class: integer,
                        types: vec![expected].into(),
                    },
                );
            },
            | (Expr::Lit { lit: Literal::Float(_) }, _) => {
                let decimal = self.lang_class("decimal-class");

                self.constrain(
                    CtntExpected::ExprOrPat(expr.into()),
                    CtntFound::ExprOrPat(expr.into()),
                    CtntInfo {
                        class: decimal,
                        types: vec![expected].into(),
                    },
                );
            },
            | (Expr::App { base, arg }, _) => {
                let base_ty = self.infer_expr(*base);
                let ret = self.check_app(base_ty, *arg, expr);

                if !self.subsume_types(ret, expected, expr.into()) {
                    self.report_mismatch(expected, ret, expr);
                }
            },
            // | (Expr::Tuple { exprs }, TyInfo::Tuple(tys)) if exprs.len() == tys.len() => {
            //     for (&expr, &exp) in exprs.iter().zip(tys.iter()) {
            //         self.check_expr(expr, exp);
            //     }
            // },
            //             | (Expr::Clos { pats, stmts }, _) => {
            //                 use hir_def::id::HasModule;
            //                 let module = self.owner.module(self.db.upcast());
            //                 let block_ty = self.db.lang_item(module.lib, "block-type".into()).unwrap();
            //                 let block_ty = block_ty.as_type_ctor().unwrap();
            //
            //                 if let Some([f_ty, r_ty]) = expected.match_ctor(&self.types, block_ty).as_deref() {
            //                     let ret = self.fresh_type(src);
            //                     let args = pats.iter().map(|&p| self.infer_pat(p)).collect();
            //                     let ty = self.fn_type(args, ret, src);
            //
            //                     self.block_ret_type = Some(ret);
            //                     self.block_break_type = Some(*r_ty);
            //                     self.check_block(stmts, ret, expr.into());
            //                     self.block_ret_type = None;
            //                     self.block_break_type = None;
            //
            //                     if !self.unify_types(*f_ty, ty) {
            //                         self.report_mismatch(*f_ty, ty, expr);
            //                     }
            //                 } else {
            //                     let infer = self.infer_expr(expr);
            //
            //                     if !self.subsume_types(infer, expected, expr.into()) {
            //                         self.report_mismatch(expected, infer, expr);
            //                     }
            //                 }
            //             },
            | (_, _) => {
                let infer = self.infer_expr(expr);

                if !self.subsume_types(infer, expected, expr.into()) {
                    self.report_mismatch(expected, infer, expr);
                }
            },
        }
    }

    pub fn check_app(&mut self, base_ty: TyId, arg: ExprId, expr: ExprId) -> TyId {
        let src = self.source(expr);
        let base_ty = self.subst_type(base_ty);

        match self.types[base_ty].clone() {
            | TyInfo::ForAll(kinds, ty, scope) => {
                let repl = kinds
                    .iter()
                    .map(|&k| self.fresh_type_with_kind(k, src))
                    .collect::<Vec<_>>();
                let ty = ty.replace_vars(&mut self.types, &repl, scope);

                self.check_app(ty, arg, expr)
            },
            | TyInfo::Where(where_, ty) => {
                for ctnt in where_.constraints.iter() {
                    self.constrain(
                        CtntExpected::ExprOrPat(expr.into()),
                        CtntFound::ExprOrPat(expr.into()),
                        ctnt.clone(),
                    );
                }

                self.check_app(ty, arg, expr)
            },
            | _ => {
                let ret = self.fresh_type(src);
                let arg = self.infer_expr(arg);
                let func_ty = self.fn_type([arg], ret, src);

                if !self.unify_types(base_ty, func_ty) {
                    self.report_mismatch(base_ty, func_ty, expr);
                }

                ret
            },
        }
    }

    pub fn check_block(&mut self, stmts: &[Stmt], expected: TyId, expr: ExprId) {
        let src = self.source(expr);
        let def = self.resolver.body_owner().unwrap();
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

        let unit = self.unit(src);

        if !self.unify_types(expected, unit) {
            self.report_mismatch(expected, unit, expr);
        }

        self.resolver = old_resolver;
    }
}

impl InferenceContext<'_> {
    fn infer_path(&mut self, path: &Path, resolver: Resolver, expr: ExprId, idx: Option<usize>) -> TyId {
        let src = self.source((expr, idx.unwrap_or(0)));

        match resolver.resolve_value_fully(self.db.upcast(), path) {
            | Some((value, vis)) => 't: {
                if path.segments().len() > 1 && !vis.is_visible_from(self.db.upcast(), self.resolver.module().unwrap())
                {
                    self.report(InferenceDiagnostic::PrivateValue { id: expr.into() });
                }

                let id = match value {
                    | ValueNs::Local(pat) => break 't self.result.type_of_pat[pat],
                    | ValueNs::Fixity(id) => break 't self.infer_infix(id, expr.into()),
                    | ValueNs::Func(id) => {
                        if self.owner == TypeVarOwner::DefWithBodyId(id.into()) {
                            break 't self.subst.subst_type(&mut self.types, self.result.self_type.ty);
                        } else {
                            id.into()
                        }
                    },
                    | ValueNs::Static(id) => id.into(),
                    | ValueNs::Const(id) => id.into(),
                    | ValueNs::Ctor(id) => id.into(),
                };

                let ty = self.db.value_ty(id).ty;
                let ty = ty.to_info(self.db, &mut self.types, &mut self.type_vars, src);
                let ty = self.instantiate(ty, (expr, idx.unwrap_or(0)).into());

                self.result.type_of_expr.insert(expr, ty);

                return ty;
            },
            | None => {
                self.report(InferenceDiagnostic::UnresolvedValue { id: expr.into() });
                self.error(src)
            },
        }
    }

    pub fn infer_infix(&mut self, id: FixityId, origin: ExprOrPatId) -> TyId {
        let src = self.source(origin);
        let data = self.db.fixity_data(id);
        let resolver = id.resolver(self.db.upcast());
        let id = match resolver.resolve_value_fully(self.db.upcast(), &data.func) {
            | Some((ValueNs::Func(id), vis)) => {
                if data.func.segments().len() > 1 && !vis.is_visible_from(self.db.upcast(), resolver.module().unwrap())
                {
                    self.report(InferenceDiagnostic::PrivateValue { id: origin });
                }

                id.into()
            },
            | Some((ValueNs::Ctor(id), vis)) => {
                if data.func.segments().len() > 1 && !vis.is_visible_from(self.db.upcast(), resolver.module().unwrap())
                {
                    self.report(InferenceDiagnostic::PrivateValue { id: origin });
                }

                id.into()
            },
            | _ => return self.error(src),
        };

        let ty = self
            .db
            .value_ty(id)
            .ty
            .to_info(self.db, &mut self.types, &mut self.type_vars, src);

        self.instantiate(ty, origin)
    }
}
