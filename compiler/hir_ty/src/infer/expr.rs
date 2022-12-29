use std::sync::Arc;

use hir_def::expr::{CaseArm, CaseValue, Expr, ExprId, Literal, Stmt};
use hir_def::id::{FixityId, TypeVarOwner};
use hir_def::infix::ProcessInfix;
use hir_def::lang_item;
use hir_def::path::Path;
use hir_def::resolver::{HasResolver, Resolver, ValueNs};
use tracing::trace;

use super::diagnostics::{CtntExpected, CtntFound};
use super::{BodyInferenceContext, ExprOrPatId, InferenceContext, InferenceDiagnostic};
use crate::info::{CtntInfo, FieldInfo, ToInfo, TyId, TyInfo, TySource};
use crate::lower::LowerCtx;

impl BodyInferenceContext<'_> {
    pub fn infer_expr(&mut self, expr: ExprId) -> TyId {
        self.db.unwind_if_cancelled();
        trace!("infer_expr({:?})", expr);

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
            | Expr::Hole => {
                let ty = self.fresh_type(src);

                self.report(InferenceDiagnostic::ValueHole {
                    id: expr,
                    ty,
                    search: Default::default(),
                });

                ty
            },
            | Expr::Unit => self.unit(src),
            | Expr::Path { path } => return self.icx.infer_path(path, self.resolver.clone(), expr, None),
            | Expr::Lit { lit } => match lit {
                | Literal::Int(_) => {
                    let integer = self.lang_class(lang_item::INTEGER_CLASS);
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
                    let decimal = self.lang_class(lang_item::DECIMAL_CLASS);
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
                | Literal::Char(_) => self.lang_type(lang_item::CHAR_TYPE, src),
                | Literal::String(_) => self.lang_type(lang_item::STR_TYPE, src),
            },
            | Expr::Infix { exprs, ops } => {
                let exprs = exprs.iter().map(|&e| self.infer_expr(e)).collect::<Vec<_>>();

                self.process_infix(
                    exprs.into_iter(),
                    ops,
                    src,
                    |ctx, i, op, lhs, rhs| {
                        let mid = ctx.check_app(op, lhs, (expr, i));

                        ctx.check_app(mid, rhs, (expr, i))
                    },
                    |ctx, i, path, resolver| ctx.infer_path(path, resolver, expr, Some(i)),
                )
            },
            | Expr::App { base, arg } => {
                let base_ty = self.infer_expr(*base);

                self.infer_app(base_ty, *arg, expr)
            },
            | Expr::Field { base, field } => {
                let row_kind = self.lang_type(lang_item::ROW_KIND, src);
                let type_kind = self.type_kind(src);
                let record_type = self.lang_type(lang_item::RECORD_TYPE, src);
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
                let record_type = self.lang_type(lang_item::RECORD_TYPE, src);
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
                let array_type = self.lang_type(lang_item::ARRAY_TYPE, src);
                let len = self.types.insert(TyInfo::Figure(exprs.len() as i128), src);
                let elem_ty = self.fresh_type(src);

                for &expr in exprs.iter() {
                    self.check_expr(expr, elem_ty);
                }

                self.types.insert(TyInfo::App(array_type, [len, elem_ty].into()), src)
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
                let bool_type = self.lang_type(lang_item::BOOL_TYPE, cond_src);

                self.check_expr(*cond, bool_type);

                if let Some(else_) = else_ {
                    let _else_src = self.source(*else_);
                    let then_ty = self.infer_expr(*then);
                    let then_ty = self.subst_type(then_ty);
                    let else_ty = self.infer_expr(*else_);
                    let else_ty = self.subst_type(else_ty);
                    let never_ty = self.lang_type(lang_item::NEVER_TYPE, then_src);

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
            | Expr::Case { pred, arms } => self.infer_case(src, *pred, arms),
            | Expr::Recur => match self.lambda_type.last() {
                | Some(lambda_type) => *lambda_type,
                | None => todo!(),
            },
            | Expr::Return { expr: inner } => {
                let ret = self.ret_type;

                self.check_expr(*inner, ret);
                self.lang_type(lang_item::NEVER_TYPE, src)
            },
            | e => unimplemented!("{:?}", e),
        };

        // eprintln!("{:?} :: {}", body[expr], ty.display(self.db));

        self.result.type_of_expr.insert(expr, ty);
        ty
    }

    pub fn infer_case(&mut self, src: TySource, pred: ExprId, arms: &[CaseArm]) -> TyId {
        let pred_ty = self.infer_expr(pred);
        let res = self.fresh_type(src);

        for arm in arms.iter() {
            self.check_pat(arm.pat, pred_ty);

            match arm.value {
                | CaseValue::Normal(expr) => {
                    self.with_expr_scope(expr, |ctx| {
                        let ty = ctx.infer_expr(expr);
                        let ty = ctx.subst_type(ty);

                        if !ctx.subsume_types(ty, res, expr.into()) {
                            ctx.report_mismatch(res, ty, expr);
                        }
                    });
                },
                | CaseValue::Guarded(ref guards, ref exprs) => {
                    for &guard in guards.iter() {
                        self.with_expr_scope(guard, |ctx| {
                            let bool_src = ctx.source(guard);
                            let bool_ty = ctx.lang_type(lang_item::BOOL_TYPE, bool_src);

                            ctx.check_expr(guard, bool_ty);
                        });
                    }

                    for &expr in exprs.iter() {
                        self.with_expr_scope(expr, |ctx| {
                            let ty = ctx.infer_expr(expr);
                            let ty = ctx.subst_type(ty);

                            if !ctx.subsume_types(ty, res, expr.into()) {
                                ctx.report_mismatch(res, ty, expr);
                            }
                        });
                    }
                },
            }
        }

        res
    }

    pub fn infer_block(&mut self, stmts: &[Stmt], expr: ExprId) -> TyId {
        let src = self.source(expr);

        if stmts.is_empty() {
            return self.fresh_type(src);
        }

        let def = self.resolver.body_owner().unwrap();
        let new_resolver = Resolver::for_expr(self.db.upcast(), def, expr);
        let old_resolver = std::mem::replace(&mut self.resolver, new_resolver);
        let last = stmts.len() - 1;
        let never_ty = self.lang_type(lang_item::NEVER_TYPE, src);
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
                    self.check_expr(val, ty)
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
        let try_class = self.lang_class(lang_item::TRY_CLASS);
        let ty_kind = self.type_kind(src);
        let kind = self.fn_type([ty_kind], ty_kind, src);
        let try_container = self.fresh_type_with_kind(kind, src);
        let last = stmts.len() - 1;
        let never_ty = self.lang_type(lang_item::NEVER_TYPE, src);
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
                    let src = self.source(expr);
                    let ty = self.fresh_type(src);
                    let app = self.types.insert(TyInfo::App(try_container, [ty].into()), src);

                    self.resolver = Resolver::for_expr(self.db.upcast(), def, expr);
                    self.check_expr(expr, app);

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
                let integer = self.lang_class(lang_item::INTEGER_CLASS);

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
                let decimal = self.lang_class(lang_item::DECIMAL_CLASS);

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
                let ret = self.infer_app(base_ty, *arg, expr);

                if !self.subsume_types(ret, expected, expr.into()) {
                    self.report_mismatch(expected, ret, expr);
                }
            },
            | (_, _) => {
                let infer = self.infer_expr(expr);
                let infer = self.subst_type(infer);
                // log::debug!("{}", std::panic::Location::caller());

                if !self.subsume_types(infer, expected, expr.into()) {
                    self.report_mismatch(expected, infer, expr);
                }
            },
        }
    }

    pub fn infer_app(&mut self, base_ty: TyId, arg: ExprId, expr: ExprId) -> TyId {
        let base_ty = self.subst_type(base_ty);
        let func_ty = self.lang_ctor(lang_item::FN_TYPE);

        if let Some(&[arg_ty, ret_ty]) = base_ty.match_ctor(&self.types, func_ty).as_deref() {
            self.check_expr(arg, arg_ty);
            return ret_ty;
        }

        let arg = self.infer_expr(arg);

        self.check_app(base_ty, arg, expr)
    }

    pub fn check_app(&mut self, base_ty: TyId, arg: TyId, expr: impl Into<ExprOrPatId> + Copy) -> TyId {
        let src = self.source(expr.into());
        let base_ty = self.subst_type(base_ty);
        // log::debug!("{}", base_ty.display(self.db, &self.types));

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
                let func_ty = self.fn_type([arg], ret, src);
                // log::debug!("{}", std::panic::Location::caller());
                // log::debug!(
                //     "{} == {}",
                //     base_ty.display(self.db, &self.types),
                //     func_ty.display(self.db, &self.types)
                // );

                if !self.unify_types(base_ty, func_ty) {
                    // log::debug!("mismatch");
                    self.report_mismatch(base_ty, func_ty, expr);
                }

                ret
            },
        }
    }

    pub fn check_block(&mut self, stmts: &[Stmt], expected: TyId, expr: ExprId) {
        if stmts.is_empty() {
            return;
        }

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
                    self.check_expr(val, ty)
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
        let ty = match resolver.resolve_value_fully(self.db.upcast(), path) {
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
                            self.can_generalize = false;
                            // break 't self.subst.subst_type(&mut self.types, self.result.self_type.ty);
                            break 't self.instantiate(self.result.self_type.ty, (expr, idx.unwrap_or(0)).into());
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

                self.result.type_of_expr.insert(expr, ty);

                return self.instantiate(ty, (expr, idx.unwrap_or(0)).into());
            },
            | None => {
                let ty = self.error(src);

                self.report(InferenceDiagnostic::UnresolvedValue { id: expr.into() });
                ty
            },
        };

        self.result.type_of_expr.insert(expr, ty);
        ty
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
