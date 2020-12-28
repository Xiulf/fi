use crate::ctx::*;
use crate::error::*;
use crate::ty::*;
use hir::ir;

impl<'db> Ctx<'db> {
    crate fn infer_body(&mut self, span: ir::Span, body: &ir::Body) -> Result<Ty> {
        let params = Ty::tuple(
            span,
            body.params.iter().map(|p| {
                let ty_kind = self.ty_kind(p.span);
                let ty = self.fresh_type_with_kind(p.span, ty_kind);

                self.tys.insert(p.id, ty.clone());
                ty
            }),
        );

        let val_ty = self.infer_expr(&body.value)?;
        let func_ty = self.db.lang_items().fn_ty();
        let func_ty = Ty::ctor(span, func_ty.owner);

        Ok(Ty::app(span, func_ty, List::from([params, val_ty])))
    }

    crate fn infer_pat(&mut self, pat: &ir::Pat) -> Result<Ty> {
        let ty = match &pat.kind {
            ir::PatKind::Error => Ty::error(pat.span),
            ir::PatKind::Wildcard => self.fresh_type_with_kind(pat.span, self.ty_kind(pat.span)),
            ir::PatKind::Int { .. } => {
                let ty_kind = self.ty_kind(pat.span);
                let ty = self.fresh_type_with_kind(pat.span, ty_kind);
                let _ctnt = Ctnt {
                    trait_: self.db.lang_items().integer_trait().owner,
                    tys: List::from([ty.clone()]),
                };

                ty
            }
            ir::PatKind::Float { .. } => {
                let ty_kind = self.ty_kind(pat.span);
                let ty = self.fresh_type_with_kind(pat.span, ty_kind);
                let _ctnt = Ctnt {
                    trait_: self.db.lang_items().decimal_trait().owner,
                    tys: List::from([ty.clone()]),
                };

                ty
            }
            ir::PatKind::Char { .. } => {
                let char_ty = self.db.lang_items().char();

                Ty::ctor(pat.span, char_ty.owner)
            }
            ir::PatKind::Bind { sub: None, .. } => {
                self.fresh_type_with_kind(pat.span, self.ty_kind(pat.span))
            }
            _ => unimplemented!("infer {:?}", pat),
        };

        self.tys.insert(pat.id, ty.clone());

        Ok(ty)
    }

    crate fn infer_expr(&mut self, expr: &ir::Expr) -> Result<Ty> {
        let ty = match &expr.kind {
            ir::ExprKind::Error => Ty::error(expr.span),
            ir::ExprKind::Ident { res } => match res {
                ir::Res::Error => Ty::error(expr.span),
                ir::Res::Local(id) => {
                    let ty = self.tys[id].clone();
                    let ty = self.introduce_skolem_scope(ty);

                    if let Type::Ctnt(_ctnt, _ty) = &*ty {
                        todo!();
                        // ty.clone()
                    } else {
                        ty
                    }
                }
                ir::Res::Def(ir::DefKind::Ctor, id) => {
                    let ty = self.db.typecheck(*id).ty.clone();
                    let ty = self.instantiate(ty);

                    self.introduce_skolem_scope(ty)
                }
                ir::Res::Def(_, id) => {
                    let ty = if let Some(ty) = self.tys.get(&ir::HirId {
                        owner: *id,
                        local_id: ir::LocalId(0),
                    }) {
                        ty.clone()
                    } else {
                        self.db.typecheck(*id).ty.clone()
                    };

                    let ty = self.introduce_skolem_scope(ty);

                    if let Type::Ctnt(_ctnt, _ty) = &*ty {
                        todo!();
                        // ty.clone()
                    } else {
                        ty
                    }
                }
            },
            ir::ExprKind::Int { .. } => {
                let ty_kind = self.ty_kind(expr.span);
                let ty = self.fresh_type_with_kind(expr.span, ty_kind);
                let _ctnt = Ctnt {
                    trait_: self.db.lang_items().integer_trait().owner,
                    tys: List::from([ty.clone()]),
                };

                ty
            }
            ir::ExprKind::Float { .. } => {
                let ty_kind = self.ty_kind(expr.span);
                let ty = self.fresh_type_with_kind(expr.span, ty_kind);
                let _ctnt = Ctnt {
                    trait_: self.db.lang_items().decimal_trait().owner,
                    tys: List::from([ty.clone()]),
                };

                ty
            }
            ir::ExprKind::Char { .. } => {
                let char_ty = self.db.lang_items().char();

                Ty::ctor(expr.span, char_ty.owner)
            }
            ir::ExprKind::Tuple { exprs } => {
                let tys = exprs
                    .iter()
                    .map(|e| self.infer_expr(e))
                    .collect::<Result<List<_>>>()?;

                Ty::tuple(expr.span, tys)
            }
            ir::ExprKind::Array { exprs } => {
                let tys = exprs
                    .iter()
                    .map(|e| self.infer_expr(e))
                    .collect::<Result<List<_>>>()?;

                let len = Ty::int(expr.span, exprs.len() as u128);
                let ty_kind = self.ty_kind(expr.span);
                let el = self.fresh_type_with_kind(expr.span, ty_kind);

                tys.iter().try_for_each(|t| {
                    let t = self.instantiate(t.clone());

                    self.unify_types(el.clone(), t.clone())?;
                    self.tys.insert(expr.id, t);

                    Ok(())
                })?;

                let array_ty = self.array_ty(expr.span);

                Ty::app(expr.span, array_ty, List::from([el, len]))
            }
            ir::ExprKind::Record { fields } => {
                // self.ensure_no_duplicate_props(fields);
                let fields = fields
                    .iter()
                    .map(|f| {
                        let ty = self.infer_expr(&f.val)?;

                        Ok(Field {
                            span: f.span,
                            name: f.name.symbol,
                            ty: if let ir::ExprKind::Ident { .. } = f.val.kind {
                                self.instantiate(ty)
                            } else {
                                ty
                            },
                        })
                    })
                    .collect::<Result<List<_>>>()?;

                let row = Ty::row(expr.span, fields, None);
                let record_ty = self.record_ty(expr.span);

                Ty::app(expr.span, record_ty, List::from([row]))
            }
            ir::ExprKind::Field { base, field } => {
                let ty_kind = self.ty_kind(expr.span);
                let row_kind = self.row_kind(expr.span);
                let row_kind = Ty::app(expr.span, row_kind, List::from([ty_kind.clone()]));
                let ret = self.fresh_type_with_kind(expr.span, ty_kind);
                let tail = self.fresh_type_with_kind(expr.span, row_kind);
                let row = Ty::row(
                    expr.span,
                    List::from([Field {
                        span: field.span,
                        name: field.symbol,
                        ty: ret.clone(),
                    }]),
                    tail,
                );

                let record_ty = self.record_ty(expr.span);
                let ty = Ty::app(expr.span, record_ty, List::from([row]));

                self.check_expr(base, ty)?;

                ret
            }
            ir::ExprKind::Case { pred, arms } => {
                let ts = self.instantiate_for_binders(pred, arms);
                let ty_kind = self.ty_kind(expr.span);
                let ret = self.fresh_type_with_kind(expr.span, ty_kind);

                self.check_binders(ts, ret.clone(), arms)?;

                ret
            }
            ir::ExprKind::Hole { name } => {
                let ty_kind = self.ty_kind(expr.span);
                let ty = self.fresh_type_with_kind(expr.span, ty_kind);

                self.errors.push(TypeError::HoleType(*name, ty.clone()));

                ty
            }
            _ => unimplemented!("infer {:?}", expr),
        };

        self.tys.insert(expr.id, ty.clone());

        Ok(ty)
    }
}
