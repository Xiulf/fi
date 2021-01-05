use crate::ctx::*;
use crate::error::*;
use crate::ty::*;
use hir::ir;

impl<'db> Ctx<'db> {
    crate fn infer_body(&mut self, span: ir::Span, body: &ir::Body) -> Result<Ty> {
        let params = Ty::tuple(
            span,
            self.file,
            body.params.iter().map(|p| {
                let ty_kind = self.ty_kind(p.span, self.file);
                let ty = self.fresh_type_with_kind(p.span, self.file, ty_kind);

                self.tys.insert(p.id, ty.clone());
                ty
            }),
        );

        let val_ty = self.infer_expr(&body.value)?;
        let func_ty = self.db.lang_items().fn_ty();
        let func_ty = Ty::ctor(span, self.file, func_ty.owner);

        Ok(Ty::app(
            span,
            self.file,
            func_ty,
            List::from([params, val_ty]),
        ))
    }

    crate fn infer_pat(&mut self, pat: &ir::Pat) -> Result<Ty> {
        let ty = match &pat.kind {
            ir::PatKind::Error => Ty::error(pat.span, self.file),
            ir::PatKind::Wildcard => {
                self.fresh_type_with_kind(pat.span, self.file, self.ty_kind(pat.span, self.file))
            }
            ir::PatKind::Int { .. } => {
                let ty_kind = self.ty_kind(pat.span, self.file);
                let ty = self.fresh_type_with_kind(pat.span, self.file, ty_kind);
                let _ctnt = Ctnt {
                    span: pat.span,
                    file: self.file,
                    trait_: self.db.lang_items().integer_trait().owner,
                    tys: List::from([ty.clone()]),
                };

                ty
            }
            ir::PatKind::Float { .. } => {
                let ty_kind = self.ty_kind(pat.span, self.file);
                let ty = self.fresh_type_with_kind(pat.span, self.file, ty_kind);
                let _ctnt = Ctnt {
                    span: pat.span,
                    file: self.file,
                    trait_: self.db.lang_items().decimal_trait().owner,
                    tys: List::from([ty.clone()]),
                };

                ty
            }
            ir::PatKind::Char { .. } => {
                let char_ty = self.db.lang_items().char();

                Ty::ctor(pat.span, self.file, char_ty.owner)
            }
            ir::PatKind::Bind { sub: None, .. } => {
                self.fresh_type_with_kind(pat.span, self.file, self.ty_kind(pat.span, self.file))
            }
            ir::PatKind::Typed { pat, ty } => {
                let ty = self.hir_ty(ty);
                let (elab_ty, kind) = self.kind_of(ty)?;
                let _ = self.check_type_kind(kind)?;
                let ty1 = self.introduce_skolem_scope(elab_ty);
                let ty2 = self.infer_pat(pat)?;
                let _ = self.unify_types(ty1.clone(), ty2)?;

                ty1
            }
            _ => unimplemented!("infer {:?}", pat),
        };

        self.tys.insert(pat.id, ty.clone());

        Ok(ty)
    }

    crate fn infer_expr(&mut self, expr: &ir::Expr) -> Result<Ty> {
        let ty = match &expr.kind {
            ir::ExprKind::Error => Ty::error(expr.span, self.file),
            ir::ExprKind::Ident { res, .. } => match res {
                ir::Res::Error => Ty::error(expr.span, self.file),
                ir::Res::Local(id) => {
                    let ty = self.tys[id].clone();
                    let ty = self.introduce_skolem_scope(ty);

                    (if let Type::Ctnt(ctnt, ty) = &*ty {
                        self.ctnts.push((
                            expr.id,
                            ctnt.clone() ^ (expr.span, self.file),
                            self.ctnt_ctx.clone(),
                        ));

                        ty.clone()
                    } else {
                        ty
                    }) ^ (expr.span, self.file)
                }
                ir::Res::Def(ir::DefKind::Ctor, id) => {
                    let ty = self.db.typecheck(*id).ty.clone();
                    let ty = self.instantiate(expr.id, ty);

                    self.introduce_skolem_scope(ty) ^ (expr.span, self.file)
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

                    (if let Type::Ctnt(ctnt, ty) = &*ty {
                        self.ctnts.push((
                            expr.id,
                            ctnt.clone() ^ (expr.span, self.file),
                            self.ctnt_ctx.clone(),
                        ));

                        ty.clone()
                    } else {
                        ty
                    }) ^ (expr.span, self.file)
                }
            },
            ir::ExprKind::Int { .. } => {
                let ty_kind = self.ty_kind(expr.span, self.file);
                let ty = self.fresh_type_with_kind(expr.span, self.file, ty_kind);
                let ctnt = Ctnt {
                    span: expr.span,
                    file: self.file,
                    trait_: self.db.lang_items().integer_trait().owner,
                    tys: List::from([ty.clone()]),
                };

                self.ctnts.push((expr.id, ctnt, Vec::new()));

                ty
            }
            ir::ExprKind::Float { .. } => {
                let ty_kind = self.ty_kind(expr.span, self.file);
                let ty = self.fresh_type_with_kind(expr.span, self.file, ty_kind);
                let ctnt = Ctnt {
                    span: expr.span,
                    file: self.file,
                    trait_: self.db.lang_items().decimal_trait().owner,
                    tys: List::from([ty.clone()]),
                };

                self.ctnts.push((expr.id, ctnt, Vec::new()));

                ty
            }
            ir::ExprKind::Char { .. } => {
                let char_ty = self.db.lang_items().char();

                Ty::ctor(expr.span, self.file, char_ty.owner)
            }
            ir::ExprKind::Tuple { exprs } => {
                let tys = exprs
                    .iter()
                    .map(|e| self.infer_expr(e))
                    .collect::<Result<List<_>>>()?;

                Ty::tuple(expr.span, self.file, tys)
            }
            ir::ExprKind::Array { exprs } => {
                let tys = exprs
                    .iter()
                    .map(|e| self.infer_expr(e))
                    .collect::<Result<List<_>>>()?;

                let len = Ty::int(expr.span, self.file, exprs.len() as u128);
                let ty_kind = self.ty_kind(expr.span, self.file);
                let el = self.fresh_type_with_kind(expr.span, self.file, ty_kind);

                tys.iter().zip(exprs).try_for_each(|(t, e)| {
                    let t = self.instantiate(e.id, t.clone());

                    self.unify_types(el.clone(), t.clone())?;
                    self.tys.insert(e.id, t);

                    Ok(())
                })?;

                let array_ty = self.array_ty(expr.span, self.file);

                Ty::app(expr.span, self.file, array_ty, List::from([el, len]))
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
                                self.instantiate(f.val.id, ty)
                            } else {
                                ty
                            },
                        })
                    })
                    .collect::<Result<List<_>>>()?;

                let row = Ty::row(expr.span, self.file, fields, None);
                let record_ty = self.record_ty(expr.span, self.file);

                Ty::app(expr.span, self.file, record_ty, List::from([row]))
            }
            ir::ExprKind::Field { base, field } => {
                let ty_kind = self.ty_kind(expr.span, self.file);
                let row_kind = self.row_kind(expr.span, self.file);
                let row_kind = Ty::app(
                    expr.span,
                    self.file,
                    row_kind,
                    List::from([ty_kind.clone()]),
                );
                let ret = self.fresh_type_with_kind(expr.span, self.file, ty_kind);
                let tail = self.fresh_type_with_kind(expr.span, self.file, row_kind);
                let row = Ty::row(
                    expr.span,
                    self.file,
                    List::from([Field {
                        span: field.span,
                        name: field.symbol,
                        ty: ret.clone(),
                    }]),
                    tail,
                );

                let record_ty = self.record_ty(expr.span, self.file);
                let ty = Ty::app(expr.span, self.file, record_ty, List::from([row]));

                self.check_expr(base, ty)?;

                ret
            }
            ir::ExprKind::Index { base, index } => {
                let ty_kind = self.ty_kind(expr.span, self.file);
                let figure_kind = self.figure_kind(expr.span, self.file);
                let elem = self.fresh_type_with_kind(expr.span, self.file, ty_kind);
                let len = self.fresh_type_with_kind(expr.span, self.file, figure_kind);
                let arr_ty = self.array_ty(base.span, self.file);
                let arr_ty = Ty::app(
                    base.span,
                    self.file,
                    arr_ty,
                    List::from([elem.clone(), len]),
                );

                let uint_ty = self.db.lang_items().uint();
                let uint_ty = self.db.typecheck(uint_ty.owner).ty.clone();

                self.check_expr(index, uint_ty)?;
                self.check_expr(base, arr_ty)?;

                elem
            }
            ir::ExprKind::App { base, args } => {
                let base_ty = self.infer_expr(base)?;

                self.check_func_app(base.id, base_ty, args)?
            }
            ir::ExprKind::Do { .. } => {
                let ty_kind = self.ty_kind(expr.span, self.file);
                let ty = self.fresh_type_with_kind(expr.span, self.file, ty_kind);

                self.check_expr(expr, ty.clone())?;

                ty
            }
            ir::ExprKind::If { cond, then, else_ } => {
                let bool_ty = self.db.lang_items().bool();
                let bool_ty = Ty::ctor(cond.span, self.file, bool_ty.owner);
                let ty_kind = self.ty_kind(expr.span, self.file);
                let ret = self.fresh_type_with_kind(expr.span, self.file, ty_kind);

                self.check_expr(cond, bool_ty)?;
                self.check_expr(then, ret.clone())?;
                self.check_expr(else_, ret.clone())?;

                ret
            }
            ir::ExprKind::Case { pred, arms } => {
                let ts = self.instantiate_for_binders(pred, arms)?;
                let ty_kind = self.ty_kind(expr.span, self.file);
                let ret = self.fresh_type_with_kind(expr.span, self.file, ty_kind);

                self.check_binders(ts, ret.clone(), arms)?;

                ret
            }
            ir::ExprKind::Typed { expr, ty } => {
                let ty = self.hir_ty(ty);
                let (elab_ty, kind) = self.kind_of(ty)?;
                let ty = self.introduce_skolem_scope(elab_ty);

                self.check_type_kind(kind)?;
                self.check_expr(expr, ty.clone())?;

                ty
            }
            ir::ExprKind::Hole { name } => {
                let ty_kind = self.ty_kind(expr.span, self.file);
                let ty = self.fresh_type_with_kind(expr.span, self.file, ty_kind);

                self.errors.push(TypeError::HoleType(*name, ty.clone()));

                ty
            }
            _ => unimplemented!("infer {:?}", expr),
        };

        self.tys.insert(expr.id, ty.clone());

        Ok(ty)
    }

    crate fn infer_stmt(&mut self, stmt: &ir::Stmt) -> Result<Ty> {
        match &stmt.kind {
            ir::StmtKind::Bind { binding } => {
                let ty = self.hir_ty(&binding.ty);

                self.check_pat(&binding.pat, ty.clone())?;
                self.check_expr(&binding.val, ty)?;

                Ok(Ty::tuple(stmt.span, self.file, List::empty()))
            }
            ir::StmtKind::Discard { expr } => self.infer_expr(expr),
        }
    }

    crate fn instantiate_for_binders(
        &mut self,
        preds: &[ir::Expr],
        arms: &[ir::CaseArm],
    ) -> Result<List<Ty>> {
        let inst = arms
            .iter()
            .map(|a| {
                a.pats
                    .iter()
                    .map(|p| self.pat_requires_monotype(p))
                    .collect::<Vec<_>>()
            })
            .fold(vec![false; preds.len()], |acc, p| {
                acc.into_iter().zip(p).map(|(a, b)| a || b).collect()
            });

        preds
            .iter()
            .zip(inst)
            .map(|(pred, inst)| {
                let ty = self.infer_expr(pred)?;

                if inst {
                    Ok(self.instantiate(pred.id, ty))
                } else {
                    Ok(ty)
                }
            })
            .collect()
    }

    crate fn pat_requires_monotype(&mut self, pat: &ir::Pat) -> bool {
        match &pat.kind {
            ir::PatKind::Wildcard => false,
            ir::PatKind::Bind { sub: None, .. } => false,
            ir::PatKind::Bind { sub: Some(sub), .. } => self.pat_requires_monotype(sub),
            ir::PatKind::Typed { pat, ty } => {
                self.is_mono_type(ty) || self.pat_requires_monotype(pat)
            }
            _ => true,
        }
    }

    crate fn is_mono_type(&mut self, ty: &ir::Type) -> bool {
        self.hir_ty(ty).is_mono_type()
    }
}
