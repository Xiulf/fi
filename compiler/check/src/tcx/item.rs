use crate::constraint::Constraint;
use crate::tcx::Tcx;
use crate::ty::*;

impl<'tcx> Tcx<'tcx> {
    pub fn infer_item(&self, id: &hir::Id) -> Ty<'tcx> {
        let item = &self.package.items[id];

        match &item.kind {
            hir::ItemKind::Extern { abi: _, ty } => self.type_of(ty),
            hir::ItemKind::Func {
                generics,
                params,
                ret,
                body: _,
            } => {
                let params = self.intern.intern_param_list(
                    &params
                        .iter()
                        .map(|id| {
                            let param = &self.package.items[id];

                            Param {
                                span: param.span,
                                name: param.name,
                                ty: self.type_of(id),
                            }
                        })
                        .collect::<Vec<_>>(),
                );

                let ret = self.type_of(ret);
                let mut ty = self.intern_ty(Type::Func(Some(*id), params, ret));

                if !generics.params.is_empty() {
                    let args = generics.params.iter().map(|g| g.id).collect::<Vec<_>>();
                    let args = self.intern.intern_id_list(&args);

                    ty = self.intern_ty(Type::Forall(args, ty));
                }

                ty
            }
            hir::ItemKind::Method {
                owner,
                generics,
                params,
                ret,
                body: _,
            } => {
                let params = self.intern.intern_param_list(
                    &std::iter::once(Param {
                        span: item.name.span,
                        name: Ident {
                            symbol: hir::Symbol::dummy(),
                            span: item.name.span,
                        },
                        ty: self.type_of(owner),
                    })
                    .chain(params.iter().map(|id| {
                        let param = &self.package.items[id];

                        Param {
                            span: param.span,
                            name: param.name,
                            ty: self.type_of(id),
                        }
                    }))
                    .collect::<Vec<_>>(),
                );

                let ret = self.type_of(ret);
                let mut ty = self.intern_ty(Type::Func(Some(*id), params, ret));

                if !generics.params.is_empty() {
                    let args = generics.params.iter().map(|g| g.id).collect::<Vec<_>>();
                    let args = self.intern.intern_id_list(&args);

                    ty = self.intern_ty(Type::Forall(args, ty));
                }

                ty
            }
            hir::ItemKind::Param { ty } => self.type_of(ty),
            hir::ItemKind::Var { ty, .. } => self.type_of(ty),
            hir::ItemKind::Const { ty, .. } => self.type_of(ty),
            hir::ItemKind::Struct { .. } => self.intern_ty(Type::TypeOf(*id, List::empty())),
            hir::ItemKind::Enum { .. } => self.intern_ty(Type::TypeOf(*id, List::empty())),
            hir::ItemKind::Ctor {
                item,
                variant: _,
                params: Some(params),
            } => {
                let params = self.intern.intern_param_list(
                    &params
                        .iter()
                        .map(|field| Param {
                            span: field.span,
                            name: field.name,
                            ty: self.type_of(&field.ty),
                        })
                        .collect::<Vec<_>>(),
                );

                let ret = self.type_of(item);

                if let Type::Forall(args, ret) = ret {
                    let ty = self.intern_ty(Type::Func(Some(*id), params, ret));

                    self.intern_ty(Type::Forall(*args, ty))
                } else {
                    self.intern_ty(Type::Func(Some(*id), params, ret))
                }
            }
            hir::ItemKind::Ctor {
                item,
                variant: _,
                params: None,
            } => self.type_of(item),
        }
    }

    pub fn check_item(&self, id: &hir::Id) {
        let item = &self.package.items[id];

        match &item.kind {
            hir::ItemKind::Func { ret, body, .. } => {
                let body_ty = self.infer_block(body);
                let ret_ty = self.type_of(ret);
                let ret_span = self.span_of(ret);

                self.constrain(Constraint::Equal(body_ty, body.span, ret_ty, ret_span));
            }
            hir::ItemKind::Method { ret, body, .. } => {
                let body_ty = self.infer_block(body);
                let ret_ty = self.type_of(ret);
                let ret_span = self.span_of(ret);

                self.constrain(Constraint::Equal(body_ty, body.span, ret_ty, ret_span));
            }
            hir::ItemKind::Var {
                ty, val: Some(val), ..
            } => {
                let val_ty = self.type_of(val);
                let val_span = self.span_of(val);
                let ty_ty = self.type_of(ty);
                let ty_span = self.span_of(ty);

                self.constrain(Constraint::Equal(val_ty, val_span, ty_ty, ty_span));
            }
            hir::ItemKind::Const { ty, val } => {
                let val_ty = self.type_of(val);
                let val_span = self.span_of(val);
                let ty_ty = self.type_of(ty);
                let ty_span = self.span_of(ty);

                self.constrain(Constraint::Equal(val_ty, val_span, ty_ty, ty_span));
            }
            hir::ItemKind::Struct {
                generics,
                fields,
                methods: _,
            } => {
                let fields = fields
                    .iter()
                    .map(|f| Field {
                        span: f.span,
                        name: f.name,
                        ty: self.type_of(&f.ty),
                    })
                    .collect::<Vec<_>>();

                let fields = self.intern.intern_field_list(&fields);
                let mut new_ty = self.intern_ty(Type::Struct(*id, fields));

                if !generics.params.is_empty() {
                    let args = generics.params.iter().map(|g| g.id).collect::<Vec<_>>();
                    let args = self.intern.intern_id_list(&args);

                    new_ty = self.intern_ty(Type::Forall(args, new_ty));
                }

                self.types.borrow_mut().insert(*id, new_ty);
            }
            hir::ItemKind::Enum {
                generics,
                variants,
                methods: _,
            } => {
                let variants = variants
                    .iter()
                    .map(|v| {
                        let fields = v.fields.as_ref().map(|fields| {
                            fields
                                .iter()
                                .map(|f| Field {
                                    span: f.span,
                                    name: f.name,
                                    ty: self.type_of(&f.ty),
                                })
                                .collect::<Vec<_>>()
                        });

                        let fields = fields
                            .map(|fields| self.intern.intern_field_list(&fields))
                            .unwrap_or(List::empty());

                        Variant {
                            span: v.span,
                            name: v.name,
                            fields,
                        }
                    })
                    .collect::<Vec<_>>();

                let variants = self.intern.intern_variant_list(&variants);
                let mut new_ty = self.intern_ty(Type::Enum(*id, variants));

                if !generics.params.is_empty() {
                    let args = generics.params.iter().map(|g| g.id).collect::<Vec<_>>();
                    let args = self.intern.intern_id_list(&args);

                    new_ty = self.intern_ty(Type::Forall(args, new_ty));
                }

                self.types.borrow_mut().insert(*id, new_ty);
            }
            _ => {}
        }
    }
}
