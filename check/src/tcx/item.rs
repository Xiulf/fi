use crate::constraint::Constraint;
use crate::tcx::Tcx;
use crate::ty::*;

impl<'tcx> Tcx<'tcx> {
    pub fn infer_item(&self, id: &hir::Id) -> Ty<'tcx> {
        let item = &self.package.items[id];

        match &item.kind {
            hir::ItemKind::Extern { abi: _, ty } => self.type_of(ty),
            hir::ItemKind::Func {
                params,
                ret,
                body: _,
            } => {
                let params = self.arena.alloc_slice_fill_iter(params.iter().map(|id| {
                    let param = &self.package.items[id];

                    Param {
                        name: param.name,
                        ty: self.type_of(id),
                    }
                }));

                let ret = self.type_of(ret);

                self.intern_ty(Type::Func(params, ret))
            }
            hir::ItemKind::Param { ty } => self.type_of(ty),
            hir::ItemKind::Var { ty, .. } => self.type_of(ty),
            hir::ItemKind::Struct { .. } => self.new_var(),
            hir::ItemKind::Enum { .. } => self.new_var(),
            hir::ItemKind::Cons {
                item,
                variant: _,
                params: Some(params),
            } => {
                let params = self
                    .arena
                    .alloc_slice_fill_iter(params.iter().map(|field| Param {
                        name: field.name,
                        ty: self.type_of(&field.ty),
                    }));

                let ret = self.type_of(item);

                self.intern_ty(Type::Func(params, ret))
            }
            hir::ItemKind::Cons {
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
            hir::ItemKind::Var {
                ty, val: Some(val), ..
            } => {
                let val_ty = self.type_of(val);
                let val_span = self.span_of(val);
                let ty_ty = self.type_of(ty);
                let ty_span = self.span_of(ty);

                self.constrain(Constraint::Equal(val_ty, val_span, ty_ty, ty_span));
            }
            hir::ItemKind::Struct { fields } => {
                let ty = self.type_of(id);
                let fields = fields.iter().map(|f| Field {
                    name: f.name,
                    ty: self.type_of(&f.ty),
                });

                let fields = self.arena.alloc_slice_fill_iter(fields);
                let new_ty = self.intern_ty(Type::Struct(*id, fields));

                self.constrain(Constraint::Equal(ty, item.span, new_ty, item.span));
            }
            hir::ItemKind::Enum { variants } => {
                let ty = self.type_of(id);
                let variants = variants.iter().map(|v| {
                    let fields = v.fields.as_ref().map(|fields| {
                        fields.iter().map(|f| Field {
                            name: f.name,
                            ty: self.type_of(&f.ty),
                        })
                    });

                    let fields = fields
                        .map(|fields| self.arena.alloc_slice_fill_iter(fields))
                        .unwrap_or(&mut []);

                    Variant {
                        name: v.name,
                        fields,
                    }
                });

                let variants = self.arena.alloc_slice_fill_iter(variants);
                let new_ty = self.intern_ty(Type::Enum(*id, variants));

                self.constrain(Constraint::Equal(ty, item.span, new_ty, item.span));
            }
            _ => {}
        }
    }
}
