use crate::constraint::Constraint;
use crate::tcx::Tcx;
use crate::ty::*;

impl<'tcx> Tcx<'tcx> {
    pub fn infer_item(&self, id: &hir::ItemId) -> Ty<'tcx> {
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
                        ty: self.type_of(&hir::Id::item(*id)),
                    }
                }));

                let ret = self.type_of(ret);

                self.intern_ty(Type::Func(params, ret))
            }
            hir::ItemKind::Param { ty } => self.type_of(ty),
            hir::ItemKind::Var { ty, .. } => self.type_of(ty),
        }
    }

    pub fn check_item(&self, id: &hir::ItemId) {
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
            _ => {}
        }
    }
}
