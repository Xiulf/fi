use crate::place::Place;
use crate::ptr::Pointer;
use crate::FunctionCtx;
use cranelift::codegen::ir::InstBuilder;
use cranelift_module::Backend;

impl<'a, 'tcx, B: Backend> FunctionCtx<'a, 'tcx, B> {
    pub fn trans_place(&mut self, place: &mir::Place<'tcx>) -> Place<'tcx> {
        let mut res = match &place.base {
            mir::PlaceBase::Local(id) => self.locals[id],
            mir::PlaceBase::Global(id) => {
                let (data_id, layout) = self.data_ids[id];
                let local_data_id = self.module.declare_data_in_func(data_id, self.builder.func);
                let global_ptr = self
                    .builder
                    .ins()
                    .global_value(self.pointer_type, local_data_id);

                Place::new_ref(Pointer::addr(global_ptr), layout)
            }
        };

        for elem in &place.elems {
            match elem {
                mir::PlaceElem::Deref => res = res.deref(self),
                mir::PlaceElem::Field(idx) => res = res.field(self, *idx),
                mir::PlaceElem::Index(idx) => {
                    let idx = self.trans_operand(idx).load_scalar(self);

                    res = res.index(self, idx);
                }
                mir::PlaceElem::Slice(lo, hi) => {
                    let lo = self.trans_operand(lo).load_scalar(self);
                    let hi = self.trans_operand(hi).load_scalar(self);
                    let len = self.builder.ins().isub(hi, lo);
                    let ptr = res.index(self, lo).as_ptr();
                    let slice_ty = self
                        .tcx
                        .intern_ty(check::ty::Type::Slice(res.layout.ty.idx(self.tcx)));

                    let layout = self.tcx.layout(slice_ty);

                    res = Place::new_ref_meta(ptr, len, layout);
                }
                mir::PlaceElem::AsVariant(idx) => {
                    res = Place {
                        kind: res.kind,
                        layout: self.tcx.layout_for_variant(res.layout, *idx),
                    };
                }
            }
        }

        res
    }
}
