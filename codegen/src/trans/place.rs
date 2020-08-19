use crate::place::Place;
use crate::FunctionCtx;
use cranelift::codegen::ir::{self, InstBuilder};
use cranelift_module::Backend;

impl<'a, 'tcx, B: Backend> FunctionCtx<'a, 'tcx, B> {
    pub fn trans_place(&mut self, place: &mir::Place) -> Place<'tcx> {
        let mut res = match &place.base {
            mir::PlaceBase::Local(id) => self.locals[id],
            mir::PlaceBase::Global(_) => unimplemented!(),
        };

        for elem in &place.elems {
            match elem {
                mir::PlaceElem::Deref => res = res.deref(self),
                mir::PlaceElem::Field(idx) => res = res.field(self, *idx),
                mir::PlaceElem::Index(idx) => {
                    let idx = self.trans_place(idx).to_value(self).load_scalar(self);

                    res = res.index(self, idx);
                }
            }
        }

        res
    }
}
