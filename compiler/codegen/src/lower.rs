use crate::*;
use mir::layout::Layout;

impl FunctionCtx<'_, '_> {
    pub fn lower_place(&mut self, place: &ir::Place) -> place::Place {
        let mut res = self.locals[place.local].clone();

        for elem in &place.elems {
            match elem {
                | ir::PlaceElem::Deref => res = res.deref(self),
                | ir::PlaceElem::Field(field) => res = res.field(self, *field),
                | ir::PlaceElem::Index(op) => {
                    let idx = self.lower_op(op, None);

                    res = res.index(self, idx);
                },
                | ir::PlaceElem::Offset(op) => {
                    let offset = self.lower_op(op, None);

                    res = res.offset(self, offset);
                },
                | ir::PlaceElem::Downcast(idx) => res = res.downcast_variant(self, *idx),
            }
        }

        res
    }

    fn lower_op(&mut self, op: &ir::Operand, into: Option<place::Place>) -> value::Value {
        match op {
            | ir::Operand::Place(place) => {
                let place = self.lower_place(place);
                let value = place.to_value(self);

                if let Some(into) = into {
                    into.store(self, value.clone());
                }

                value
            },
            | ir::Operand::Const(c, lyt) => self.lower_const(c, lyt.clone(), into),
        }
    }

    fn lower_const(&mut self, c: &ir::Const, layout: Arc<Layout>, into: Option<place::Place>) -> value::Value {
        unimplemented!()
    }
}
