use crate::place::Place;
use crate::value::Value;
use crate::FunctionCtx;
use cranelift::codegen::ir::InstBuilder;
use cranelift_module::Backend;

impl<'a, 'tcx, B: Backend> FunctionCtx<'a, 'tcx, B> {
    pub fn trans_operand(&mut self, operand: &mir::Operand<'tcx>) -> Value<'tcx> {
        match operand {
            mir::Operand::Place(place) => self.trans_place(place).to_value(self),
            mir::Operand::Const(c) => match c {
                mir::Const::Unit => Value::new_unit(self.tcx.layout(self.tcx.builtin.unit)),
                mir::Const::Scalar(val, ty) => Value::new_const(self, *val, self.tcx.layout(ty)),
                mir::Const::FuncAddr(id) => {
                    let func = self.func_ids[id].0;
                    let func = self.module.declare_func_in_func(func, self.builder.func);
                    let func = self.builder.ins().func_addr(self.pointer_type, func);

                    Value::new_val(func, self.tcx.layout_of(&mir::Id::item(*id)))
                }
                mir::Const::Type(_) => unimplemented!(),
                mir::Const::Bytes(bytes) => {
                    let place = Place::new_stack(self, self.tcx.layout(self.tcx.builtin.str));

                    self.trans_bytes(place, bytes)
                }
            },
        }
    }

    pub fn trans_bytes(&mut self, place: Place<'tcx>, bytes: &Box<[u8]>) -> Value<'tcx> {
        use cranelift_module::{DataContext, Linkage};
        let data_id = self
            .module
            .declare_data(
                &format!("__bytes_{}", self.bytes_count),
                Linkage::Local,
                false,
                false,
                Some(1),
            )
            .unwrap();

        let mut data_ctx = DataContext::new();

        *self.bytes_count += 1;
        data_ctx.define(bytes.clone());
        self.module.define_data(data_id, &data_ctx).unwrap();

        let global = self.module.declare_data_in_func(data_id, self.builder.func);
        let value = self.builder.ins().global_value(self.pointer_type, global);
        let len = self
            .builder
            .ins()
            .iconst(self.pointer_type, bytes.len() as i64);

        place.field(self, 0).store(
            self,
            Value::new_val(value, self.tcx.layout(self.tcx.builtin.ref_u8)),
        );

        place.field(self, 1).store(
            self,
            Value::new_val(len, self.tcx.layout(self.tcx.builtin.usize)),
        );

        place.to_value(self)
    }
}
