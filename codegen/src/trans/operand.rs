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

                    Value::new_val(func, self.tcx.layout_of(id))
                }
                mir::Const::Type(_) => unimplemented!(),
                mir::Const::Bytes(_) => unimplemented!(),
            },
        }
    }
}
