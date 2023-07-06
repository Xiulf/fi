use inkwell::values::BasicValue;
use mir::ir;

use crate::ctx::BodyCtx;
use crate::operand::OperandRef;

impl<'ctx> BodyCtx<'_, '_, 'ctx> {
    pub fn codegen_intrinsic(&mut self, place: &ir::Place, name: &str, args: &[ir::Operand]) {
        let layout = self.place_layout(place);

        match name {
            | "panic" => {
                assert_eq!(args.len(), 1);
                let arg = self.codegen_operand(&args[0]);
                let ptr = arg.pair().0.into_pointer_value();
                self.build_puts(ptr);
            },
            | "ptr_load_volatile" => {
                let ty = self.basic_type_for_ral(&layout.elem(self.db).unwrap());
                let arg = self.codegen_operand(&args[0]);
                let ptr = arg.load(self.cx).into_pointer_value();
                let val = self.builder.build_load(ty, ptr, "");
                let op = OperandRef::new_imm(layout, val);
                let load = val.as_instruction_value().unwrap();

                load.set_volatile(true).unwrap();
                self.store_return(place, op);
            },
            | _ => panic!("unknown intrinsic {name:?}"),
        }
    }
}
