use mir::ir;

use crate::ctx::BodyCtx;

impl<'ctx> BodyCtx<'_, '_, 'ctx> {
    pub fn codegen_intrinsic(&mut self, _place: &ir::Place, name: &str, args: &[ir::Operand]) {
        match name {
            | "panic" => {
                assert_eq!(args.len(), 1);
                let arg = self.codegen_operand(&args[0]);
                let ptr = arg.pair().0.into_pointer_value();
                self.build_puts(ptr);
            },
            | _ => panic!("unknown intrinsic {name:?}"),
        }
    }
}
