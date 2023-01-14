use inkwell::types::{self, BasicType};
use inkwell::values;
use mir::syntax::{Operand, Place};

use crate::ctx::{BodyCtx, CodegenCtx};
use crate::operand::OperandRef;

impl<'ctx> BodyCtx<'_, '_, 'ctx> {
    pub fn codegen_intrinsic(&mut self, place: &Place, func: &str, args: &[Operand]) {
        let val = match func {
            | "byteswap" => {
                let width = self.operand_layout(&args[0]).size.bits();

                match func {
                    | "byteswap" if width <= 8 => self.codegen_operand(&args[0]).load(self.cx),
                    | "byteswap" => self.codegen_intrinsic_call(&format!("llvm.bswap.i{width}"), args),
                    | _ => unreachable!(),
                }
            },
            | _ => self.codegen_intrinsic_call(func, args),
        };

        let layout = self.place_layout(place);
        let res = OperandRef::new_imm(layout, val);

        self.store_return(place, res);
    }

    fn codegen_intrinsic_call(&mut self, func: &str, args: &[Operand]) -> values::BasicValueEnum<'ctx> {
        let func = self.cx.get_intrinsic(func);
        let args = args
            .iter()
            .map(|a| self.codegen_operand(a).load(self.cx).into())
            .collect::<Vec<_>>();

        self.builder
            .build_call(func, &args, "")
            .try_as_basic_value()
            .left()
            .unwrap()
    }
}

impl<'ctx> CodegenCtx<'_, 'ctx> {
    fn get_intrinsic(&mut self, name: &str) -> values::FunctionValue<'ctx> {
        if let Some(v) = self.intrinsics.get(name) {
            return v.clone();
        }

        self.declare_intrinsic(name)
    }

    fn insert_intrinsic(
        &mut self,
        name: &'static str,
        args: &[types::BasicMetadataTypeEnum<'ctx>],
        ret: types::BasicTypeEnum<'ctx>,
    ) -> values::FunctionValue<'ctx> {
        let ty = ret.fn_type(args, false);
        let f = self.module.add_function(name, ty, None);

        self.intrinsics.insert(name, f);
        f
    }

    fn declare_intrinsic(&mut self, name: &str) -> values::FunctionValue<'ctx> {
        macro_rules! ifn {
            ($name:literal,fn() -> $ret:expr) => {
                if name == $name {
                    return self.insert_intrinsic($name, &[], $ret.into());
                }
            };
            ($name:literal,fn($($arg:expr),*) -> $ret:expr) => {
                if name == $name {
                    return self.insert_intrinsic($name, &[$($arg.into()),*], $ret.into());
                }
            };
        }

        let t_i8 = self.context.i8_type();
        let t_i16 = self.context.i16_type();
        let t_i32 = self.context.i32_type();
        let t_i64 = self.context.i64_type();
        let t_i128 = self.context.i128_type();

        ifn!("llvm.bswap.i16", fn(t_i16) -> t_i16);
        ifn!("llvm.bswap.i32", fn(t_i32) -> t_i32);
        ifn!("llvm.bswap.i64", fn(t_i64) -> t_i64);
        ifn!("llvm.bswap.i128", fn(t_i128) -> t_i128);

        ifn!("llvm.bitreverse.i8", fn(t_i8) -> t_i8);
        ifn!("llvm.bitreverse.i16", fn(t_i16) -> t_i16);
        ifn!("llvm.bitreverse.i32", fn(t_i32) -> t_i32);
        ifn!("llvm.bitreverse.i64", fn(t_i64) -> t_i64);
        ifn!("llvm.bitreverse.i128", fn(t_i128) -> t_i128);

        panic!("unknown intrinsic {}", name);
    }
}
