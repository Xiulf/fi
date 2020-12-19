use crate::ModuleCtx;
use layout::*;

pub trait LayoutLlvmExt {
    fn llvm_type<'ctx>(&self, mcx: &ModuleCtx<'_, 'ctx>) -> inkwell::types::AnyTypeEnum<'ctx>;
}

impl LayoutLlvmExt for TyLayout<check::ty::Ty> {
    fn llvm_type<'ctx>(&self, mcx: &ModuleCtx<'_, 'ctx>) -> inkwell::types::AnyTypeEnum<'ctx> {
        use inkwell::types::BasicType;
        use inkwell::AddressSpace;

        if let Abi::Scalar(scalar) = &self.abi {
            if scalar.is_bool() {
                mcx.ctx.bool_type().into()
            } else {
                match scalar.value {
                    Primitive::Int(int, _) => match int {
                        Integer::I8 => mcx.ctx.i8_type().into(),
                        Integer::I16 => mcx.ctx.i16_type().into(),
                        Integer::I32 => mcx.ctx.i32_type().into(),
                        Integer::I64 => mcx.ctx.i64_type().into(),
                        Integer::I128 => mcx.ctx.i128_type().into(),
                    },
                    Primitive::F32 => mcx.ctx.f32_type().into(),
                    Primitive::F64 => mcx.ctx.f64_type().into(),
                    Primitive::Pointer => inkwell::types::Type::new(
                        self.pointee(mcx.lib, mcx.db.to_layout_db())
                            .llvm_type(mcx)
                            .as_type_ref(),
                    )
                    .ptr_type(AddressSpace::Generic),
                }
            }
        } else {
            unimplemented!();
        }
    }
}
