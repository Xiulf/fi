use crate::{ir, mir, FunctionCtx};

impl<'db, 'ctx, 'mcx> FunctionCtx<'db, 'ctx, 'mcx> {
    pub fn translate(&mut self) {}

    pub fn trans_op(&mut self, op: &mir::Operand) -> ir::AnyValueEnum<'ctx> {}

    pub fn trans_place(&mut self, place: &mir::Place) -> ir::AnyValueEnum<'ctx> {}

    pub fn trans_const(&mut self, c: &mir::Const, ty: &mir::Ty) -> ir::AnyValueEnum<'ctx> {
        let layout = self.db.layout_of(self.lib, ty.clone());

        match c {
            mir::Const::Undefined => self.ctx.i32_type().get_undef().into(),
            _ => unimplemented!(),
        }
    }

    pub fn llvm_type(&self, layout: layout::TyLayout<check::ty::Ty>) -> ir::AnyTypeEnum {
        use check::ty::Type;
        use layout::*;

        match layout.abi {
            layout::Abi::Uninhabited => self.ctx.void_type().into(),
            layout::Abi::Scalar(scalar) if scalar.is_bool() => self.ctx.bool_type().into(),
            layout::Abi::Scalar(scalar) => match scalar.value {
                Primitive::Int(int, _) => match int {
                    Integer::I8 => self.ctx.i8_type().into(),
                    Integer::I16 => self.ctx.i16_type().into(),
                    Integer::I32 => self.ctx.i32_type().into(),
                    Integer::I64 => self.ctx.i64_type().into(),
                    Integer::I128 => self.ctx.i128_type().into(),
                },
                layout::Primitive::F32 => self.ctx.f32_type().into(),
                layout::Primitive::F64 => self.ctx.f64_type().into(),
                layout::Primitive::Pointer => self.llvm_type(layout.pointee(self.db)).ptr_type(),
            },
        }
    }
}
