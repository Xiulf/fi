use inkwell::types::BasicTypeEnum;
use inkwell::values::{self, BasicValue};

use crate::ctx::CodegenCtx;
use crate::layout::ReprAndLayout;
use crate::place::PlaceRef;

#[derive(Debug, Clone, Copy)]
pub enum OperandValue<'ctx> {
    Ref(values::PointerValue<'ctx>),
    Imm(values::BasicValueEnum<'ctx>),
    Pair(values::BasicValueEnum<'ctx>, values::BasicValueEnum<'ctx>),
}

#[derive(Debug, Clone)]
pub struct OperandRef<'ctx> {
    pub val: OperandValue<'ctx>,
    pub layout: ReprAndLayout,
}

impl<'ctx> OperandRef<'ctx> {
    pub fn new_zst(ctx: &mut CodegenCtx<'_, 'ctx>, layout: ReprAndLayout) -> Self {
        let undef = match ctx.basic_type_for_repr(&layout.repr) {
            | BasicTypeEnum::IntType(t) => t.get_undef().as_basic_value_enum(),
            | BasicTypeEnum::FloatType(t) => t.get_undef().as_basic_value_enum(),
            | BasicTypeEnum::PointerType(t) => t.get_undef().as_basic_value_enum(),
            | BasicTypeEnum::ArrayType(t) => t.get_undef().as_basic_value_enum(),
            | BasicTypeEnum::StructType(t) => t.get_undef().as_basic_value_enum(),
            | BasicTypeEnum::VectorType(t) => t.get_undef().as_basic_value_enum(),
        };

        Self {
            val: OperandValue::Imm(undef),
            layout,
        }
    }

    pub fn new_imm(layout: ReprAndLayout, imm: values::BasicValueEnum<'ctx>) -> Self {
        Self {
            val: OperandValue::Imm(imm),
            layout,
        }
    }

    pub fn new_pair(layout: ReprAndLayout, a: values::BasicValueEnum<'ctx>, b: values::BasicValueEnum<'ctx>) -> Self {
        Self {
            val: OperandValue::Pair(a, b),
            layout,
        }
    }

    pub fn immediate(self) -> values::BasicValueEnum<'ctx> {
        match self.val {
            | OperandValue::Imm(v) => v,
            | _ => unreachable!(),
        }
    }

    pub fn deref(&self, ctx: &mut CodegenCtx<'_, 'ctx>) -> PlaceRef<'ctx> {
        let layout = self.layout.elem(ctx.db).unwrap();
        let ptr = match self.val {
            | OperandValue::Imm(ptr) => ptr.into_pointer_value(),
            | _ => unreachable!(),
        };

        PlaceRef {
            ptr,
            layout: layout.layout,
        }
    }

    pub fn store(&self, ctx: &mut CodegenCtx<'_, 'ctx>, dest: PlaceRef<'ctx>) {
        if dest.layout.is_zst() {
            return;
        }

        match self.val {
            | OperandValue::Ref(ptr) => {
                let src_align = self.layout.align.bytes() as u32;
                let dst_align = dest.layout.align.bytes() as u32;
                let size = ctx.context.i32_type().const_int(dest.layout.size.bytes(), false);

                ctx.builder
                    .build_memcpy(dest.ptr, dst_align, ptr, src_align, size)
                    .unwrap();
            },
            | OperandValue::Imm(val) => {
                ctx.builder.build_store(dest.ptr, val);
            },
            | OperandValue::Pair(a, b) => {
                let ptr = ctx.builder.build_struct_gep(dest.ptr, 0, "").unwrap();
                ctx.builder.build_store(ptr, a);
                let ptr = ctx.builder.build_struct_gep(dest.ptr, 1, "").unwrap();
                ctx.builder.build_store(ptr, b);
            },
        }
    }
}
