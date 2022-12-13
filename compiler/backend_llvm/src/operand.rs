use inkwell::types::{BasicType, BasicTypeEnum};
use inkwell::values::{self, BasicValue};
use inkwell::AddressSpace;

use crate::ctx::CodegenCtx;
use crate::layout::{Abi, ReprAndLayout};
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
            | v => unreachable!("{:?}", v),
        }
    }

    pub fn by_ref(self) -> values::PointerValue<'ctx> {
        match self.val {
            | OperandValue::Ref(ptr) => ptr,
            | v => unreachable!("{:?}", v),
        }
    }

    pub fn load(self, ctx: &mut CodegenCtx<'_, 'ctx>) -> values::BasicValueEnum<'ctx> {
        match self.val {
            | OperandValue::Ref(ptr) => ctx.builder.build_load(ptr, ""),
            | _ => self.immediate(),
        }
    }

    pub fn deref(&self, ctx: &mut CodegenCtx<'_, 'ctx>) -> PlaceRef<'ctx> {
        let layout = self.layout.elem(ctx.db).unwrap();
        let ptr = match self.val {
            | OperandValue::Imm(ptr) => ptr.into_pointer_value(),
            | _ => unreachable!(),
        };

        PlaceRef { ptr, layout }
    }

    pub fn field(&self, ctx: &mut CodegenCtx<'_, 'ctx>, index: usize) -> Self {
        let field = self.layout.field(ctx.db, index).unwrap();
        let offset = self.layout.fields.offset(index);
        let mut val = match (self.val, &self.layout.abi) {
            | _ if field.is_zst() => {
                return Self::new_zst(ctx, field);
            },
            | (OperandValue::Imm(_) | OperandValue::Pair(..), _) if field.size == self.layout.size => {
                assert_eq!(offset.bytes(), 0);
                self.val
            },
            | (OperandValue::Pair(a_val, b_val), Abi::ScalarPair(_, _)) => {
                if offset.bytes() == 0 {
                    OperandValue::Imm(a_val)
                } else {
                    OperandValue::Imm(b_val)
                }
            },
            | _ => unreachable!(),
        };

        match (&mut val, &field.abi) {
            | (OperandValue::Imm(val), _) => {
                let ty = ctx.basic_type_for_repr(&field.repr);
                *val = ctx.builder.build_bitcast(*val, ty, "");
            },
            | (OperandValue::Pair(a_val, b_val), Abi::ScalarPair(a, b)) => {
                let a_ty = ctx.basic_type_for_scalar(a, None);
                let b_ty = ctx.basic_type_for_scalar(b, None);
                *a_val = ctx.builder.build_bitcast(*a_val, a_ty, "");
                *b_val = ctx.builder.build_bitcast(*b_val, b_ty, "");
            },
            | (OperandValue::Pair(..), _) => unreachable!(),
            | (OperandValue::Ref(..), _) => unreachable!(),
        }

        Self { val, layout: field }
    }

    pub fn bitcast(self, ctx: &mut CodegenCtx<'_, 'ctx>, layout: ReprAndLayout) -> Self {
        let ty = ctx.basic_type_for_repr(&layout.repr);
        let val = match self.val {
            | OperandValue::Imm(val) => OperandValue::Imm(ctx.builder.build_bitcast(val, ty, "")),
            | OperandValue::Ref(ptr) => {
                let ty = ty.ptr_type(AddressSpace::Generic);
                OperandValue::Ref(ctx.builder.build_pointer_cast(ptr, ty, ""))
            },
            | OperandValue::Pair(a, b) => OperandValue::Pair(a, b),
        };

        Self { layout, val }
    }

    pub fn store(&self, ctx: &mut CodegenCtx<'_, 'ctx>, dest: &PlaceRef<'ctx>) {
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
