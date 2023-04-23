use inkwell::types::{BasicType, BasicTypeEnum};
use inkwell::values::{self, BasicValue};
use inkwell::AddressSpace;

use crate::ctx::CodegenCtx;
use crate::layout::{Abi, ReprAndLayout};
use crate::place::PlaceRef;

#[derive(Debug, Clone, Copy)]
pub enum OperandValue<'ctx> {
    Ref(values::PointerValue<'ctx>, Option<values::BasicValueEnum<'ctx>>),
    Phi(values::PhiValue<'ctx>),
    Imm(values::BasicValueEnum<'ctx>),
    Pair(values::BasicValueEnum<'ctx>, values::BasicValueEnum<'ctx>),
}

#[derive(Debug, Clone)]
pub struct OperandRef<'ctx> {
    pub layout: ReprAndLayout,
    pub val: OperandValue<'ctx>,
}

impl<'ctx> OperandRef<'ctx> {
    pub fn new_zst(ctx: &mut CodegenCtx<'_, 'ctx>, layout: ReprAndLayout) -> Self {
        let undef = match ctx.basic_type_for_ral(&layout) {
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

    pub fn new_phi(layout: ReprAndLayout, phi: values::PhiValue<'ctx>) -> Self {
        Self {
            val: OperandValue::Phi(phi),
            layout,
        }
    }

    pub fn new_pair(layout: ReprAndLayout, a: values::BasicValueEnum<'ctx>, b: values::BasicValueEnum<'ctx>) -> Self {
        Self {
            val: OperandValue::Pair(a, b),
            layout,
        }
    }

    #[track_caller]
    pub fn immediate(self) -> values::BasicValueEnum<'ctx> {
        match self.val {
            | OperandValue::Imm(v) => v,
            | OperandValue::Phi(p) => p.as_basic_value(),
            | v => unreachable!("{:?}", v),
        }
    }

    #[track_caller]
    pub fn phi(self) -> values::PhiValue<'ctx> {
        match self.val {
            | OperandValue::Phi(v) => v,
            | v => unreachable!("{:?}", v),
        }
    }

    #[track_caller]
    pub fn pair(&self) -> (values::BasicValueEnum<'ctx>, values::BasicValueEnum<'ctx>) {
        match self.val {
            | OperandValue::Pair(a, b) => (a, b),
            | v => unreachable!("{:?}", v),
        }
    }

    #[track_caller]
    pub fn _by_ref(self) -> values::PointerValue<'ctx> {
        match self.val {
            | OperandValue::Ref(ptr, _) => ptr,
            | v => unreachable!("{:?}", v),
        }
    }

    pub fn load(&self, ctx: &mut CodegenCtx<'_, 'ctx>) -> values::BasicValueEnum<'ctx> {
        match self.val {
            | OperandValue::Ref(ptr, _) => ctx.builder.build_load(ptr, ""),
            | OperandValue::Phi(phi) => phi.as_basic_value(),
            | OperandValue::Imm(imm) => imm,
            | OperandValue::Pair(_, _) => todo!(),
        }
    }

    pub fn deref(&self, ctx: &mut CodegenCtx<'_, 'ctx>) -> PlaceRef<'ctx> {
        let layout = self.layout.elem(ctx.db).unwrap();
        let (ptr, extra) = match self.val {
            | OperandValue::Imm(ptr) => (ptr.into_pointer_value(), None),
            | OperandValue::Pair(ptr, extra) => (ptr.into_pointer_value(), Some(extra)),
            | _ => unreachable!(),
        };

        PlaceRef { layout, ptr, extra }
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
                let ty = ctx.basic_type_for_ral(&field);
                *val = ctx.builder.build_bitcast(*val, ty, "");
            },
            | (OperandValue::Pair(a_val, b_val), Abi::ScalarPair(_, _)) => {
                let a_ty = ctx.basic_type_for_ral(&field.field(ctx.db, 0).unwrap());
                let b_ty = ctx.basic_type_for_ral(&field.field(ctx.db, 1).unwrap());
                *a_val = ctx.builder.build_bitcast(*a_val, a_ty, "");
                *b_val = ctx.builder.build_bitcast(*b_val, b_ty, "");
            },
            | (OperandValue::Pair(..), _) => unreachable!(),
            | (OperandValue::Ref(..), _) => unreachable!(),
            | (OperandValue::Phi(..), _) => unreachable!(),
        }

        Self { val, layout: field }
    }

    pub fn bitcast(self, ctx: &mut CodegenCtx<'_, 'ctx>, layout: ReprAndLayout) -> Self {
        let ty = ctx.basic_type_for_ral(&layout);
        let val = match self.val {
            | OperandValue::Imm(val) => OperandValue::Imm(ctx.builder.build_bitcast(val, ty, "")),
            | OperandValue::Ref(ptr, None) if matches!(layout.abi, Abi::ScalarPair(_, _)) => {
                let val = ctx.builder.build_load(ptr, "").into_struct_value();
                let a = ctx.builder.build_extract_value(val, 0, "").unwrap();
                let b = ctx.builder.build_extract_value(val, 1, "").unwrap();
                OperandValue::Pair(a, b)
            },
            | OperandValue::Ref(ptr, Some(extra)) if matches!(layout.abi, Abi::ScalarPair(_, _)) => {
                let val = ctx.builder.build_load(ptr, "");
                OperandValue::Pair(val, extra)
            },
            | OperandValue::Ref(ptr, extra) => {
                let ty = ty.ptr_type(AddressSpace::default());
                OperandValue::Ref(ctx.builder.build_pointer_cast(ptr, ty, ""), extra)
            },
            | OperandValue::Phi(phi) => OperandValue::Phi(phi),
            | OperandValue::Pair(a, b) => {
                let a_ty = ctx.basic_type_for_ral(&layout.field(ctx.db, 0).unwrap());
                let b_ty = ctx.basic_type_for_ral(&layout.field(ctx.db, 1).unwrap());
                let a = ctx.builder.build_bitcast(a, a_ty, "");
                let b = ctx.builder.build_bitcast(b, b_ty, "");

                OperandValue::Pair(a, b)
            },
        };

        Self { layout, val }
    }

    pub fn store(&self, ctx: &mut CodegenCtx<'_, 'ctx>, dest: &PlaceRef<'ctx>) {
        self.val.store(ctx, dest);
    }
}

impl<'ctx> OperandValue<'ctx> {
    pub fn store(self, ctx: &mut CodegenCtx<'_, 'ctx>, dest: &PlaceRef<'ctx>) {
        if dest.layout.is_zst() {
            return;
        }

        match self {
            | OperandValue::Ref(ptr, None) => {
                let dst_align = dest.layout.align.bytes() as u32;
                let size = ctx.context.i32_type().const_int(dest.layout.size.bytes(), false);

                ctx.builder
                    .build_memcpy(dest.ptr, dst_align, ptr, dst_align, size)
                    .unwrap();
            },
            | OperandValue::Ref(_, Some(_)) => unreachable!(),
            | OperandValue::Imm(val) => {
                ctx.builder.build_store(dest.ptr, val);
            },
            | OperandValue::Phi(val) => {
                ctx.builder.build_store(dest.ptr, val.as_basic_value());
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
