use inkwell::types::BasicType;
use inkwell::{values, AddressSpace};

use crate::ctx::CodegenCtx;
use crate::layout::{Abi, ReprAndLayout};
use crate::operand::{OperandRef, OperandValue};

#[derive(Debug, Clone)]
pub struct PlaceRef<'ctx> {
    pub layout: ReprAndLayout,
    pub ptr: values::PointerValue<'ctx>,
}

impl<'ctx> PlaceRef<'ctx> {
    pub fn new_uninit(ctx: &mut CodegenCtx<'_, 'ctx>, layout: ReprAndLayout) -> Self {
        let ty = ctx.basic_type_for_layout(&layout).ptr_type(AddressSpace::Local);

        Self {
            layout,
            ptr: ty.const_null(),
        }
    }

    pub fn new(layout: ReprAndLayout, ptr: values::PointerValue<'ctx>) -> Self {
        Self { layout, ptr }
    }

    pub fn new_alloca(ctx: &mut CodegenCtx<'_, 'ctx>, layout: ReprAndLayout) -> Self {
        let ty = ctx.basic_type_for_repr(&layout.repr);
        let ptr = ctx.builder.build_alloca(ty, "");

        Self { layout, ptr }
    }

    pub fn cast(&self, ctx: &mut CodegenCtx<'_, 'ctx>, layout: ReprAndLayout) -> Self {
        if self.layout == layout {
            return self.clone();
        }

        let ads = self.ptr.get_type().get_address_space();
        let ty = ctx.basic_type_for_layout(&layout).ptr_type(ads);
        let ptr = ctx.builder.build_pointer_cast(self.ptr, ty, "");

        Self { layout, ptr }
    }

    pub fn deref(self, ctx: &mut CodegenCtx<'_, 'ctx>) -> Self {
        self.load_operand(ctx).deref(ctx)
    }

    pub fn field(&self, ctx: &mut CodegenCtx<'_, 'ctx>, field: usize) -> Self {
        let layout = self.layout.field(ctx.db, field).unwrap();

        if !self.ptr.get_type().get_element_type().is_struct_type() {
            return Self { layout, ptr: self.ptr };
        }

        let ptr = ctx.builder.build_struct_gep(self.ptr, field as u32, "").unwrap();

        Self { layout, ptr }
        // let layout = self.layout.field(ctx.db, field).unwrap();
        // let offset = self.layout.fields.offset(field);

        // if offset == Size::ZERO {
        //     return self.cast(ctx, layout);
        // }

        // let ads = self.ptr.get_type().get_address_space();
        // let isize = ctx.context.ptr_sized_int_type(&ctx.target_data, Some(ads));
        // let offset = isize.const_int(offset.bytes(), true);
        // let mut new = self.cast(ctx, layout);

        // new.ptr = unsafe { ctx.builder.build_gep(new.ptr, &[offset], "") };
        // new
    }

    pub fn downcast(&self, ctx: &mut CodegenCtx<'_, 'ctx>, _ctor: hir::Ctor) -> Self {
        todo!()
    }

    // pub fn to_value(self) -> Value<'ctx> {
    //     Value::new_ref(self.layout.layout, self.ptr)
    // }

    // pub fn load(&self, ctx: &mut CodegenCtx<'_, 'ctx>) -> values::BasicValueEnum<'ctx> {
    //     ctx.builder.build_load(self.ptr, "")
    // }

    // pub fn store(&self, ctx: &mut CodegenCtx<'_, 'ctx>, value: Value<'ctx>) {
    //     match value.kind {
    //         | ValueKind::Value(value) => {
    //             ctx.builder.build_store(self.ptr, value);
    //         },
    //         | ValueKind::ValuePair(a, b) => {
    //             let ptr = self.field(ctx, 0).ptr;
    //             ctx.builder.build_store(ptr, a);
    //             let ptr = self.field(ctx, 1).ptr;
    //             ctx.builder.build_store(ptr, b);
    //         },
    //         | ValueKind::ByRef(ptr) => {
    //             let src_align = value.layout.align.bytes() as u32;
    //             let dst_align = self.layout.align.bytes() as u32;
    //             let size = self.layout.size.min(value.layout.size).bytes();
    //             let size = ctx.context.i32_type().const_int(size, false);

    //             ctx.builder
    //                 .build_memcpy(self.ptr, dst_align, ptr, src_align, size)
    //                 .unwrap();
    //         },
    //     }
    // }

    pub fn load_operand(self, ctx: &mut CodegenCtx<'_, 'ctx>) -> OperandRef<'ctx> {
        if self.layout.is_zst() {
            return OperandRef::new_zst(ctx, self.layout);
        }

        let val = match &self.layout.abi {
            | Abi::Scalar(_) => {
                let load = ctx.builder.build_load(self.ptr, "");

                OperandValue::Imm(load)
            },
            | Abi::ScalarPair(_, _) => {
                let load = |i| {
                    let ptr = ctx.builder.build_struct_gep(self.ptr, i, "").unwrap();
                    ctx.builder.build_load(ptr, "")
                };

                OperandValue::Pair(load(0), load(1))
            },
            | _ => OperandValue::Ref(self.ptr),
        };

        OperandRef {
            layout: self.layout,
            val,
        }
    }
}
