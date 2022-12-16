use base_db::target::CompilerTarget;
use inkwell::types::BasicType;
use inkwell::{values, AddressSpace};

use crate::ctx::CodegenCtx;
use crate::layout::{primitive_align, primitive_size, Abi, ReprAndLayout, TagEncoding, Variants};
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

    pub fn field(&self, ctx: &mut CodegenCtx<'_, 'ctx>, index: usize) -> Self {
        let triple = match ctx.db.target() {
            | CompilerTarget::Native(triple) => triple,
            | _ => unreachable!(),
        };

        let field = self.layout.field(ctx.db, index).unwrap();
        let offset = self.layout.fields.offset(index);

        let ptr = match self.layout.abi {
            | _ if offset.bytes() == 0 => self.ptr,
            | Abi::ScalarPair(ref a, ref b)
                if offset == primitive_size(a.value, &triple).align_to(primitive_align(b.value, &triple)) =>
            {
                ctx.builder.build_struct_gep(self.ptr, 1, "").unwrap()
            },
            | Abi::Scalar(_) | Abi::ScalarPair(..) if field.is_zst() => {
                let ptr = ctx.context.i8_type().ptr_type(AddressSpace::Generic);
                let ptr = ctx.builder.build_pointer_cast(self.ptr, ptr, "");
                let usize = ctx.context.ptr_sized_int_type(&ctx.target_data, None);
                let offset = usize.const_int(offset.bytes(), false);
                unsafe { ctx.builder.build_gep(ptr, &[offset], "") }
            },
            | Abi::Scalar(_) | Abi::ScalarPair(..) => unreachable!(),
            | _ => {
                let min_offset = self.layout.fields.min_offset();
                let index = if min_offset.bytes() != 0 { index + 1 } else { index };

                ctx.builder.build_struct_gep(self.ptr, index as u32, "").unwrap()
            },
        };

        let ty = ctx.basic_type_for_repr(&field.repr).ptr_type(AddressSpace::Generic);
        let ptr = ctx.builder.build_pointer_cast(ptr, ty, "");

        PlaceRef::new(field, ptr)
    }

    pub fn downcast(&self, ctx: &mut CodegenCtx<'_, 'ctx>, ctor: hir::Ctor) -> Self {
        let mut downcast = self.clone();
        let ctors = ctor.type_ctor().ctors(ctx.db.upcast());
        let index = ctors.iter().position(|&c| c == ctor).unwrap();

        downcast.layout = self.layout.variant(index);

        let ty = ctx.basic_type_for_layout(&downcast.layout);
        let ty = ty.ptr_type(AddressSpace::Generic);

        downcast.ptr = ctx.builder.build_pointer_cast(downcast.ptr, ty, "");
        downcast
    }

    pub fn get_discr(&self, ctx: &mut CodegenCtx<'_, 'ctx>, layout: &ReprAndLayout) -> values::IntValue<'ctx> {
        let discr_ty = ctx.basic_type_for_repr(&layout.repr).into_int_type();
        let (_tag_scalar, tag_encoding, tag_field) = match self.layout.variants {
            | Variants::Single { index } => {
                return discr_ty.const_int(index as u64, false);
            },
            | Variants::Multiple {
                ref tag,
                ref tag_encoding,
                tag_field,
                ..
            } => (tag, tag_encoding, tag_field),
        };

        let tag = self.field(ctx, tag_field);
        let tag_op = tag.load_operand(ctx);
        let tag_imm = tag_op.immediate().into_int_value();

        match *tag_encoding {
            | TagEncoding::Direct => ctx.builder.build_int_cast(tag_imm, discr_ty, ""),
            | TagEncoding::Niche { .. } => todo!(),
        }
    }

    pub fn set_discr(&self, ctx: &mut CodegenCtx<'_, 'ctx>, idx: usize) {
        if self.layout.variant(idx).abi.is_uninhabited() {
            return;
        }

        match self.layout.variants {
            | Variants::Single { index } => {
                assert_eq!(index, idx);
            },
            | Variants::Multiple {
                tag_encoding: TagEncoding::Direct,
                tag_field,
                ref tag,
                ..
            } => {
                let ptr = self.field(ctx, tag_field).ptr;
                let ty = ctx.basic_type_for_scalar(tag, None).into_int_type();
                let val = ty.const_int(idx as u64, false);

                ctx.builder.build_store(ptr, val);
            },
            | Variants::Multiple {
                tag_encoding:
                    TagEncoding::Niche {
                        dataful_variant,
                        ref niche_variants,
                        niche_start,
                    },
                tag_field,
                ..
            } => {
                if idx != dataful_variant {
                    let niche = self.field(ctx, tag_field);
                    let niche_ty = ctx.basic_type_for_repr(&niche.layout.repr).into_int_type();
                    let niche_val = idx - *niche_variants.start();
                    let niche_val = (niche_val as u128).wrapping_add(niche_start);
                    let niche_val = niche_ty.const_int(niche_val as u64, false);

                    ctx.builder.build_store(niche.ptr, niche_val);
                }
            },
        }
    }

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
