use std::sync::Arc;

use inkwell::values;

use crate::ctx::CodegenCtx;
use crate::layout::{Abi, Layout};

#[derive(Clone)]
pub struct Value<'ctx> {
    pub layout: Arc<Layout>,
    pub kind: ValueKind<'ctx>,
}

#[derive(Clone)]
pub enum ValueKind<'ctx> {
    Value(values::BasicValueEnum<'ctx>),
    ValuePair(values::BasicValueEnum<'ctx>, values::BasicValueEnum<'ctx>),
    ByRef(values::PointerValue<'ctx>),
}

impl<'ctx> Value<'ctx> {
    pub fn new(layout: Arc<Layout>, value: values::BasicValueEnum<'ctx>) -> Self {
        Self {
            layout,
            kind: ValueKind::Value(value),
        }
    }

    pub fn new_ref(layout: Arc<Layout>, value: values::PointerValue<'ctx>) -> Self {
        Self {
            layout,
            kind: ValueKind::ByRef(value),
        }
    }

    pub fn deref(&self, ctx: &mut CodegenCtx<'_, 'ctx>) -> Self {
        let value = self.load(ctx).into_pointer_value();
        let layout = self.layout.elem(ctx.db).unwrap();

        Self::new_ref(layout, value)
    }

    pub fn field(&self, ctx: &mut CodegenCtx<'_, 'ctx>, field: usize) -> Self {
        let layout = self.layout.field(ctx.db, field).unwrap();

        match self.kind {
            | ValueKind::Value(_) => todo!(),
            | ValueKind::ValuePair(a, b) => match self.layout.abi {
                | Abi::ScalarPair(_, _) => {
                    let val = match field {
                        | 0 => a,
                        | 1 => b,
                        | _ => unreachable!(),
                    };

                    Self::new(layout, val)
                },
                | _ => unreachable!(),
            },
            | ValueKind::ByRef(ptr) => {
                // let offset = self.layout.fields.offset(field).bytes();
                let ptr = ctx.builder.build_struct_gep(ptr, field as u32, "").unwrap();

                Self::new_ref(layout, ptr)
            },
        }
    }

    pub fn downcast(&self, ctx: &mut CodegenCtx<'_, 'ctx>, _ctor: hir::Ctor) -> Self {
        todo!()
    }

    pub fn load(&self, ctx: &mut CodegenCtx<'_, 'ctx>) -> values::BasicValueEnum<'ctx> {
        match self.kind {
            | ValueKind::Value(value) => value,
            | ValueKind::ByRef(ptr) => ctx.builder.build_load(ptr, ""),
            | _ => unreachable!(),
        }
    }

    pub fn load_pair(
        &self,
        ctx: &mut CodegenCtx<'_, 'ctx>,
    ) -> (values::BasicValueEnum<'ctx>, values::BasicValueEnum<'ctx>) {
        match self.kind {
            | ValueKind::ValuePair(a, b) => (a, b),
            | ValueKind::ByRef(_ptr) => todo!(),
            | _ => unreachable!(),
        }
    }
}
