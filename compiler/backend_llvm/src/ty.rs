use inkwell::types::{self, BasicType};
use inkwell::AddressSpace;
use mir::repr::{Integer, Primitive, Repr, Scalar, Signature};

use crate::ctx::CodegenCtx;
use crate::layout::{Abi, Fields, Layout};

impl<'ctx> CodegenCtx<'_, 'ctx> {
    pub fn fn_type_for_signature(&self, signature: &Signature) -> types::FunctionType<'ctx> {
        let ret = self.basic_type_for_repr(&signature.ret);
        let args = signature
            .params
            .iter()
            .map(|a| self.basic_type_for_repr(a).into())
            .collect::<Vec<_>>();

        ret.fn_type(&args, false)
    }

    pub fn basic_type_for_repr(&self, repr: &Repr) -> types::BasicTypeEnum<'ctx> {
        match repr {
            | Repr::Opaque => unreachable!(),
            | Repr::ReprOf(ty) => self.basic_type_for_repr(&self.db.repr_of(*ty)),
            | Repr::Scalar(scalar) => self.basic_type_for_scalar(scalar),
            | Repr::Func(sig, false) => self
                .fn_type_for_signature(sig)
                .ptr_type(AddressSpace::Global)
                .as_basic_type_enum(),
            | Repr::Func(_sig, true) => todo!(),
            | Repr::Ptr(to) => self
                .basic_type_for_repr(to)
                .ptr_type(AddressSpace::Generic)
                .as_basic_type_enum(),
            | Repr::Box(to) => self
                .basic_type_for_repr(to)
                .ptr_type(AddressSpace::Generic)
                .as_basic_type_enum(),
            | _ => {
                let layout = crate::layout::layout_of(self.db, repr);
                self.basic_type_for_layout(&layout)
            },
        }
    }

    pub fn basic_type_for_layout(&self, layout: &Layout) -> types::BasicTypeEnum<'ctx> {
        match &layout.abi {
            | Abi::Uninhabited => unreachable!(),
            | Abi::Scalar(scalar) => self.basic_type_for_scalar(scalar),
            | Abi::ScalarPair(a, b) => {
                let fields = [self.basic_type_for_scalar(a), self.basic_type_for_scalar(b)];
                self.context.struct_type(&fields, false).as_basic_type_enum()
            },
            | Abi::Aggregate { sized: true } => match &layout.fields {
                | Fields::Primitive => unreachable!(),
                | Fields::Array { .. } => todo!(),
                | Fields::Arbitrary { fields } => {
                    let fields = fields
                        .iter()
                        .map(|(_, f)| self.basic_type_for_layout(f))
                        .collect::<Vec<_>>();

                    self.context.struct_type(&fields, false).as_basic_type_enum()
                },
                | Fields::Union { .. } => todo!(),
            },
            | Abi::Aggregate { sized: false } => todo!(),
        }
    }

    pub fn basic_type_for_scalar(&self, scalar: &Scalar) -> types::BasicTypeEnum<'ctx> {
        match scalar.value {
            | Primitive::Int(Integer::Int, _) => self
                .context
                .ptr_sized_int_type(&self.target_data, None)
                .as_basic_type_enum(),
            | Primitive::Int(Integer::I8, _) => self.context.i8_type().as_basic_type_enum(),
            | Primitive::Int(Integer::I16, _) => self.context.i16_type().as_basic_type_enum(),
            | Primitive::Int(Integer::I32, _) => self.context.i32_type().as_basic_type_enum(),
            | Primitive::Int(Integer::I64, _) => self.context.i64_type().as_basic_type_enum(),
            | Primitive::Int(Integer::I128, _) => self.context.i128_type().as_basic_type_enum(),
            | Primitive::Float => self.context.f32_type().as_basic_type_enum(),
            | Primitive::Double => self.context.f64_type().as_basic_type_enum(),
            | Primitive::Pointer => self
                .context
                .ptr_sized_int_type(&self.target_data, Some(AddressSpace::Generic))
                .as_basic_type_enum(),
        }
    }
}
