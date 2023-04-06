use inkwell::types::{self, BasicType};
use inkwell::AddressSpace;
use mir::ir::{Operand, Place, Projection};
use mir::repr::{Integer, Primitive, Repr, Scalar, Signature};
use triomphe::Arc;

use crate::abi::{FnAbi, PassMode};
use crate::ctx::{BodyCtx, CodegenCtx};
use crate::layout::{integer_size, repr_and_layout, Abi, Align, Fields, ReprAndLayout, Size};

impl<'ctx> CodegenCtx<'_, 'ctx> {
    pub fn fn_type_for_abi(&self, fn_abi: &FnAbi<'ctx>) -> types::FunctionType<'ctx> {
        let mut args = Vec::new();
        let ret = match fn_abi.ret.mode {
            | PassMode::NoPass => Ok(self.context.void_type()),
            | PassMode::ByVal(ty) => Err(ty),
            | PassMode::ByValPair(a, b) => Err(self.context.struct_type(&[a, b], false).as_basic_type_enum()),
            | PassMode::ByRef { size: Some(_) } => {
                let ret_ty = self.basic_type_for_ral(&fn_abi.ret.layout);
                args.push(ret_ty.ptr_type(AddressSpace::default()).as_basic_type_enum().into());
                Ok(self.context.void_type())
            },
            | PassMode::ByRef { size: None } => todo!(),
        };

        for arg in fn_abi.args.iter() {
            match arg.mode {
                | PassMode::NoPass => {},
                | PassMode::ByVal(ty) => args.push(ty.into()),
                | PassMode::ByValPair(a, b) => {
                    args.push(a.into());
                    args.push(b.into());
                },
                | PassMode::ByRef { size: Some(_) } => {
                    let ty = self.basic_type_for_ral(&arg.layout);
                    args.push(ty.ptr_type(AddressSpace::default()).as_basic_type_enum().into());
                },
                | PassMode::ByRef { size: None } => todo!(),
            }
        }

        match ret {
            | Ok(ret) => ret.fn_type(&args, false),
            | Err(ret) => ret.fn_type(&args, false),
        }
    }

    pub fn fn_type_for_signature(&self, sig: &Signature) -> types::FunctionType<'ctx> {
        let abi = self.compute_fn_abi(sig);
        self.fn_type_for_abi(&abi)
    }

    pub fn basic_type_for_ral(&self, layout: &Arc<ReprAndLayout>) -> types::BasicTypeEnum<'ctx> {
        if let Some(ty) = self.types.borrow().get(layout) {
            return *ty;
        }

        let ty = self.basic_type_for_ral_uncached(layout);
        self.types.borrow_mut().insert(layout.clone(), ty);
        ty
    }

    fn basic_type_for_ral_uncached(&self, layout: &ReprAndLayout) -> types::BasicTypeEnum<'ctx> {
        match layout.abi {
            | Abi::Uninhabited => return self.context.i8_type().as_basic_type_enum(),
            | Abi::Scalar(ref scalar) => {
                return match &*layout.repr {
                    | Repr::Ptr(to, _, _) | Repr::Box(to) => self
                        .basic_type_for_ral(&repr_and_layout(self.db, to.clone()))
                        .ptr_type(AddressSpace::default())
                        .as_basic_type_enum(),
                    | Repr::Func(sig, _) => self
                        .fn_type_for_signature(sig)
                        .ptr_type(AddressSpace::default())
                        .as_basic_type_enum(),
                    | _ => self.basic_type_for_scalar(scalar),
                };
            },
            | Abi::ScalarPair(..) => {
                let a = self.basic_type_for_ral(&layout.field(self.db, 0).unwrap());
                let b = self.basic_type_for_ral(&layout.field(self.db, 1).unwrap());

                return self.context.struct_type(&[a, b], false).as_basic_type_enum();
            },
            | _ => {},
        }

        match layout.fields {
            | Fields::Primitive | Fields::Union(_) => {
                let fill = self.llvm_padding(layout.size, layout.align);
                self.context.struct_type(&[fill], false).as_basic_type_enum()
            },
            | Fields::Array { count, .. } => {
                let elem = self.basic_type_for_ral(&layout.elem(self.db).unwrap());
                elem.array_type(count as u32).as_basic_type_enum()
            },
            | Fields::Arbitrary { ref offsets } => {
                let mut offset = Size::ZERO;
                let mut field_remapping = Vec::with_capacity(offsets.len());
                let mut fields = Vec::with_capacity(offsets.len() * 2);
                let mut prev_align = layout.align;

                for (i, &field_offset) in offsets.iter().enumerate() {
                    let field = layout.field(self.db, i).unwrap();
                    let field_align = prev_align.min(field.align).restrict_for_offset(field_offset);
                    let padding = field_offset - offset;

                    if padding != Size::ZERO {
                        let align = prev_align.min(field_align);
                        fields.push(self.llvm_padding(padding, align));
                    }

                    field_remapping.push(fields.len());
                    fields.push(self.basic_type_for_ral(&field));
                    offset = field_offset + field.size;
                    prev_align = field_align;
                }

                if !layout.abi.is_unsized() && offsets.len() > 0 {
                    let padding = layout.stride - offset;

                    if padding != Size::ZERO {
                        fields.push(self.llvm_padding(padding, prev_align));
                    }
                }

                self.context.struct_type(&fields, false).as_basic_type_enum()
            },
        }
    }

    pub fn llvm_padding(&self, size: Size, align: Align) -> types::BasicTypeEnum<'ctx> {
        let int = match align.bytes() {
            | 1 => Integer::I8,
            | 2 => Integer::I16,
            | 3..=4 => Integer::I32,
            | _ => Integer::I64,
        };

        let size = size.bytes();
        let int_size = integer_size(int, &self.target.triple).bytes();
        assert_eq!(size % int_size, 0);
        self.basic_type_for_integer(int)
            .array_type((size / int_size) as u32)
            .as_basic_type_enum()
    }

    pub fn basic_type_for_scalar(&self, scalar: &Scalar) -> types::BasicTypeEnum<'ctx> {
        match scalar.value {
            | Primitive::Int(Integer::I8, _) if scalar.valid_range == (0..=1) => {
                self.context.bool_type().as_basic_type_enum()
            },
            | Primitive::Int(int, _) => self.basic_type_for_integer(int).as_basic_type_enum(),
            | Primitive::Float => self.context.f32_type().as_basic_type_enum(),
            | Primitive::Double => self.context.f64_type().as_basic_type_enum(),
            | Primitive::Pointer => self
                .context
                .i8_type()
                .ptr_type(AddressSpace::default())
                .as_basic_type_enum(),
        }
    }

    pub fn basic_type_for_integer(&self, int: Integer) -> types::IntType<'ctx> {
        match int {
            | Integer::I8 => self.context.i8_type(),
            | Integer::I16 => self.context.i16_type(),
            | Integer::I32 => self.context.i32_type(),
            | Integer::I64 => self.context.i64_type(),
            | Integer::I128 => self.context.i128_type(),
            | Integer::Int => self.context.ptr_sized_int_type(&self.target_data, None),
        }
    }
}

impl<'ctx> BodyCtx<'_, '_, 'ctx> {
    pub fn place_layout(&self, place: &Place) -> Arc<ReprAndLayout> {
        let repr = self.body.locals(self.db)[place.local.0].repr.clone();
        // let repr = self.instance.subst_repr(self.db, repr);
        let mut base = repr_and_layout(self.db, repr);

        for proj in place.projection.iter() {
            base = match *proj {
                | Projection::Deref => base.elem(self.db).unwrap(),
                | Projection::Field(i) => base.field(self.db, i).unwrap(),
                | Projection::Index(_) => base.elem(self.db).unwrap(),
                | Projection::Slice(_, _) => {
                    let repr = Arc::new(Repr::Ptr(base.elem(self.db).unwrap().repr.clone(), true, false));
                    repr_and_layout(self.db, repr)
                },
                | Projection::Downcast(_) => todo!(),
            };
        }

        base
    }

    pub fn operand_layout(&self, operand: &Operand) -> Arc<ReprAndLayout> {
        match operand {
            | Operand::Copy(p) | Operand::Move(p) => self.place_layout(p),
            | Operand::Const(_, r) => {
                let r = r.clone();
                // let r = self.instance.subst_repr(self.db, r);
                repr_and_layout(self.db, r)
            },
        }
    }
}
