use inkwell::types::{self, BasicType};
use inkwell::AddressSpace;
use mir::ir::{Operand, Place, PlaceRef, Projection};
use mir::repr::{Integer, Primitive, Repr, ReprKind, Scalar, Signature};

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
            | Ok(ret) => ret.fn_type(&args, fn_abi.is_varargs),
            | Err(ret) => ret.fn_type(&args, fn_abi.is_varargs),
        }
    }

    pub fn fn_type_for_signature(&self, sig: &Signature, thick: bool) -> types::FunctionType<'ctx> {
        let abi = self.compute_fn_abi(sig, thick);
        self.fn_type_for_abi(&abi)
    }

    pub fn basic_type_for_ral(&self, layout: &ReprAndLayout) -> types::BasicTypeEnum<'ctx> {
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
                return match layout.repr.kind(self.db) {
                    | ReprKind::ReprOf(ty) => self.basic_type_for_ral(&ReprAndLayout {
                        repr: mir::repr::repr_of(self.db, *ty, mir::repr::ReprPos::Argument),
                        layout: layout.layout.clone(),
                    }),
                    | ReprKind::Ptr(_, _, _) | ReprKind::Box(_) => self
                        .basic_type_for_ral(&layout.elem(self.db).unwrap())
                        .ptr_type(AddressSpace::default())
                        .as_basic_type_enum(),
                    | ReprKind::Func(sig, env) => self
                        .fn_type_for_signature(sig, *env)
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
                fill.as_basic_type_enum()
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

                let ty = self
                    .context
                    .opaque_struct_type(&format!("struct.{}", self.types.borrow().len()));

                ty.set_body(&fields, false);
                ty.as_basic_type_enum()
            },
        }
    }

    pub fn llvm_padding(&self, size: Size, _align: Align) -> types::BasicTypeEnum<'ctx> {
        let align = Align::from_bytes(size.bytes());
        let int = match align.bytes() {
            | 1 => Integer::I8,
            | 2 => Integer::I16,
            | 3 | 4 => Integer::I32,
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

    pub fn usize_type(&self) -> types::IntType<'ctx> {
        self.context.ptr_sized_int_type(&self.target_data, None)
    }

    pub fn ptr_type(&self) -> types::PointerType<'ctx> {
        self.context.i8_type().ptr_type(AddressSpace::default())
    }
}

impl<'ctx> BodyCtx<'_, '_, 'ctx> {
    pub fn place_layout(&self, place: &Place) -> ReprAndLayout {
        self.place_ref_layout(place.as_ref())
    }

    pub fn place_ref_layout(&self, place: PlaceRef) -> ReprAndLayout {
        let repr = self.instance.subst_repr(self.db, self.body.locals[place.local.0].repr);
        let mut base = repr_and_layout(self.db, repr);

        for proj in place.projection.iter() {
            base = match *proj {
                | Projection::Deref => base.elem(self.db).unwrap(),
                | Projection::Field(i) => base.field(self.db, i).unwrap(),
                | Projection::Index(_) => base.elem(self.db).unwrap(),
                | Projection::Slice(_, _) => {
                    let repr = Repr::new(self.db, ReprKind::Ptr(base.elem(self.db).unwrap().repr, true, false));
                    repr_and_layout(self.db, repr)
                },
                | Projection::Downcast(ctor) => {
                    let ctors = hir::Ctor::from(ctor).type_ctor(self.db).ctors(self.db);
                    let index = ctors.iter().position(|c| c.id() == ctor).unwrap();
                    base.variant(index)
                },
            };
        }

        base
    }

    pub fn operand_layout(&self, operand: &Operand) -> ReprAndLayout {
        match operand {
            | Operand::Copy(p) | Operand::Move(p) => self.place_layout(p),
            | Operand::Const(_, r) => {
                let r = self.instance.subst_repr(self.db, *r);
                repr_and_layout(self.db, r)
            },
        }
    }
}
