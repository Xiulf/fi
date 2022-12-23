use inkwell::types::{self, BasicType};
use inkwell::AddressSpace;
use mir::repr::{ArrayLen, Integer, Primitive, Repr, Scalar, Signature};
use mir::syntax::{Operand, Place, Projection};

use crate::abi::{FnAbi, PassMode};
use crate::ctx::{BodyCtx, CodegenCtx};
use crate::layout::{Abi, Fields, Layout, ReprAndLayout};

impl<'ctx> CodegenCtx<'_, 'ctx> {
    pub fn fn_type_for_abi(&self, fn_abi: &FnAbi<'ctx>) -> types::FunctionType<'ctx> {
        let mut args = Vec::new();
        let ret = match fn_abi.ret.mode {
            | PassMode::NoPass => Ok(self.context.void_type()),
            | PassMode::ByVal(ty) => Err(ty),
            | PassMode::ByValPair(a, b) => Err(self.context.struct_type(&[a, b], false).as_basic_type_enum()),
            | PassMode::ByRef { size: Some(_) } => {
                let ret_ty = self.basic_type_for_repr(&fn_abi.ret.layout.repr);
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
                    let ty = self.basic_type_for_repr(&arg.layout.repr);
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

    pub fn basic_type_for_repr(&self, repr: &Repr) -> types::BasicTypeEnum<'ctx> {
        match repr {
            | Repr::Opaque => self.context.i8_type().as_basic_type_enum(),
            | Repr::ReprOf(ty) => self.basic_type_for_repr(&self.db.repr_of(*ty)),
            | Repr::Scalar(scalar) => self.basic_type_for_scalar(scalar, None),
            | Repr::Func(sig, false) => self
                .fn_type_for_signature(sig)
                .ptr_type(AddressSpace::default())
                .as_basic_type_enum(),
            | Repr::Func(_sig, true) => todo!(),
            | Repr::Ptr(to, false, _) => self
                .basic_type_for_repr(to)
                .ptr_type(AddressSpace::default())
                .as_basic_type_enum(),
            | Repr::Box(to) => self
                .basic_type_for_repr(to)
                .ptr_type(AddressSpace::default())
                .as_basic_type_enum(),
            | Repr::Array(ArrayLen::Const(len), elem) => {
                let elem = self.basic_type_for_repr(elem);
                elem.array_type(*len as u32).as_basic_type_enum()
            },
            | Repr::Struct(fields) => {
                let layout = crate::layout::layout_of(self.db, repr);

                match layout.abi {
                    | Abi::Scalar(_) => self.basic_type_for_repr(&fields[0]),
                    | _ => {
                        let fields = fields.iter().map(|f| self.basic_type_for_repr(f)).collect::<Vec<_>>();

                        self.context.struct_type(&fields, false).as_basic_type_enum()
                    },
                }
            },
            | _ => {
                let layout = crate::layout::layout_of(self.db, repr);
                self.basic_type_for_layout(&layout)
            },
        }
    }

    pub fn basic_type_for_layout(&self, layout: &Layout) -> types::BasicTypeEnum<'ctx> {
        match &layout.abi {
            | Abi::Uninhabited => unreachable!(),
            | Abi::Scalar(scalar) => self.basic_type_for_scalar(scalar, layout.elem.as_ref()),
            | Abi::ScalarPair(a, b) => {
                let fields = [
                    self.basic_type_for_scalar(a, layout.elem.as_ref()),
                    self.basic_type_for_scalar(b, layout.elem.as_ref()),
                ];
                self.context.struct_type(&fields, false).as_basic_type_enum()
            },
            | Abi::Aggregate { sized: true } => match &layout.fields {
                | Fields::Primitive => unreachable!(),
                | Fields::Array { count, .. } => {
                    let elem = layout.elem.as_ref().unwrap();
                    let elem = self.basic_type_for_repr(elem);
                    elem.array_type(*count as u32).as_basic_type_enum()
                },
                | Fields::Arbitrary { fields } if fields.is_empty() => {
                    self.context.struct_type(&[], false).as_basic_type_enum()
                },
                | Fields::Arbitrary { fields } => {
                    let min_offset = fields.iter().map(|f| f.0).min().unwrap();
                    let padding = if min_offset.bytes() > 0 {
                        Some(
                            self.context
                                .i8_type()
                                .array_type(min_offset.bytes() as u32)
                                .as_basic_type_enum(),
                        )
                    } else {
                        None
                    };

                    let fields = padding
                        .into_iter()
                        .chain(fields.iter().map(|(_, f)| self.basic_type_for_layout(f)))
                        .collect::<Vec<_>>();

                    self.context.struct_type(&fields, false).as_basic_type_enum()
                },
                | Fields::Union { .. } => match layout.size.bytes() {
                    | 0 => self.context.struct_type(&[], false).as_basic_type_enum(),
                    | 1 => self.context.i8_type().as_basic_type_enum(),
                    | 2 => self.context.i16_type().as_basic_type_enum(),
                    | 4 => self.context.i32_type().as_basic_type_enum(),
                    | 8 => self.context.i64_type().as_basic_type_enum(),
                    | 16 => self.context.i128_type().as_basic_type_enum(),
                    | s => self.context.i8_type().array_type(s as u32).as_basic_type_enum(),
                },
            },
            | Abi::Aggregate { sized: false } => todo!(),
        }
    }

    pub fn basic_type_for_scalar(&self, scalar: &Scalar, elem: Option<&Repr>) -> types::BasicTypeEnum<'ctx> {
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
            | Primitive::Pointer => elem
                .map(|e| {
                    self.basic_type_for_repr(e)
                        .ptr_type(AddressSpace::default())
                        .as_basic_type_enum()
                })
                .unwrap_or_else(|| {
                    self.context
                        .ptr_sized_int_type(&self.target_data, Some(AddressSpace::default()))
                        .as_basic_type_enum()
                }),
        }
    }
}

impl<'ctx> BodyCtx<'_, '_, 'ctx> {
    pub fn place_layout(&self, place: &Place) -> ReprAndLayout {
        let repr = self.body.locals[place.local.0].repr.clone();
        let mut base = crate::layout::repr_and_layout(self.db, repr);

        for proj in place.projection.iter() {
            base = match *proj {
                | Projection::Deref => base.elem(self.db).unwrap(),
                | Projection::Field(i) => base.field(self.db, i).unwrap(),
                | Projection::Index(_) => base.elem(self.db).unwrap(),
                | Projection::Downcast(_) => todo!(),
            };
        }

        base
    }

    pub fn operand_layout(&self, operand: &Operand) -> ReprAndLayout {
        match operand {
            | Operand::Copy(p) | Operand::Move(p) => self.place_layout(p),
            | Operand::Const(_, r) => crate::layout::repr_and_layout(self.db, r.clone()),
        }
    }
}
