mod expr;
mod item;
mod ty;
mod unify;
mod verify;

use crate::constraint::*;
use crate::layout::Layout;
use crate::layout::*;
use crate::ty::*;
use diagnostics::{Reporter, Span};
use std::cell::{Cell, RefCell};
use std::collections::BTreeMap;

pub struct Tcx<'tcx> {
    reporter: &'tcx Reporter,
    pub(crate) arena: &'tcx bumpalo::Bump,
    package: &'tcx hir::Package,
    pub(crate) target: &'tcx target_lexicon::Triple,
    types: RefCell<BTreeMap<hir::Id, Ty<'tcx>>>,
    layouts: RefCell<BTreeMap<*const Type<'tcx>, TyLayout<'tcx, Ty<'tcx>>>>,
    constraints: RefCell<Constraints<'tcx>>,
    ty_vars: Cell<usize>,
    pub builtin: BuiltinTypes<'tcx>,
}

pub struct BuiltinTypes<'tcx> {
    pub error: Ty<'tcx>,
    pub never: Ty<'tcx>,
    pub unit: Ty<'tcx>,
    pub bool: Ty<'tcx>,
    pub str: Ty<'tcx>,
    pub typeid: Ty<'tcx>,
    pub u8: Ty<'tcx>,
    pub u16: Ty<'tcx>,
    pub u32: Ty<'tcx>,
    pub u64: Ty<'tcx>,
    pub u128: Ty<'tcx>,
    pub usize: Ty<'tcx>,
    pub i8: Ty<'tcx>,
    pub i16: Ty<'tcx>,
    pub i32: Ty<'tcx>,
    pub i64: Ty<'tcx>,
    pub i128: Ty<'tcx>,
    pub isize: Ty<'tcx>,
    pub f32: Ty<'tcx>,
    pub f64: Ty<'tcx>,
    pub ref_u8: Ty<'tcx>,
}

impl<'tcx> BuiltinTypes<'tcx> {
    pub fn new(arena: &'tcx bumpalo::Bump) -> Self {
        let u8 = arena.alloc(Type::UInt(8));

        BuiltinTypes {
            error: arena.alloc(Type::Error),
            never: arena.alloc(Type::Never),
            unit: arena.alloc(Type::Tuple(&[])),
            bool: arena.alloc(Type::Bool),
            str: arena.alloc(Type::Str),
            typeid: arena.alloc(Type::TypeId),
            u8,
            u16: arena.alloc(Type::UInt(16)),
            u32: arena.alloc(Type::UInt(32)),
            u64: arena.alloc(Type::UInt(64)),
            u128: arena.alloc(Type::UInt(128)),
            usize: arena.alloc(Type::UInt(0)),
            i8: arena.alloc(Type::Int(8)),
            i16: arena.alloc(Type::Int(16)),
            i32: arena.alloc(Type::Int(32)),
            i64: arena.alloc(Type::Int(64)),
            i128: arena.alloc(Type::Int(128)),
            isize: arena.alloc(Type::Int(0)),
            f32: arena.alloc(Type::Float(32)),
            f64: arena.alloc(Type::Float(64)),
            ref_u8: arena.alloc(Type::Ref(false, u8)),
        }
    }
}

impl<'tcx> Tcx<'tcx> {
    pub fn new(
        reporter: &'tcx Reporter,
        arena: &'tcx bumpalo::Bump,
        target: &'tcx target_lexicon::Triple,
        package: &'tcx hir::Package,
    ) -> Self {
        Tcx {
            reporter,
            arena,
            target,
            package,
            types: RefCell::new(BTreeMap::new()),
            layouts: RefCell::new(BTreeMap::new()),
            constraints: RefCell::new(Constraints::new()),
            builtin: BuiltinTypes::new(arena),
            ty_vars: Cell::new(0),
        }
    }

    pub fn type_of(&self, id: &hir::Id) -> Ty<'tcx> {
        let types = self.types.borrow();

        if let Some(ty) = types.get(id) {
            ty
        } else {
            std::mem::drop(types);

            let ty = if let Some(_) = self.package.exprs.get(id) {
                self.infer_expr(id)
            } else if let Some(_) = self.package.types.get(id) {
                self.infer_type(id)
            } else if let Some(_) = self.package.items.get(id) {
                let ty = self.infer_item(id);

                self.types.borrow_mut().insert(*id, ty);
                self.check_item(id);
                self.types.borrow()[id]
            } else {
                panic!("unused id {}", id);
            };

            self.types.borrow_mut().insert(*id, ty);

            ty
        }
    }

    pub fn span_of(&self, id: &hir::Id) -> Span {
        if let Some(expr) = self.package.exprs.get(id) {
            expr.span
        } else if let Some(ty) = self.package.types.get(id) {
            ty.span
        } else if let Some(item) = self.package.items.get(id) {
            item.span
        } else {
            panic!("unused id {}", id);
        }
    }

    pub fn constrain(&self, cs: Constraint<'tcx>) {
        self.constraints.borrow_mut().push(cs);
    }

    pub fn new_var(&self) -> Ty<'tcx> {
        let var = self.ty_vars.get();

        self.ty_vars.set(var + 1);
        self.intern_ty(Type::Var(TypeVar(var)))
    }

    pub fn new_int(&self) -> Ty<'tcx> {
        let var = self.ty_vars.get();

        self.ty_vars.set(var + 1);
        self.intern_ty(Type::VInt(TypeVar(var)))
    }

    pub fn new_uint(&self) -> Ty<'tcx> {
        let var = self.ty_vars.get();

        self.ty_vars.set(var + 1);
        self.intern_ty(Type::VUInt(TypeVar(var)))
    }

    pub fn new_float(&self) -> Ty<'tcx> {
        let var = self.ty_vars.get();

        self.ty_vars.set(var + 1);
        self.intern_ty(Type::VFloat(TypeVar(var)))
    }

    pub fn get_full_name(&self, id: &hir::Id) -> String {
        unimplemented!();
    }

    pub fn layout_of(&self, id: &hir::Id) -> TyLayout<'tcx, Ty<'tcx>> {
        self.layout(self.type_of(id))
    }

    pub fn layout(&self, ty: Ty<'tcx>) -> TyLayout<'tcx, Ty<'tcx>> {
        let layouts = self.layouts.borrow();

        if let Some(layout) = layouts.get(&(ty as *const _)) {
            return *layout;
        }

        std::mem::drop(layouts);

        let scalar_unit = |value: Primitive| {
            let bits = value.size(self.target).bits();
            assert!(bits <= 128);
            Scalar {
                value,
                valid_range: 0..=(!0 >> (128 - bits)),
            }
        };

        let scalar =
            |value: Primitive| self.intern_layout(Layout::scalar(scalar_unit(value), self.target));

        let layout = match ty {
            Type::Error => unreachable!(),
            Type::Var(_) => unreachable!(),
            Type::Param(_) => unreachable!(),
            Type::Forall(_, _) => unimplemented!(),
            Type::TypeOf(id) => return self.layout_of(id),
            Type::VInt(_) => match self.target.pointer_width() {
                Ok(target_lexicon::PointerWidth::U16) => scalar(Primitive::Int(Integer::I16, true)),
                Ok(target_lexicon::PointerWidth::U32) => scalar(Primitive::Int(Integer::I32, true)),
                Ok(target_lexicon::PointerWidth::U64) => scalar(Primitive::Int(Integer::I64, true)),
                Err(_) => scalar(Primitive::Int(Integer::I32, true)),
            },
            Type::VUInt(_) => unreachable!(),
            Type::VFloat(_) => unreachable!(),
            Type::Never => self.intern_layout(Layout {
                fields: FieldsShape::Primitive,
                variants: Variants::Single { index: 0 },
                largest_niche: None,
                abi: Abi::Uninhabited,
                size: Size::ZERO,
                align: Align::from_bits(8),
                stride: Size::ZERO,
            }),
            Type::Bool => self.intern_layout(Layout::scalar(
                Scalar {
                    value: Primitive::Int(Integer::I8, false),
                    valid_range: 0..=1,
                },
                self.target,
            )),
            Type::Int(0) => match self.target.pointer_width() {
                Ok(target_lexicon::PointerWidth::U16) => scalar(Primitive::Int(Integer::I16, true)),
                Ok(target_lexicon::PointerWidth::U32) => scalar(Primitive::Int(Integer::I32, true)),
                Ok(target_lexicon::PointerWidth::U64) => scalar(Primitive::Int(Integer::I64, true)),
                Err(_) => scalar(Primitive::Int(Integer::I32, true)),
            },
            Type::UInt(0) => match self.target.pointer_width() {
                Ok(target_lexicon::PointerWidth::U16) => {
                    scalar(Primitive::Int(Integer::I16, false))
                }
                Ok(target_lexicon::PointerWidth::U32) => {
                    scalar(Primitive::Int(Integer::I32, false))
                }
                Ok(target_lexicon::PointerWidth::U64) => {
                    scalar(Primitive::Int(Integer::I64, false))
                }
                Err(_) => scalar(Primitive::Int(Integer::I32, false)),
            },
            Type::Float(0) => match self.target.pointer_width() {
                Ok(target_lexicon::PointerWidth::U32) => scalar(Primitive::F32),
                Ok(target_lexicon::PointerWidth::U64) => scalar(Primitive::F64),
                _ => scalar(Primitive::F32),
            },
            Type::Int(8) => scalar(Primitive::Int(Integer::I8, true)),
            Type::Int(16) => scalar(Primitive::Int(Integer::I16, true)),
            Type::Int(32) => scalar(Primitive::Int(Integer::I32, true)),
            Type::Int(64) => scalar(Primitive::Int(Integer::I64, true)),
            Type::Int(128) => scalar(Primitive::Int(Integer::I128, true)),
            Type::Int(_) => unreachable!(),
            Type::UInt(8) => scalar(Primitive::Int(Integer::I8, false)),
            Type::UInt(16) => scalar(Primitive::Int(Integer::I16, false)),
            Type::UInt(32) => scalar(Primitive::Int(Integer::I32, false)),
            Type::UInt(64) => scalar(Primitive::Int(Integer::I64, false)),
            Type::UInt(128) => scalar(Primitive::Int(Integer::I128, false)),
            Type::UInt(_) => unreachable!(),
            Type::Float(32) => scalar(Primitive::F32),
            Type::Float(64) => scalar(Primitive::F64),
            Type::Float(_) => unreachable!(),
            Type::Str => {
                let mut data_ptr = scalar_unit(Primitive::Pointer);

                data_ptr.valid_range = 1..=*data_ptr.valid_range.end();

                let metadata = scalar_unit(Primitive::Int(Integer::ptr_sized(self.target), false));

                self.intern_layout(self.scalar_pair(data_ptr, metadata))
            }
            Type::TypeId => match self.target.pointer_width() {
                Ok(target_lexicon::PointerWidth::U16) => {
                    scalar(Primitive::Int(Integer::I16, false))
                }
                Ok(target_lexicon::PointerWidth::U32) => {
                    scalar(Primitive::Int(Integer::I32, false))
                }
                Ok(target_lexicon::PointerWidth::U64) => {
                    scalar(Primitive::Int(Integer::I64, false))
                }
                Err(_) => scalar(Primitive::Int(Integer::I32, false)),
            },
            Type::Ref(_, _) => {
                let data_ptr = scalar_unit(Primitive::Pointer);

                self.intern_layout(Layout::scalar(data_ptr, self.target))
            }
            Type::Array(of, len) => {
                let of_layout = self.layout(of);
                let size = of_layout.stride * (*len as u64);
                let largest_niche = if *len != 0 {
                    of_layout.largest_niche.clone()
                } else {
                    None
                };

                self.intern_layout(Layout {
                    size,
                    align: of_layout.align,
                    stride: size,
                    abi: Abi::Aggregate { sized: true },
                    fields: FieldsShape::Array {
                        stride: of_layout.stride,
                        count: *len as u64,
                    },
                    variants: Variants::Single { index: 0 },
                    largest_niche,
                })
            }
            Type::Slice(_) => {
                let mut data_ptr = scalar_unit(Primitive::Pointer);

                data_ptr.valid_range = 1..=*data_ptr.valid_range.end();

                let metadata = scalar_unit(Primitive::Int(Integer::ptr_sized(self.target), false));

                self.intern_layout(self.scalar_pair(data_ptr, metadata))
            }
            Type::Tuple(tys) => self
                .intern_layout(self.struct_layout(tys.iter().map(|ty| self.layout(ty)).collect())),
            Type::Struct(_, fields) => self.intern_layout(
                self.struct_layout(fields.iter().map(|f| self.layout(f.ty)).collect()),
            ),
            Type::Func(_, _) => {
                let mut ptr = scalar_unit(Primitive::Pointer);

                ptr.valid_range = 1..=*ptr.valid_range.end();
                self.intern_layout(Layout::scalar(ptr, self.target))
            }
            Type::Enum(_, variants) => {
                let variants = variants
                    .iter()
                    .map(|v| {
                        self.struct_layout(
                            v.fields
                                .iter()
                                .map(|f| self.layout(f.ty))
                                .collect::<Vec<_>>(),
                        )
                    })
                    .collect();

                self.intern_layout(self.enum_layout(variants))
            }
        };

        let layout = TyLayout { ty, layout };

        self.layouts.borrow_mut().insert(ty as *const _, layout);

        layout
    }

    fn scalar_pair(&self, a: Scalar, b: Scalar) -> Layout {
        let b_align = b.value.align(self.target);
        let b_offset = a.value.size(self.target).align_to(b_align);
        let align = a.value.align(self.target).max(b_align);
        let size = a.value.size(self.target) + b.value.size(self.target);
        let stride = (b_offset + b.value.size(self.target)).align_to(align);
        let largest_niche = Niche::from_scalar(self.target, b_offset, b.clone())
            .into_iter()
            .chain(Niche::from_scalar(self.target, Size::ZERO, a.clone()))
            .max_by_key(|niche| niche.available(self.target));

        Layout {
            fields: FieldsShape::Arbitrary {
                offsets: vec![Size::ZERO, b_offset],
            },
            variants: Variants::Single { index: 0 },
            largest_niche,
            abi: Abi::ScalarPair(a, b),
            align,
            size,
            stride,
        }
    }

    fn struct_layout(&self, fields: Vec<TyLayout<'tcx, Ty<'tcx>>>) -> Layout {
        // TODO: optimize layout
        let mut align = Align::from_bytes(1);
        let mut offsets = vec![Size::ZERO; fields.len()];
        let mut offset = Size::ZERO;
        let mut niches = Vec::new();

        for i in 0..fields.len() {
            let field = fields[i];

            if let Some(niche) = field.largest_niche.clone() {
                niches.push(niche);
            }

            let field_align = field.align;

            offset = offset.align_to(field_align);
            align = align.max(field_align);
            offsets[i] = offset;
            offset = offset + field.size;
        }

        let size = offset;
        let stride = offset.align_to(align);
        let abi = Abi::Aggregate { sized: true };
        let largest_niche = niches
            .into_iter()
            .max_by_key(|niche| niche.available(self.target));

        Layout {
            fields: FieldsShape::Arbitrary { offsets },
            variants: Variants::Single { index: 0 },
            largest_niche,
            abi,
            align,
            size,
            stride,
        }
    }

    fn enum_layout(&self, mut variants: Vec<Layout>) -> Layout {
        if variants.is_empty() {
            Layout {
                fields: FieldsShape::Arbitrary {
                    offsets: Vec::new(),
                },
                variants: Variants::Single { index: 0 },
                largest_niche: None,
                abi: Abi::Aggregate { sized: true },
                size: Size::ZERO,
                align: Align::from_bytes(1),
                stride: Size::ZERO,
            }
        } else if variants.len() == 1 {
            variants.pop().unwrap()
        } else {
            let largest_niche = variants
                .iter()
                .filter_map(|v| v.largest_niche.clone())
                .max_by_key(|niche| niche.available(self.target));

            for (i, variant) in variants.iter_mut().enumerate() {
                variant.variants = Variants::Single { index: i };
            }

            let largest = variants.iter().max_by_key(|v| v.size).unwrap();
            let align = largest.align;
            let mut size = largest.size;
            let mut no_niche = |mut variants: Vec<Layout>| {
                let tag_size = Size::from_bits(variants.len()).align_to(align);
                let offsets = vec![Size::ZERO, tag_size];
                let tag = Scalar {
                    value: Primitive::Int(
                        match tag_size.bytes() {
                            1 => Integer::I8,
                            2 => Integer::I16,
                            4 => Integer::I32,
                            8 => Integer::I64,
                            _ => Integer::I128,
                        },
                        false,
                    ),
                    valid_range: 0..=u128::max_value(),
                };

                let tag_encoding = TagEncoding::Direct;

                size = size + tag_size;

                for variant in &mut variants {
                    if let FieldsShape::Arbitrary { offsets } = &mut variant.fields {
                        for offset in offsets {
                            *offset = *offset + tag_size;
                        }
                    }
                }

                (
                    FieldsShape::Arbitrary { offsets },
                    Variants::Multiple {
                        tag,
                        tag_encoding,
                        tag_field: 0,
                        variants,
                    },
                )
            };

            let (fields, variants) = if let Some(niche) = largest_niche {
                if niche.available(self.target) >= variants.len() as u128 {
                    // unimplemented!();
                    no_niche(variants) // TODO: implement niches
                } else {
                    no_niche(variants)
                }
            } else {
                no_niche(variants)
            };

            let stride = size.align_to(align);

            Layout {
                fields,
                variants,
                largest_niche: None,
                abi: Abi::Aggregate { sized: true },
                size,
                align,
                stride,
            }
        }
    }

    pub fn intern_ty(&self, ty: Type<'tcx>) -> Ty<'tcx> {
        self.arena.alloc(ty)
    }

    pub(crate) fn intern_layout(&self, layout: Layout) -> &'tcx Layout {
        self.arena.alloc(layout)
    }
}
