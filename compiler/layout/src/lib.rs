#![feature(label_break_value)]

use check::ty::*;
use std::convert::{TryFrom, TryInto};
use std::ops::{Add, Mul, RangeInclusive};
use std::sync::Arc;

#[salsa::query_group(LayoutDatabaseStorage)]
pub trait LayoutDatabase: check::TypeDatabase + ToLayoutDb {
    fn layout_of(&self, lib: source::LibId, ty: Ty) -> TyLayout<Ty>;
}

pub trait ToLayoutDb {
    fn to_layout_db(&self) -> &dyn LayoutDatabase;
}

impl<T: LayoutDatabase> ToLayoutDb for T {
    fn to_layout_db(&self) -> &dyn LayoutDatabase {
        self
    }
}

fn layout_of(db: &dyn LayoutDatabase, lib: source::LibId, ty: Ty) -> TyLayout<Ty> {
    let scalar_unit = |value: Primitive| {
        let bits = value.size(&db.target(lib)).bits();
        assert!(bits <= 128);
        Scalar {
            value,
            valid_range: 0..=(!0 >> (128 - bits)),
        }
    };

    let scalar = |value: Primitive| Layout::scalar(scalar_unit(value), &db.target(lib));

    let layout = match &*ty {
        Type::Error => unreachable!(),
        Type::Int(_) => unreachable!(),
        Type::Infer(_) => unreachable!(),
        Type::Var(_) => Layout {
            size: Size::ZERO,
            align: Align::from_bytes(1),
            stride: Size::ZERO,
            abi: Abi::Aggregate { sized: false },
            fields: FieldsShape::Arbitrary {
                offsets: Vec::new(),
            },
            variants: Variants::Single { index: 0 },
            largest_niche: None,
        },
        Type::TypeOf(id) => return db.layout_of(lib, db.typecheck(*id).ty.clone()),
        Type::ForAll(_, ty) => return db.layout_of(lib, ty.clone()),
        Type::Func(_, _) => {
            let mut ptr = scalar_unit(Primitive::Pointer);

            ptr.valid_range = 1..=*ptr.valid_range.end();
            Layout::scalar(ptr, &db.target(lib))
        }
        Type::Tuple(tys) => struct_layout(
            db,
            lib,
            tys.into_iter().map(|t| db.layout_of(lib, t)).collect(),
        ),
        Type::Record(_, Some(_)) => unimplemented!(),
        Type::Record(fields, None) => struct_layout(
            db,
            lib,
            fields
                .into_iter()
                .map(|f| db.layout_of(lib, f.ty))
                .collect(),
        ),
        Type::App(base, args) => match &**base {
            Type::Data(def) => {
                if *def == db.lang_items().ptr_ty().owner {
                    scalar(Primitive::Pointer)
                } else if *def == db.lang_items().array_ty().owner {
                    let of_layout = db.layout_of(lib, args[0].clone());
                    let len = match &*args[1] {
                        Type::Int(i) => *i,
                        _ => unreachable!(),
                    };

                    let size = of_layout.stride * (len as u64);
                    let largest_niche = if len != 0 {
                        of_layout.largest_niche.clone()
                    } else {
                        None
                    };

                    Layout {
                        size,
                        align: of_layout.align,
                        stride: size,
                        abi: Abi::Aggregate { sized: true },
                        fields: FieldsShape::Array {
                            stride: of_layout.stride,
                            count: len as u64,
                        },
                        variants: Variants::Single { index: 0 },
                        largest_niche,
                    }
                } else if *def == db.lang_items().slice_ty().owner {
                    let mut data_ptr = scalar_unit(Primitive::Pointer);

                    data_ptr.valid_range = 1..=*data_ptr.valid_range.end();

                    let metadata =
                        scalar_unit(Primitive::Int(Integer::ptr_sized(&db.target(lib)), false));

                    scalar_pair(db, lib, data_ptr, metadata)
                } else {
                    return db.layout_of(lib, base.clone());
                }
            }
            _ => return db.layout_of(lib, base.clone()),
        },
        Type::Data(id) => 'block: {
            let file = db.module_tree(id.lib).file(id.module);
            let hir = db.module_hir(file);
            let def = hir.def(*id);

            if let hir::ir::Def::Item(item) = def {
                if let Some(repr) = item.repr() {
                    break 'block match repr {
                        "u8" => scalar(Primitive::Int(Integer::I8, false)),
                        "u16" => scalar(Primitive::Int(Integer::I16, false)),
                        "u32" => scalar(Primitive::Int(Integer::I32, false)),
                        "u64" => scalar(Primitive::Int(Integer::I64, false)),
                        "u128" => scalar(Primitive::Int(Integer::I128, false)),
                        "i8" => scalar(Primitive::Int(Integer::I8, true)),
                        "i16" => scalar(Primitive::Int(Integer::I16, true)),
                        "i32" => scalar(Primitive::Int(Integer::I32, true)),
                        "i64" => scalar(Primitive::Int(Integer::I64, true)),
                        "i128" => scalar(Primitive::Int(Integer::I128, true)),
                        _ => unreachable!("unknown repr"),
                    };
                }
            }

            let variants = db.variants(*id);

            data_layout(
                db,
                lib,
                variants
                    .into_iter()
                    .map(|v| {
                        struct_layout(
                            db,
                            lib,
                            v.tys.into_iter().map(|t| db.layout_of(lib, t)).collect(),
                        )
                    })
                    .collect(),
            )
        }
    };

    TyLayout {
        ty,
        layout: Arc::new(layout),
    }
}

fn scalar_pair(db: &dyn LayoutDatabase, lib: source::LibId, a: Scalar, b: Scalar) -> Layout {
    let target = db.target(lib);
    let b_align = b.value.align(&target);
    let b_offset = a.value.size(&target).align_to(b_align);
    let align = a.value.align(&target).max(b_align);
    let size = a.value.size(&target) + b.value.size(&target);
    let stride = (b_offset + b.value.size(&target)).align_to(align);
    let largest_niche = Niche::from_scalar(&target, b_offset, b.clone())
        .into_iter()
        .chain(Niche::from_scalar(&target, Size::ZERO, a.clone()))
        .max_by_key(|niche| niche.available(&target));

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

fn struct_layout(db: &dyn LayoutDatabase, lib: source::LibId, fields: Vec<TyLayout<Ty>>) -> Layout {
    let mut align = Align::from_bytes(1);
    let mut offsets = vec![Size::ZERO; fields.len()];
    let mut offset = Size::ZERO;
    let mut niches = Vec::new();

    for (i, field) in fields.into_iter().enumerate() {
        if let Some(niche) = field.largest_niche.clone() {
            niches.push(niche);
        }

        offset = offset.align_to(field.align);
        align = align.max(field.align);
        offsets[i] = offset;
        offset = offset + field.size;
    }

    let size = offset;
    let stride = offset.align_to(align);
    let abi = Abi::Aggregate { sized: true };
    let largest_niche = niches
        .into_iter()
        .max_by_key(|niche| niche.available(&db.target(lib)));

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

fn data_layout(db: &dyn LayoutDatabase, lib: source::LibId, mut variants: Vec<Layout>) -> Layout {
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
        let target = db.target(lib);
        let largest_niche = variants
            .iter()
            .filter_map(|v| v.largest_niche.clone())
            .max_by_key(|niche| niche.available(&target));

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
            if niche.available(&target) >= variants.len() as u128 {
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TyLayout<Ty> {
    pub ty: Ty,
    pub layout: Arc<Layout>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Layout {
    pub size: Size,
    pub align: Align,
    pub stride: Size,
    pub abi: Abi,
    pub fields: FieldsShape,
    pub variants: Variants,
    pub largest_niche: Option<Niche>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Abi {
    Uninhabited,
    Scalar(Scalar),
    ScalarPair(Scalar, Scalar),
    Aggregate { sized: bool },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum FieldsShape {
    Primitive,
    Union(usize),
    Array { stride: Size, count: u64 },
    Arbitrary { offsets: Vec<Size> },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Variants {
    Single {
        index: usize,
    },
    Multiple {
        tag: Scalar,
        tag_encoding: TagEncoding,
        tag_field: usize,
        variants: Vec<Layout>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TagEncoding {
    Direct,
    Niche {
        dataful_variant: usize,
        niche_variants: RangeInclusive<usize>,
        niche_start: u128,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Niche {
    pub offset: Size,
    pub scalar: Scalar,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Scalar {
    pub value: Primitive,
    pub valid_range: RangeInclusive<u128>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Primitive {
    Int(Integer, bool),
    F32,
    F64,
    Pointer,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Integer {
    I8,
    I16,
    I32,
    I64,
    I128,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Size {
    raw: u64,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Align {
    pow2: u8,
}

impl<Ty> std::ops::Deref for TyLayout<Ty> {
    type Target = Layout;

    fn deref(&self) -> &Layout {
        &*self.layout
    }
}

impl TyLayout<Ty> {
    pub fn pointee(&self, lib: source::LibId, db: &dyn LayoutDatabase) -> Self {
        if let Type::App(_, args) = &*self.ty {
            db.layout_of(lib, args[0].clone())
        } else {
            unreachable!();
        }
    }
}

impl Layout {
    pub fn scalar(scalar: Scalar, triple: &target_lexicon::Triple) -> Self {
        let size = scalar.value.size(triple);
        let align = Align::from_bytes(size.bytes());
        let largest_niche = Niche::from_scalar(triple, Size::ZERO, scalar.clone());

        Layout {
            fields: FieldsShape::Primitive,
            variants: Variants::Single { index: 0 },
            largest_niche,
            abi: Abi::Scalar(scalar),
            size,
            align,
            stride: size,
        }
    }

    pub fn is_unsized(&self) -> bool {
        self.abi.is_unsized()
    }

    pub fn is_zst(&self) -> bool {
        match self.abi {
            Abi::Scalar(_) | Abi::ScalarPair(..) => false,
            Abi::Uninhabited => self.size.bytes() == 0,
            Abi::Aggregate { sized } => sized && self.size.bytes() == 0,
        }
    }
}

impl Abi {
    pub fn is_unsized(&self) -> bool {
        match self {
            Abi::Uninhabited | Abi::Scalar(_) | Abi::ScalarPair(..) => false,
            Abi::Aggregate { sized } => !sized,
        }
    }

    pub fn is_signed(&self) -> bool {
        match self {
            Abi::Scalar(scalar) => match scalar.value {
                Primitive::Int(_, signed) => signed,
                _ => false,
            },
            _ => false,
        }
    }

    pub fn is_float(&self) -> bool {
        match self {
            Abi::Scalar(scalar) => match scalar.value {
                Primitive::F32 => true,
                Primitive::F64 => true,
                _ => false,
            },
            _ => false,
        }
    }
}

impl FieldsShape {
    pub fn count(&self) -> usize {
        match self {
            FieldsShape::Primitive => 0,
            FieldsShape::Union(count) => *count,
            FieldsShape::Array { count, .. } => *count as usize,
            FieldsShape::Arbitrary { offsets } => offsets.len(),
        }
    }

    pub fn offset(&self, idx: usize) -> Size {
        match self {
            FieldsShape::Primitive => unreachable!(),
            FieldsShape::Union(_) => Size::ZERO,
            FieldsShape::Array { stride, count: _ } => {
                let i = u64::try_from(idx).unwrap();

                *stride * i
            }
            FieldsShape::Arbitrary { offsets } => offsets[idx],
        }
    }
}

impl Niche {
    pub fn from_scalar(
        triple: &target_lexicon::Triple,
        offset: Size,
        scalar: Scalar,
    ) -> Option<Self> {
        let niche = Niche { offset, scalar };

        if niche.available(triple) > 0 {
            Some(niche)
        } else {
            None
        }
    }

    pub fn available(&self, triple: &target_lexicon::Triple) -> u128 {
        let Scalar {
            value,
            valid_range: ref v,
        } = self.scalar;
        let bits = value.size(triple).bits();
        assert!(bits <= 128);
        let max_value = !0u128 >> (128 - bits);
        let niche = v.end().wrapping_add(1)..*v.start();

        niche.end.wrapping_sub(niche.start) & max_value
    }

    pub fn reserve(&self, triple: &target_lexicon::Triple, count: u128) -> Option<(u128, Scalar)> {
        assert!(count > 0);
        let Scalar {
            value,
            valid_range: ref v,
        } = self.scalar;
        let bits = value.size(triple).bits();
        assert!(bits <= 128);
        let max_value = !0u128 >> (128 - bits);

        if count > max_value {
            return None;
        }

        let start = v.end().wrapping_add(1) & max_value;
        let end = v.end().wrapping_add(count) & max_value;
        let valid_range_contains = |x| {
            if v.start() <= v.end() {
                *v.start() <= x && x <= *v.end()
            } else {
                *v.start() <= x || x <= *v.end()
            }
        };

        if valid_range_contains(end) {
            None
        } else {
            Some((
                start,
                Scalar {
                    value,
                    valid_range: *v.start()..=end,
                },
            ))
        }
    }
}

impl Size {
    pub const ZERO: Self = Size { raw: 0 };

    pub fn from_bits(bits: impl TryInto<u64>) -> Self {
        let bits = bits.try_into().ok().unwrap();

        Size::from_bytes(bits / 8 + ((bits % 8) + 7) / 8)
    }

    pub fn from_bytes(bytes: impl TryInto<u64>) -> Self {
        Size {
            raw: bytes.try_into().ok().unwrap(),
        }
    }

    pub fn bytes(self) -> u64 {
        self.raw
    }

    pub fn bits(self) -> u64 {
        self.bytes() * 8
    }

    pub fn align_to(self, align: Align) -> Self {
        let mask = align.bytes() - 1;

        Size::from_bytes((self.bytes() + mask) & !mask)
    }

    pub fn is_aligned(self, align: Align) -> bool {
        let mask = align.bytes() - 1;

        self.bytes() & mask == 0
    }
}

impl Align {
    pub fn from_bits(bits: u64) -> Self {
        Align::from_bytes(Size::from_bits(bits).bytes())
    }

    pub fn from_bytes(mut bytes: u64) -> Self {
        if bytes == 0 {
            return Align { pow2: 0 };
        }

        let mut pow2: u8 = 0;

        while (bytes & 1) == 0 {
            pow2 += 1;
            bytes >>= 1;
        }

        Align { pow2 }
    }

    pub fn bytes(self) -> u64 {
        1 << self.pow2
    }

    pub fn bits(self) -> u64 {
        self.bytes() * 8
    }

    pub fn max_for_offset(offset: Size) -> Self {
        Align {
            pow2: offset.bytes().trailing_zeros() as u8,
        }
    }

    pub fn restrict_for_offset(self, offset: Size) -> Self {
        self.min(Align::max_for_offset(offset))
    }
}

impl Add for Size {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        Size {
            raw: self.raw + other.raw,
        }
    }
}

impl Mul<u64> for Size {
    type Output = Self;

    fn mul(self, other: u64) -> Self {
        Size {
            raw: self.raw * other,
        }
    }
}

impl Scalar {
    pub fn is_bool(&self) -> bool {
        self.valid_range == (0..=1) && matches!(self.value, Primitive::Int(Integer::I8, _))
    }
}

impl Primitive {
    pub fn size(&self, triple: &target_lexicon::Triple) -> Size {
        match self {
            Primitive::Int(i, _) => i.size(),
            Primitive::F32 => Size::from_bits(32),
            Primitive::F64 => Size::from_bits(64),
            Primitive::Pointer => match triple.pointer_width() {
                Ok(pw) => Size::from_bytes(pw.bytes()),
                Err(_) => Size::from_bits(32),
            },
        }
    }

    pub fn align(&self, triple: &target_lexicon::Triple) -> Align {
        Align::from_bytes(self.size(triple).bytes())
    }
}

impl Integer {
    pub fn size(&self) -> Size {
        match self {
            Integer::I8 => Size::from_bytes(1),
            Integer::I16 => Size::from_bytes(2),
            Integer::I32 => Size::from_bytes(4),
            Integer::I64 => Size::from_bytes(8),
            Integer::I128 => Size::from_bytes(16),
        }
    }

    pub fn align(&self) -> Align {
        Align::from_bytes(self.size().bytes())
    }

    pub fn ptr_sized(triple: &target_lexicon::Triple) -> Self {
        match triple.pointer_width() {
            Ok(target_lexicon::PointerWidth::U16) => Integer::I16,
            Ok(target_lexicon::PointerWidth::U32) => Integer::I32,
            Ok(target_lexicon::PointerWidth::U64) => Integer::I64,
            Err(_) => Integer::I32,
        }
    }
}
