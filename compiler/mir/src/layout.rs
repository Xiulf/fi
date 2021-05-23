use crate::db::MirDatabase;
use hir::ty::{Ty, TyKind};
use std::convert::TryInto;
use std::ops::RangeInclusive;
use std::sync::{Arc, Weak};
use target_lexicon::{PointerWidth, Triple};

pub fn layout_of_query(db: &dyn MirDatabase, mut ty: Ty) -> Arc<Layout> {
    let triple = db.target_triple();
    let scalar_unit = |value: Primitive| {
        let bits = value.size(&triple).bits();

        Scalar {
            value,
            valid_range: 0..=(!0 >> (128 - bits)),
        }
    };

    let scalar = |value: Primitive| {
        let scalar = scalar_unit(value);

        Layout::scalar(scalar, &triple)
    };

    let mut args = Vec::new();

    while let TyKind::App(a, b) = ty.lookup(db.upcast()) {
        args.push(b);
        ty = a;
    }

    args.reverse();

    let layout = match ty.lookup(db.upcast()) {
        | TyKind::Error
        | TyKind::Unknown(_)
        | TyKind::Skolem(_, _)
        | TyKind::Row(_, _)
        | TyKind::Figure(_)
        | TyKind::Symbol(_)
        | TyKind::App(_, _) => unreachable!(),
        | TyKind::ForAll(_, ty) => return db.layout_of(ty),
        | TyKind::Ctnt(_, ty) => return db.layout_of(ty),
        | TyKind::TypeVar(_) => unimplemented!(),
        | TyKind::Tuple(tys) => unimplemented!(),
        | TyKind::Ctor(id) => unimplemented!(),
    };

    Arc::new(layout)
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Layout {
    pub size: Size,
    pub align: Align,
    pub stride: Size,
    pub elem: Option<Ty>,
    pub abi: Abi,
    pub fields: Fields,
    pub variants: Variants,
    pub largest_niche: Option<Niche>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Size {
    raw: u64,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Align {
    pow2: u8,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Abi {
    Uninhabited,
    Scalar(Scalar),
    ScalarPair(Scalar, Scalar),
    Aggregate { sized: bool },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Fields {
    Primitive,
    Array { stride: Size, count: usize },
    Union { types: Vec<Ty> },
    Arbitrary { fields: Vec<(Size, Ty)> },
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

impl Layout {
    pub fn scalar(scalar: Scalar, triple: &Triple) -> Self {
        let size = scalar.value.size(triple);
        let align = Align::from_bytes(size.bytes());
        let largest_niche = Niche::from_scalar(triple, Size::ZERO, scalar.clone());

        Self {
            size,
            align,
            stride: size.align_to(align),
            elem: None,
            abi: Abi::Scalar(scalar),
            fields: Fields::Primitive,
            variants: Variants::Single { index: 0 },
            largest_niche,
        }
    }

    pub fn unit() -> Self {
        Self {
            size: Size::ZERO,
            align: Align::from_bytes(0),
            stride: Size::ZERO,
            elem: None,
            abi: Abi::Aggregate { sized: true },
            fields: Fields::Arbitrary { fields: Vec::new() },
            variants: Variants::Single { index: 0 },
            largest_niche: None,
        }
    }

    pub fn is_zst(&self) -> bool {
        match self.abi {
            | Abi::Scalar(_) | Abi::ScalarPair(_, _) => false,
            | Abi::Uninhabited => self.size.bytes() == 0,
            | Abi::Aggregate { sized } => sized && self.size.bytes() == 0,
        }
    }

    pub fn elem(&self, db: &dyn MirDatabase) -> Option<Arc<Self>> {
        self.elem.map(|e| db.layout_of(e))
    }

    pub fn field(&self, db: &dyn MirDatabase, field: usize) -> Option<Arc<Self>> {
        assert!(field < self.fields.count());

        match &self.fields {
            | Fields::Primitive => None,
            | Fields::Array { .. } => self.elem(db),
            | Fields::Union { types } => Some(db.layout_of(types[field])),
            | Fields::Arbitrary { fields } => Some(db.layout_of(fields[field].1)),
        }
    }

    pub fn variant(&self, variant: usize) -> Self {
        match self.variants {
            | Variants::Single { index } if variant == index && self.fields != Fields::Primitive => self.clone(),
            | Variants::Single { index } => Layout {
                size: Size::ZERO,
                stride: Size::ZERO,
                align: Align::from_bytes(0),
                elem: None,
                abi: Abi::Uninhabited,
                fields: Fields::Arbitrary { fields: Vec::new() },
                variants: Variants::Single { index },
                largest_niche: None,
            },
            | Variants::Multiple { ref variants, .. } => variants[variant].clone(),
        }
    }
}

impl Size {
    pub const ZERO: Self = Self { raw: 0 };

    pub fn from_bits(bits: impl TryInto<u64>) -> Self {
        let bits = bits.try_into().ok().unwrap();

        Self::from_bytes(bits / 8 + ((bits % 8) + 7) / 8)
    }

    pub fn from_bytes(bytes: impl TryInto<u64>) -> Self {
        Self {
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

        Self::from_bytes((self.bytes() + mask) & !mask)
    }

    pub fn is_aligned(self, align: Align) -> bool {
        let mask = align.bytes() - 1;

        self.bytes() & mask == 0
    }
}

impl std::ops::Add for Size {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self {
            raw: self.raw + rhs.raw,
        }
    }
}

impl std::ops::Mul<u64> for Size {
    type Output = Self;

    fn mul(self, rhs: u64) -> Self::Output {
        Self { raw: self.raw * rhs }
    }
}

impl Align {
    pub fn from_bits(bits: u64) -> Self {
        Self::from_bytes(Self::from_bits(bits).bytes())
    }

    pub fn from_bytes(mut bytes: u64) -> Self {
        if bytes == 0 {
            return Self { pow2: 2 };
        }

        let mut pow2 = 0u8;

        while (bytes & 1) == 0 {
            pow2 += 1;
            bytes >>= 1;
        }

        Self { pow2 }
    }

    pub fn bytes(self) -> u64 {
        1 << self.pow2
    }

    pub fn bits(self) -> u64 {
        self.bytes() * 8
    }

    pub fn max_for_offset(offset: Size) -> Self {
        Self {
            pow2: offset.bytes().trailing_zeros() as u8,
        }
    }

    pub fn restrict_for_offset(self, offset: Size) -> Self {
        self.min(Self::max_for_offset(offset))
    }
}

impl Abi {
    pub fn is_unsized(&self) -> bool {
        match self {
            | Abi::Uninhabited | Abi::Scalar(_) | Abi::ScalarPair(_, _) => false,
            | Abi::Aggregate { sized } => !sized,
        }
    }
}

impl Fields {
    pub fn count(&self) -> usize {
        match self {
            | Fields::Primitive => 0,
            | Fields::Array { count, .. } => *count,
            | Fields::Union { types } => types.len(),
            | Fields::Arbitrary { fields } => fields.len(),
        }
    }

    pub fn offset(&self, idx: usize) -> Size {
        match self {
            | Fields::Primitive => unreachable!(),
            | Fields::Array { stride, .. } => {
                let i = idx as u64;

                *stride * i
            },
            | Fields::Union { .. } => Size::ZERO,
            | Fields::Arbitrary { fields } => fields[idx].0,
        }
    }
}

impl Niche {
    pub fn from_scalar(triple: &Triple, offset: Size, scalar: Scalar) -> Option<Self> {
        let niche = Self { offset, scalar };

        if niche.available(triple) > 0 {
            Some(niche)
        } else {
            None
        }
    }

    pub fn available(&self, triple: &Triple) -> u128 {
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

    pub fn reserve(&self, triple: &Triple, count: u128) -> Option<(u128, Scalar)> {
        assert!(count > 0);
        let Scalar {
            value,
            valid_range: ref v,
        } = self.scalar;

        let bits = value.size(triple).bits();
        assert!(bits <= 128);
        let max_value = !0128 >> (128 - bits);

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
            Some((start, Scalar {
                value,
                valid_range: *v.start()..=end,
            }))
        }
    }
}

impl Primitive {
    pub fn size(self, triple: &Triple) -> Size {
        match self {
            | Primitive::Int(int, _) => int.size(),
            | Primitive::F32 => Size::from_bits(32),
            | Primitive::F64 => Size::from_bits(64),
            | Primitive::Pointer => match triple.pointer_width() {
                | Ok(PointerWidth::U16) => Size::from_bits(16),
                | Ok(PointerWidth::U32) => Size::from_bits(32),
                | Ok(PointerWidth::U64) => Size::from_bits(64),
                | Err(_) => Size::from_bits(32),
            },
        }
    }

    pub fn align(self, triple: &Triple) -> Align {
        Align::from_bytes(self.size(triple).bytes())
    }
}

impl Integer {
    pub fn size(self) -> Size {
        match self {
            | Integer::I8 => Size::from_bits(8),
            | Integer::I16 => Size::from_bits(16),
            | Integer::I32 => Size::from_bits(32),
            | Integer::I64 => Size::from_bits(64),
            | Integer::I128 => Size::from_bits(128),
        }
    }

    pub fn align(self) -> Align {
        Align::from_bytes(self.size().bytes())
    }
}
