use std::convert::{TryFrom, TryInto};
use std::ops::{Add, Mul, RangeInclusive};

#[derive(Debug, Clone, Copy)]
pub struct TyLayout<'a, Ty> {
    pub ty: Ty,
    pub layout: &'a Layout,
}

#[derive(Debug, Clone)]
pub struct Layout {
    pub size: Size,
    pub align: Align,
    pub stride: Size,
    pub abi: Abi,
    pub fields: FieldsShape,
    pub variants: Variants,
    pub largest_niche: Option<Niche>,
}

#[derive(Debug, Clone)]
pub enum Abi {
    Uninhabited,
    Scalar(Scalar),
    ScalarPair(Scalar, Scalar),
    Aggregate { sized: bool },
}

#[derive(Debug, Clone, PartialEq)]
pub enum FieldsShape {
    Primitive,
    Union(usize),
    Array { stride: Size, count: u64 },
    Arbitrary { offsets: Vec<Size> },
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub enum TagEncoding {
    Direct,
    Niche {
        dataful_variant: usize,
        niche_variants: RangeInclusive<usize>,
        niche_start: u128,
    },
}

#[derive(Debug, Clone)]
pub struct Niche {
    pub offset: Size,
    pub scalar: Scalar,
}

#[derive(Debug, Clone)]
pub struct Scalar {
    pub value: Primitive,
    pub valid_range: RangeInclusive<u128>,
}

#[derive(Debug, Clone, Copy)]
pub enum Primitive {
    Int(Integer, bool),
    F32,
    F64,
    Pointer,
}

#[derive(Debug, Clone, Copy)]
pub enum Integer {
    I8,
    I16,
    I32,
    I64,
    I128,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Size {
    raw: u64,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Align {
    pow2: u8,
}

impl<'a, Ty> std::ops::Deref for TyLayout<'a, Ty> {
    type Target = &'a Layout;

    fn deref(&self) -> &&'a Layout {
        &self.layout
    }
}

impl<'tcx> TyLayout<'tcx, crate::ty::Ty<'tcx>> {
    pub fn field(&self, tcx: &crate::tcx::Tcx<'tcx>, idx: usize) -> Self {
        use super::ty::Type;

        assert!(idx < self.fields.count());

        tcx.layout(match self.ty {
            Type::Error | Type::Var(_) => unreachable!(),
            Type::TypeOf(id) => return tcx.layout_of(id).field(tcx, idx),
            Type::Never
            | Type::Param(_)
            | Type::Forall(_, _)
            | Type::Bool
            | Type::TypeId
            | Type::VInt(_)
            | Type::VUInt(_)
            | Type::VFloat(_)
            | Type::Int(_)
            | Type::UInt(_)
            | Type::Float(_)
            | Type::Ref(_, _)
            | Type::Array(_, _)
            | Type::Func(..) => unreachable!("{}.{}", self.ty, idx),
            Type::Str => {
                if idx == 0 {
                    tcx.builtin.ref_u8
                } else {
                    tcx.builtin.usize
                }
            }
            Type::Slice(of) => {
                if idx == 0 {
                    tcx.intern_ty(Type::Ref(false, of))
                } else {
                    tcx.builtin.usize
                }
            }
            Type::Tuple(tys) => tys[idx],
            Type::Struct(_, fields) => fields[idx].ty,
            Type::Enum(_, variants) => match self.variants {
                Variants::Single { index } => variants[index].fields[idx].ty,
                Variants::Multiple { ref tag, .. } => {
                    assert_eq!(idx, 0);

                    return TyLayout {
                        layout: tcx.intern_layout(Layout::scalar(tag.clone(), tcx.target)),
                        ty: tag.value.ty(tcx),
                    };
                }
            },
        })
    }

    pub fn for_variant(self, tcx: &crate::tcx::Tcx<'tcx>, variant: usize) -> Self {
        let layout = match self.variants {
            Variants::Single { index }
                if variant == index && self.fields != FieldsShape::Primitive =>
            {
                self.layout
            }
            Variants::Single { index } => {
                let fields = match self.ty {
                    crate::ty::Type::Enum(_, variants) => variants.len(),
                    _ => unreachable!(),
                };

                tcx.intern_layout(Layout {
                    variants: Variants::Single { index },
                    fields: if fields == 0 {
                        FieldsShape::Arbitrary {
                            offsets: Vec::new(),
                        }
                    } else {
                        FieldsShape::Union(fields)
                    },
                    abi: Abi::Uninhabited,
                    largest_niche: None,
                    align: Align::from_bytes(1),
                    size: Size::ZERO,
                    stride: Size::ZERO,
                })
            }
            Variants::Multiple { ref variants, .. } => &variants[variant],
        };

        TyLayout {
            layout,
            ty: self.ty,
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

    pub fn ty<'tcx>(&self, tcx: &crate::tcx::Tcx<'tcx>) -> crate::ty::Ty<'tcx> {
        match self {
            Primitive::Int(Integer::I8, false) => tcx.builtin.u8,
            Primitive::Int(Integer::I16, false) => tcx.builtin.u16,
            Primitive::Int(Integer::I32, false) => tcx.builtin.u32,
            Primitive::Int(Integer::I64, false) => tcx.builtin.u64,
            Primitive::Int(Integer::I128, false) => tcx.builtin.u128,
            Primitive::Int(Integer::I8, true) => tcx.builtin.i8,
            Primitive::Int(Integer::I16, true) => tcx.builtin.i16,
            Primitive::Int(Integer::I32, true) => tcx.builtin.i32,
            Primitive::Int(Integer::I64, true) => tcx.builtin.i64,
            Primitive::Int(Integer::I128, true) => tcx.builtin.i128,
            Primitive::F32 => tcx.builtin.f32,
            Primitive::F64 => tcx.builtin.f64,
            _ => unreachable!(),
        }
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
