use crate::db::MirDatabase;
use hir::attrs::AttrInput;
use hir::display::HirDisplay;
use hir::ty::{Ty, TyKind};
use std::convert::TryInto;
use std::fmt;
use std::ops::RangeInclusive;
use std::sync::{Arc, Weak};
use target_lexicon::{PointerWidth, Triple};

pub fn layout_of_query(db: &dyn MirDatabase, mut ty: Ty) -> Arc<Layout> {
    let triple = db.target_triple();
    let scalar = |value: Primitive| {
        let scalar = Scalar::new(value, &triple);

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
        | TyKind::TypeVar(_) => {
            let elem = Arc::new(Layout {
                abi: Abi::Aggregate { sized: false },
                ..Layout::default()
            });

            let mut scalar = Scalar::new(Primitive::Pointer, &triple);

            scalar.valid_range = 1..=*scalar.valid_range.end();

            let mut lyt = Layout::scalar(scalar, &triple);

            lyt.elem = Some(Err(elem));
            lyt
        },
        | TyKind::Tuple(tys) => {
            let lyts = tys.iter().map(|&t| db.layout_of(t)).collect();

            struct_layout(lyts, &triple)
        },
        | TyKind::Ctor(id) => {
            let attrs = db.attrs(id.into());
            let mut attrs = attrs.by_key("repr").attrs();

            if let Some(attr) = attrs.next().and_then(|a| a.group()) {
                layout_from_repr(db, attr, &args)
            } else {
                let data = db.type_ctor_data(id);
                let lyts = data
                    .ctors
                    .iter()
                    .map(|(local_id, ctor)| {
                        let lower = db.ctor_ty(hir::id::CtorId { local_id, parent: id });
                        let lyts = ctor
                            .types
                            .iter()
                            .map(|&t| {
                                let ty = lower.types[t];
                                let ty = args.iter().fold(ty, |r, t| r.replace_var(db.upcast(), *t));

                                db.layout_of(ty)
                            })
                            .collect();

                        struct_layout(lyts, &triple)
                    })
                    .collect();

                enum_layout(lyts, &triple)
            }
        },
    };

    Arc::new(layout)
}

fn scalar_pair(a: Scalar, b: Scalar, triple: &Triple) -> Layout {
    let b_align = b.value.align(triple);
    let align = a.value.align(triple).max(b_align);
    let b_offset = a.value.size(triple).align_to(b_align);
    let size = b_offset + b.value.size(triple);
    let largest_niche = Niche::from_scalar(triple, b_offset, b.clone())
        .into_iter()
        .chain(Niche::from_scalar(triple, Size::ZERO, a.clone()))
        .max_by_key(|n| n.available(triple));

    let a_lyt = Arc::new(Layout::scalar(a.clone(), triple));
    let b_lyt = Arc::new(Layout::scalar(b.clone(), triple));

    Layout {
        size,
        align,
        stride: size.align_to(align),
        elem: None,
        abi: Abi::ScalarPair(a, b),
        fields: Fields::Arbitrary {
            fields: vec![(Size::ZERO, a_lyt), (b_offset, b_lyt)],
        },
        variants: Variants::Single { index: 0 },
        largest_niche,
    }
}

fn ptr_pair(a_lyt: Arc<Layout>, b_lyt: Arc<Layout>, triple: &Triple) -> Layout {
    let mut scalar = Scalar::new(Primitive::Pointer, triple);
    scalar.valid_range = 1..=*scalar.valid_range.end();
    let align = scalar.value.align(triple);
    let offset = scalar.value.size(triple);
    let size = offset * 2;
    let largest_niche = Niche::from_scalar(triple, Size::ZERO, scalar.clone());
    let mut a_ptr = Layout::scalar(scalar.clone(), &triple);
    let mut b_ptr = Layout::scalar(scalar.clone(), &triple);

    a_ptr.elem = Some(Err(a_lyt));
    b_ptr.elem = Some(Err(b_lyt));

    Layout {
        size,
        align,
        stride: size.align_to(align),
        elem: None,
        abi: Abi::ScalarPair(scalar.clone(), scalar),
        fields: Fields::Arbitrary {
            fields: vec![(Size::ZERO, Arc::new(a_ptr)), (offset, Arc::new(b_ptr))],
        },
        variants: Variants::Single { index: 0 },
        largest_niche,
    }
}

fn slice_layout(elem_lyt: Arc<Layout>, triple: &Triple) -> Layout {
    let len = match triple.pointer_width() {
        | Ok(PointerWidth::U16) => Scalar::new(Primitive::Int(Integer::I16, false), &triple),
        | Ok(PointerWidth::U32) => Scalar::new(Primitive::Int(Integer::I32, false), &triple),
        | Ok(PointerWidth::U64) => Scalar::new(Primitive::Int(Integer::I64, false), &triple),
        | Err(_) => Scalar::new(Primitive::Int(Integer::I32, false), &triple),
    };

    let uint = Arc::new(Layout::scalar(len.clone(), triple));
    let mut ptr = Scalar::new(Primitive::Pointer, triple);

    ptr.valid_range = 1..=*ptr.valid_range.end();

    let align = ptr.value.align(triple);
    let offset = ptr.value.size(triple);
    let size = offset * 2;
    let largest_niche = Niche::from_scalar(triple, offset, ptr.clone());

    Layout {
        size,
        align,
        stride: size.align_to(align),
        elem: None,
        abi: Abi::ScalarPair(ptr, len),
        fields: Fields::Arbitrary {
            fields: vec![(Size::ZERO, elem_lyt), (offset, uint)],
        },
        variants: Variants::Single { index: 0 },
        largest_niche,
    }
}

fn struct_layout(lyts: Vec<Arc<Layout>>, triple: &Triple) -> Layout {
    match (lyts.get(0), lyts.get(1)) {
        | (Some(a), Some(b)) => match (&a.abi, &b.abi) {
            | (Abi::Scalar(a), Abi::Scalar(b)) => return scalar_pair(a.clone(), b.clone(), triple),
            | (_, _) => {},
        },
        | (Some(s), None) | (None, Some(s)) => match &s.abi {
            | Abi::Scalar(s) => return Layout::scalar(s.clone(), triple),
            | _ => {},
        },
        | (None, None) => {},
    }

    let mut align = Align::ONE;
    let mut fields = lyts.iter().map(|lyt| (Size::ZERO, lyt.clone())).collect::<Vec<_>>();
    let mut offset = Size::ZERO;
    let mut niches = Vec::new();

    for (i, lyt) in lyts.into_iter().enumerate() {
        if let Some(niche) = &lyt.largest_niche {
            niches.push(niche.clone());
        }

        offset = offset.align_to(lyt.align);
        align = align.max(lyt.align);
        fields[i].0 = offset;
        offset = offset + lyt.size;
    }

    let size = offset;
    let stride = offset.align_to(align);
    let largest_niche = niches.into_iter().max_by_key(|n| n.available(triple));

    Layout {
        size,
        align,
        stride,
        elem: None,
        abi: Abi::Aggregate { sized: true },
        fields: Fields::Arbitrary { fields },
        variants: Variants::Single { index: 0 },
        largest_niche,
    }
}

fn enum_layout(mut lyts: Vec<Layout>, triple: &Triple) -> Layout {
    if lyts.is_empty() {
        Layout::default()
    } else if lyts.len() == 1 {
        lyts.pop().unwrap()
    } else {
        let largest_niche = lyts
            .iter()
            .filter_map(|v| v.largest_niche.clone())
            .max_by_key(|n| n.available(triple));

        for (i, lyt) in lyts.iter_mut().enumerate() {
            lyt.variants = Variants::Single { index: i };
        }

        let largest = lyts.iter().max_by_key(|l| l.size).unwrap();
        let align = largest.align;
        let mut size = largest.size;
        let mut no_niche = |mut variants: Vec<Layout>| {
            let tag_size = Size::from_bits(variants.len()).align_to(align);
            let tag = Scalar {
                value: Primitive::Int(
                    match tag_size.bytes() {
                        | 1 => Integer::I8,
                        | 2 => Integer::I16,
                        | 3 | 4 => Integer::I32,
                        | 5 | 6 | 7 | 8 => Integer::I64,
                        | _ => Integer::I128,
                    },
                    false,
                ),
                valid_range: 0..=variants.len() as u128 - 1,
            };

            for variant in &mut variants {
                if let Fields::Arbitrary { fields } = &mut variant.fields {
                    for (offset, _) in fields {
                        *offset = *offset + tag_size;
                    }
                }
            }

            let variants = variants.into_iter().map(Arc::new).collect::<Vec<_>>();
            let tag_encoding = TagEncoding::Direct;
            let union_ = Layout {
                size,
                align,
                stride: size.align_to(align),
                elem: None,
                abi: Abi::Aggregate { sized: true },
                fields: Fields::Union {
                    fields: variants.clone(),
                },
                variants: Variants::Single { index: 0 },
                largest_niche: None,
            };

            let fields = vec![
                (Size::ZERO, Arc::new(Layout::scalar(tag.clone(), triple))),
                (tag_size, Arc::new(union_)),
            ];

            size = size + tag_size;

            if size == tag_size {
                (
                    tag.clone(),
                    Fields::Arbitrary {
                        fields: vec![(Size::ZERO, Arc::new(Layout::scalar(tag.clone(), triple)))],
                    },
                    Variants::Multiple {
                        tag,
                        tag_encoding,
                        variants,
                        tag_field: 0,
                    },
                )
            } else {
                (tag.clone(), Fields::Arbitrary { fields }, Variants::Multiple {
                    tag,
                    tag_encoding,
                    variants,
                    tag_field: 0,
                })
            }
        };

        let (tag, fields, variants) = if let Some(niche) = largest_niche {
            if niche.available(triple) >= lyts.len() as u128 {
                // @TODO: implement niches
                no_niche(lyts)
            } else {
                no_niche(lyts)
            }
        } else {
            no_niche(lyts)
        };

        let stride = size.align_to(align);

        if tag.value.size(triple) == size {
            Layout {
                size,
                align,
                stride,
                elem: None,
                abi: Abi::Scalar(tag),
                fields,
                variants,
                largest_niche: None,
            }
        } else {
            Layout {
                size,
                align,
                stride,
                elem: None,
                abi: Abi::Aggregate { sized: true },
                fields,
                variants,
                largest_niche: None,
            }
        }
    }
}

fn layout_from_repr(db: &dyn MirDatabase, group: &[AttrInput], args: &[Ty]) -> Layout {
    let triple = db.target_triple();
    let mut layout = Layout::default();

    if let Some(_) = group.iter().find(|i| i.ident("uninhabited")) {
        layout.abi = Abi::Uninhabited;
    }

    if let Some(fst) = group.iter().find_map(|i| i.field("scalar_first")) {
        if let Some(snd) = group.iter().find_map(|i| i.field("scalar_second")) {
            let first = if let Some(val) = fst.string() {
                scalar_from_repr(val, &triple)
            } else {
                let group = fst.group().unwrap();
                let mut s = scalar_from_repr(group.iter().find_map(|i| i.string()).unwrap(), &triple);

                if let Some(val) = group
                    .iter()
                    .find_map(|i| i.field("valid_range_start"))
                    .and_then(|i| i.int())
                {
                    s.valid_range = (val as u128)..=*s.valid_range.end();
                }

                if let Some(val) = group
                    .iter()
                    .find_map(|i| i.field("valid_range_end"))
                    .and_then(|i| i.int())
                {
                    s.valid_range = *s.valid_range.start()..=(val as u128);
                }

                s
            };

            let second = if let Some(val) = snd.string() {
                scalar_from_repr(val, &triple)
            } else {
                let group = snd.group().unwrap();
                let mut s = scalar_from_repr(group.iter().find_map(|i| i.string()).unwrap(), &triple);

                if let Some(val) = group
                    .iter()
                    .find_map(|i| i.field("valid_range_start"))
                    .and_then(|i| i.int())
                {
                    s.valid_range = (val as u128)..=*s.valid_range.end();
                }

                if let Some(val) = group
                    .iter()
                    .find_map(|i| i.field("valid_range_end"))
                    .and_then(|i| i.int())
                {
                    s.valid_range = *s.valid_range.start()..=(val as u128);
                }

                s
            };

            layout = scalar_pair(first, second, &triple);
        }
    }

    if let Some(val) = group.iter().find_map(|i| i.field("scalar")).and_then(|i| i.string()) {
        layout = Layout::scalar(scalar_from_repr(val, &triple), &triple);
    }

    if let Some(val) = group
        .iter()
        .find_map(|i| i.field("valid_range_start"))
        .and_then(|i| i.int())
    {
        if let Abi::Scalar(s) = &mut layout.abi {
            s.valid_range = (val as u128)..=*s.valid_range.end();
            layout.largest_niche = Niche::from_scalar(&triple, Size::ZERO, s.clone());
        }
    }

    if let Some(val) = group
        .iter()
        .find_map(|i| i.field("valid_range_end"))
        .and_then(|i| i.int())
    {
        if let Abi::Scalar(s) = &mut layout.abi {
            s.valid_range = *s.valid_range.start()..=(val as u128);
            layout.largest_niche = Niche::from_scalar(&triple, Size::ZERO, s.clone());
        }
    }

    if let Some(elem) = group.iter().find_map(|i| i.field("elem")) {
        if let Some(idx) = elem.int() {
            layout.elem = Some(Ok(args[idx as usize]));
        } else if let Some(group) = elem.group() {
            layout.elem = Some(Err(Arc::new(layout_from_repr(db, group, args))));
        }
    }

    if let Some(arr) = group.iter().find_map(|i| i.field("array")).and_then(|i| i.group()) {
        if let Some(len) = arr.iter().find_map(|i| i.field("len")).and_then(|i| i.int()) {
            if let Some(elem) = &layout.elem {
                let stride = match elem {
                    | Ok(ty) => db.layout_of(*ty).stride,
                    | Err(lyt) => lyt.stride,
                };

                layout.size = stride * (len as u64);
                layout.align = Align::from_bytes(stride.bytes());
                layout.stride = layout.size;
                layout.abi = Abi::Aggregate { sized: true };
                layout.fields = Fields::Array {
                    stride,
                    count: len as usize,
                };

                layout.largest_niche = None;
            }
        }
    }

    if let Some(rec) = group.iter().find_map(|i| i.field("record")).and_then(|i| i.group()) {
        if let Some(fields) = rec.iter().find_map(|i| i.field("fields")).and_then(|i| i.int()) {
            if let TyKind::Row(fields, tail) = args[fields as usize].lookup(db.upcast()) {
                if let Some(_) = tail {
                    let data_lyt = Arc::new(Layout {
                        size: Size::ZERO,
                        align: Align::ONE,
                        stride: Size::ZERO,
                        elem: None,
                        abi: Abi::Aggregate { sized: false },
                        fields: Fields::Arbitrary { fields: Vec::new() },
                        variants: Variants::Single { index: 0 },
                        largest_niche: None,
                    });

                    let uint = ptr_sized_int(db, false);
                    let idxs_layout = Arc::new(Layout {
                        size: uint.stride * fields.len() as u64,
                        align: uint.align,
                        stride: uint.stride * fields.len() as u64,
                        elem: Some(Err(uint.clone())),
                        abi: Abi::Aggregate { sized: true },
                        fields: Fields::Array {
                            stride: uint.stride,
                            count: fields.len(),
                        },
                        variants: Variants::Single { index: 0 },
                        largest_niche: None,
                    });

                    layout = ptr_pair(data_lyt, idxs_layout, &triple);
                } else {
                    let lyts = fields.iter().map(|f| db.layout_of(f.ty)).collect();

                    layout = struct_layout(lyts, &triple);
                }
            }
        }
    }

    layout
}

fn scalar_from_repr(repr: &str, triple: &Triple) -> Scalar {
    match repr {
        | "u8" => Scalar::new(Primitive::Int(Integer::I8, false), triple),
        | "u16" => Scalar::new(Primitive::Int(Integer::I16, false), triple),
        | "u32" => Scalar::new(Primitive::Int(Integer::I32, false), triple),
        | "u64" => Scalar::new(Primitive::Int(Integer::I64, false), triple),
        | "u128" => Scalar::new(Primitive::Int(Integer::I128, false), triple),
        | "i8" => Scalar::new(Primitive::Int(Integer::I8, true), triple),
        | "i16" => Scalar::new(Primitive::Int(Integer::I16, true), triple),
        | "i32" => Scalar::new(Primitive::Int(Integer::I32, true), triple),
        | "i64" => Scalar::new(Primitive::Int(Integer::I64, true), triple),
        | "i128" => Scalar::new(Primitive::Int(Integer::I128, true), triple),
        | "f32" => Scalar::new(Primitive::F32, triple),
        | "f64" => Scalar::new(Primitive::F64, triple),
        | "ptr" => Scalar::new(Primitive::Pointer, triple),
        | "ptr_sized_int" => match triple.pointer_width() {
            | Ok(PointerWidth::U16) => Scalar::new(Primitive::Int(Integer::I16, true), triple),
            | Ok(PointerWidth::U32) => Scalar::new(Primitive::Int(Integer::I32, true), triple),
            | Ok(PointerWidth::U64) => Scalar::new(Primitive::Int(Integer::I64, true), triple),
            | Err(_) => Scalar::new(Primitive::Int(Integer::I32, true), triple),
        },
        | "ptr_sized_uint" => match triple.pointer_width() {
            | Ok(PointerWidth::U16) => Scalar::new(Primitive::Int(Integer::I16, false), triple),
            | Ok(PointerWidth::U32) => Scalar::new(Primitive::Int(Integer::I32, false), triple),
            | Ok(PointerWidth::U64) => Scalar::new(Primitive::Int(Integer::I64, false), triple),
            | Err(_) => Scalar::new(Primitive::Int(Integer::I32, false), triple),
        },
        | _ => panic!("invalid scalar '{}'", repr),
    }
}

pub fn ptr_sized_int(db: &dyn MirDatabase, sign: bool) -> Arc<Layout> {
    let triple = db.target_triple();
    let scalar = match triple.pointer_width() {
        | Ok(PointerWidth::U16) => Scalar::new(Primitive::Int(Integer::I16, sign), &triple),
        | Ok(PointerWidth::U32) => Scalar::new(Primitive::Int(Integer::I32, sign), &triple),
        | Ok(PointerWidth::U64) => Scalar::new(Primitive::Int(Integer::I64, sign), &triple),
        | Err(_) => Scalar::new(Primitive::Int(Integer::I32, sign), &triple),
    };

    Arc::new(Layout::scalar(scalar, &triple))
}

pub fn str_slice(db: &dyn MirDatabase) -> Arc<Layout> {
    let triple = db.target_triple();
    let uint8 = Scalar::new(Primitive::Int(Integer::I8, false), &triple);
    let uint8 = Arc::new(Layout::scalar(uint8, &triple));
    let ptr = reference(db, uint8);

    Arc::new(slice_layout(ptr, &triple))
}

pub fn type_var(db: &dyn MirDatabase, lib: base_db::libs::LibId, kind: Ty) -> Option<Arc<Layout>> {
    let type_id = db.lang_item(lib, "type-kind".into()).unwrap();
    let type_id = type_id.as_type_ctor().unwrap();
    let figure_id = db.lang_item(lib, "figure-kind".into()).unwrap();
    let figure_id = figure_id.as_type_ctor().unwrap();
    let symbol_id = db.lang_item(lib, "symbol-kind".into()).unwrap();
    let symbol_id = symbol_id.as_type_ctor().unwrap();
    let kind = kind.lookup(db.upcast());

    if kind == TyKind::Ctor(type_id) {
        Some(reference(db, type_info(db)))
    } else if kind == TyKind::Ctor(figure_id) {
        Some(ptr_sized_int(db, true))
    } else if kind == TyKind::Ctor(symbol_id) {
        Some(str_slice(db))
    } else {
        None
    }
}

pub fn type_info(db: &dyn MirDatabase) -> Arc<Layout> {
    let triple = db.target_triple();
    let uint = ptr_sized_int(db, false);

    Arc::new(Layout {
        size: uint.size,
        align: uint.align,
        stride: uint.stride,
        elem: None,
        abi: Abi::Aggregate { sized: true },
        fields: Fields::Arbitrary {
            fields: vec![(Size::ZERO, uint)],
        },
        variants: Variants::Single { index: 0 },
        largest_niche: None,
    })
}

pub fn reference(db: &dyn MirDatabase, to: Arc<Layout>) -> Arc<Layout> {
    let triple = db.target_triple();
    let mut scalar = Scalar::new(Primitive::Pointer, &triple);

    scalar.valid_range = 1..=*scalar.valid_range.end();

    let mut lyt = Layout::scalar(scalar, &triple);

    lyt.elem = Some(Err(to));

    Arc::new(lyt)
}

pub fn array(of: Arc<Layout>, count: usize) -> Arc<Layout> {
    Arc::new(Layout {
        size: of.stride * count as u64,
        align: of.align,
        stride: of.stride * count as u64,
        abi: Abi::Aggregate { sized: true },
        fields: Fields::Array {
            stride: of.stride,
            count,
        },
        variants: Variants::Single { index: 0 },
        elem: Some(Err(of)),
        largest_niche: None,
    })
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Layout {
    pub size: Size,
    pub align: Align,
    pub stride: Size,
    pub elem: Option<Result<Ty, Arc<Layout>>>,
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
    Union { fields: Vec<Arc<Layout>> },
    Arbitrary { fields: Vec<(Size, Arc<Layout>)> },
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
        variants: Vec<Arc<Layout>>,
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

impl Default for Layout {
    fn default() -> Self {
        Self::UNIT
    }
}

impl Layout {
    pub const UNIT: Self = Self {
        size: Size::ZERO,
        align: Align::ONE,
        stride: Size::ZERO,
        elem: None,
        abi: Abi::Aggregate { sized: true },
        fields: Fields::Arbitrary { fields: Vec::new() },
        variants: Variants::Single { index: 0 },
        largest_niche: None,
    };

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

    pub fn is_zst(&self) -> bool {
        match self.abi {
            | Abi::Scalar(_) | Abi::ScalarPair(_, _) => false,
            | Abi::Uninhabited => self.size.bytes() == 0,
            | Abi::Aggregate { sized } => sized && self.size.bytes() == 0,
        }
    }

    pub fn elem(&self, db: &dyn MirDatabase) -> Option<Arc<Self>> {
        match self.elem.as_ref()? {
            | Ok(ty) => Some(db.layout_of(*ty)),
            | Err(lyt) => Some(lyt.clone()),
        }
    }

    pub fn field(&self, db: &dyn MirDatabase, field: usize) -> Option<Arc<Self>> {
        assert!(field < self.fields.count());

        match &self.fields {
            | Fields::Primitive => None,
            | Fields::Array { .. } => self.elem(db),
            | Fields::Union { fields: types } => Some(types[field].clone()),
            | Fields::Arbitrary { fields } => Some(fields[field].1.clone()),
        }
    }

    pub fn variant(self: &Arc<Layout>, variant: usize) -> Arc<Layout> {
        match self.variants {
            | Variants::Single { index } if variant == index && self.fields != Fields::Primitive => self.clone(),
            | Variants::Single { index } => Arc::new(Layout {
                size: Size::ZERO,
                stride: Size::ZERO,
                align: Align::ONE,
                elem: None,
                abi: Abi::Uninhabited,
                fields: Fields::Arbitrary { fields: Vec::new() },
                variants: Variants::Single { index },
                largest_niche: None,
            }),
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
    pub const ONE: Self = Self { pow2: 0 };

    pub fn from_bits(bits: u64) -> Self {
        Self::from_bytes(Self::from_bits(bits).bytes())
    }

    pub fn from_bytes(mut bytes: u64) -> Self {
        if bytes == 0 {
            return Self::ONE;
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
            | Fields::Union { fields: types } => types.len(),
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

impl Scalar {
    pub fn new(value: Primitive, triple: &Triple) -> Self {
        let bits = value.size(&triple).bits();

        Scalar {
            value,
            valid_range: 0..=(!0 >> (128 - bits)),
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

impl fmt::Display for Layout {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.abi {
            | Abi::Uninhabited => write!(f, "uninhabited"),
            | Abi::Scalar(s) => {
                match s.value {
                    | Primitive::Int(Integer::I8, false) => write!(f, "u8"),
                    | Primitive::Int(Integer::I16, false) => write!(f, "u16"),
                    | Primitive::Int(Integer::I32, false) => write!(f, "u32"),
                    | Primitive::Int(Integer::I64, false) => write!(f, "u64"),
                    | Primitive::Int(Integer::I128, false) => write!(f, "u128"),
                    | Primitive::Int(Integer::I8, true) => write!(f, "i8"),
                    | Primitive::Int(Integer::I16, true) => write!(f, "i16"),
                    | Primitive::Int(Integer::I32, true) => write!(f, "i32"),
                    | Primitive::Int(Integer::I64, true) => write!(f, "i64"),
                    | Primitive::Int(Integer::I128, true) => write!(f, "i128"),
                    | Primitive::F32 => write!(f, "f32"),
                    | Primitive::F64 => write!(f, "f64"),
                    | Primitive::Pointer => write!(f, "ptr"),
                }?;

                if *s.valid_range.start() != 0 {
                    write!(f, "@{}..", s.valid_range.start())?;
                }

                if let Primitive::Pointer = s.value {
                    if let Some(Err(lyt)) = &self.elem {
                        write!(f, "->{}", lyt)?;
                    }
                }

                Ok(())
            },
            | Abi::ScalarPair(_, _) => {
                write!(f, "(")?;

                if let Fields::Arbitrary { fields } = &self.fields {
                    for (i, (_, field)) in fields.iter().enumerate() {
                        if i != 0 {
                            write!(f, ", ")?;
                        }

                        field.fmt(f)?;
                    }
                }

                write!(f, ")")
            },
            | Abi::Aggregate { .. } => match &self.variants {
                | Variants::Single { .. } => match &self.fields {
                    | Fields::Primitive => unreachable!(),
                    | Fields::Array { count, .. } => {
                        write!(f, "[{}]", count)?;

                        if let Some(Err(lyt)) = &self.elem {
                            lyt.fmt(f)?;
                        }

                        Ok(())
                    },
                    | Fields::Union { fields } => {
                        write!(f, "union {{")?;

                        for (i, field) in fields.iter().enumerate() {
                            if i != 0 {
                                write!(f, ",")?;
                            }

                            write!(f, " {}", field)?;
                        }

                        write!(f, " }}")
                    },
                    | Fields::Arbitrary { fields } => {
                        write!(f, "struct {{")?;

                        for (i, (_, field)) in fields.iter().enumerate() {
                            if i != 0 {
                                write!(f, ",")?;
                            }

                            write!(f, " {}", field)?;
                        }

                        write!(f, " }}")
                    },
                },
                | Variants::Multiple { variants, .. } => {
                    write!(f, "enum {{")?;

                    for (i, variant) in variants.iter().enumerate() {
                        if i != 0 {
                            write!(f, ",")?;
                        }

                        write!(f, " {}", variant)?;
                    }

                    write!(f, " }}")
                },
            },
        }
    }
}
