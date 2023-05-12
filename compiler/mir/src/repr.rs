use std::ops::RangeInclusive;

use hir_def::attrs::{self, AttrInput, AttrInputGroup};
use hir_def::display::HirDisplay;
use hir_def::expr::Literal;
use hir_def::id::{CtorId, TypeCtorId, TypeVarId};
use hir_def::{item_tree, lang_item};
use hir_ty::ty::{FloatKind, IntegerKind, PrimitiveType, Ty, TyKind};
use rustc_hash::FxHashSet;

use crate::Db;

#[salsa::interned]
pub struct Repr {
    #[return_ref]
    pub kind: ReprKind,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ReprKind {
    Opaque,
    Uninhabited,
    TypeVar(TypeVarId),
    Scalar(Scalar),
    Struct(Box<[Repr]>),
    Enum(Box<[Repr]>),
    Array(ArrayLen, Repr),
    Slice(Repr),
    Ptr(Repr, bool, bool),
    Box(BoxKind, Repr),
    Func(Signature, Option<Repr>),
    Discr(Repr),
    ReprOf(Ty),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ArrayLen {
    Const(usize),
    TypeVar(TypeVarId),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum BoxKind {
    Box,
    Ref,
    Ptr,
    TypeVar(TypeVarId),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Scalar {
    pub value: Primitive,
    pub valid_range: RangeInclusive<u128>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Primitive {
    Int(Integer, bool),
    Float,
    Double,
    Pointer,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Integer {
    Int,
    I8,
    I16,
    I32,
    I64,
    I128,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Signature {
    pub params: Box<[Repr]>,
    pub ret: Repr,
    pub is_varargs: bool,
}

impl Repr {
    pub fn unit(db: &dyn Db) -> Self {
        Self::new(db, ReprKind::Struct(Box::new([])))
    }

    pub fn uninhabited(db: &dyn Db) -> Self {
        Self::new(db, ReprKind::Uninhabited)
    }

    pub fn usize(db: &dyn Db) -> Self {
        Self::new(
            db,
            ReprKind::Scalar(Scalar {
                value: Primitive::Int(Integer::Int, false),
                valid_range: 0..=u128::MAX,
            }),
        )
    }

    pub fn isize(db: &dyn Db) -> Self {
        Self::new(
            db,
            ReprKind::Scalar(Scalar {
                value: Primitive::Int(Integer::Int, true),
                valid_range: 0..=u128::MAX,
            }),
        )
    }

    pub fn i32(db: &dyn Db) -> Self {
        Self::new(
            db,
            ReprKind::Scalar(Scalar {
                value: Primitive::Int(Integer::I32, true),
                valid_range: 0..=u128::MAX,
            }),
        )
    }
}

#[salsa::tracked]
pub fn repr_of(db: &dyn Db, ty: Ty) -> Repr {
    tracing::trace!("{}", ty.display(db));

    // if hir_ty::ty::is_recursive(db, ty) {
    _repr_of_rec(db, ty, &mut FxHashSet::default())
    // } else {
    //     _repr_of(db, ty)
    // }
}

fn _repr_of(db: &dyn Db, ty: Ty) -> Repr {
    match ty.kind(db) {
        | TyKind::Error => unreachable!(),
        | TyKind::Var(var) => Repr::new(db, ReprKind::TypeVar(*var)),
        | TyKind::Ctor(ctor) => repr_of_ctor(db, *ctor, &[], repr_of),
        | TyKind::Primitive(prim) => match prim {
            | PrimitiveType::Integer(kind) => repr_from_int_kind(db, *kind),
            | PrimitiveType::Float(kind) => repr_from_float_kind(db, *kind),
            | PrimitiveType::Lifetime(_) => unreachable!(),
        },
        | TyKind::App(mut base, args) => {
            let mut args = args.to_vec();
            while let TyKind::App(b, a) = base.kind(db) {
                args = [a.to_vec(), args].concat();
                base = *b;
            }

            match base.kind(db) {
                | TyKind::Ctor(ctor) => repr_of_ctor(db, *ctor, &args, repr_of),
                | _ => unreachable!("{}", base.display(db)),
            }
        },
        | TyKind::Func(func) => {
            let params = func.params.iter().map(|&p| repr_of(db, p)).collect();
            let ret = repr_of(db, func.ret);
            let signature = Signature {
                params,
                ret,
                is_varargs: func.is_varargs,
            };

            let env = repr_of(db, func.env);
            let env = if env == Repr::unit(db) { None } else { Some(env) };

            Repr::new(db, ReprKind::Func(signature, env))
        },
        | k => todo!("{k:?}"),
    }
}

fn _repr_of_rec(db: &dyn Db, ty: Ty, seen: &mut FxHashSet<Ty>) -> Repr {
    if !seen.insert(ty) {
        return Repr::new(db, ReprKind::ReprOf(ty));
    }

    let repr = match ty.kind(db) {
        | TyKind::Error => unreachable!(),
        | TyKind::Never => Repr::new(db, ReprKind::Uninhabited),
        | TyKind::Var(var) => Repr::new(db, ReprKind::TypeVar(*var)),
        | TyKind::Ctor(ctor) => repr_of_ctor(db, *ctor, &[], |db, ty| _repr_of_rec(db, ty, seen)),
        | TyKind::Primitive(prim) => match prim {
            | PrimitiveType::Integer(kind) => repr_from_int_kind(db, *kind),
            | PrimitiveType::Float(kind) => repr_from_float_kind(db, *kind),
            | PrimitiveType::Lifetime(_) => unreachable!(),
        },
        | TyKind::Ref(lt, to) => {
            let to = _repr_of_rec(db, *to, seen);
            let kind = match lt.kind(db) {
                | TyKind::Var(v) => BoxKind::TypeVar(*v),
                | _ => unreachable!("({}) ({})", lt.display(db), to.display(db)),
            };

            Repr::new(db, ReprKind::Box(kind, to))
        },
        | TyKind::App(mut base, args) => {
            let mut args = args.to_vec();
            while let TyKind::App(b, a) = base.kind(db) {
                args = [a.to_vec(), args].concat();
                base = *b;
            }

            match base.kind(db) {
                | TyKind::Ctor(ctor) => repr_of_ctor(db, *ctor, &args, |db, ty| _repr_of_rec(db, ty, seen)),
                | _ => unreachable!("{}", base.display(db)),
            }
        },
        | TyKind::Func(func) => {
            let params = func.params.iter().map(|&p| _repr_of_rec(db, p, seen)).collect();
            let ret = _repr_of_rec(db, func.ret, seen);
            let signature = Signature {
                params,
                ret,
                is_varargs: func.is_varargs,
            };

            let env = _repr_of_rec(db, func.env, seen);
            let env = if env == Repr::unit(db) { None } else { Some(env) };

            Repr::new(db, ReprKind::Func(signature, env))
        },
        | k => todo!("{k:?}"),
    };

    seen.remove(&ty);
    repr
}

fn repr_of_ctor(db: &dyn Db, id: TypeCtorId, args: &[Ty], mut repr_of: impl FnMut(&dyn Db, Ty) -> Repr) -> Repr {
    let lib = id.module(db).lib(db);
    let attrs = attrs::query(db, id.into());

    if let Some(attr) = attrs.by_key("repr").groups().next() {
        return repr_from_attrs(db, attr, args, &mut repr_of);
    }

    if let Some(kind) = db.type_cache().ctor_int_kind(db, id) {
        return repr_from_int_kind(db, kind);
    }

    if let Some(kind) = db.type_cache().ctor_float_kind(db, id) {
        return repr_from_float_kind(db, kind);
    }

    if Some(id) == lang_item::query(db, lib, lang_item::INT_TYPE).and_then(lang_item::LangItem::as_type_ctor) {
        return repr_from_int(db, args);
    }

    if Some(id) == lang_item::query(db, lib, lang_item::FLOAT_TYPE).and_then(lang_item::LangItem::as_type_ctor) {
        return repr_from_float(db, args);
    }

    let it = id.it(db);
    let item_tree = item_tree::query(db, it.file);
    let data = &item_tree[it.value];
    let mut repr = if data.is_foreign {
        Repr::new(db, ReprKind::Opaque)
    } else if data.ctors.is_empty() {
        Repr::unit(db)
    } else if data.ctors.len() == 1 {
        let &local_id = data.ctors.iter().next().unwrap();
        let ctor = CtorId::new(db, id, local_id);
        repr_of_variant(db, ctor, args, repr_of)
    } else {
        let variants = data
            .ctors
            .iter()
            .map(|&local_id| repr_of_variant(db, CtorId::new(db, id, local_id), args, &mut repr_of))
            .collect();

        Repr::new(db, ReprKind::Enum(variants))
    };

    if attrs.by_key("boxed").exists() {
        repr = Repr::new(db, ReprKind::Box(BoxKind::Box, repr));
    }

    repr
}

fn repr_of_variant(db: &dyn Db, ctor: CtorId, args: &[Ty], mut repr_of: impl FnMut(&dyn Db, Ty) -> Repr) -> Repr {
    let ty = hir_ty::ctor_ty(db, ctor);
    let ty = ty.replace_vars(db, args);
    let fields = match ty.kind(db) {
        | TyKind::Func(func) => &*func.params,
        | _ => &[],
    };

    let fields = fields.iter().map(|&f| repr_of(db, f)).collect();

    Repr::new(db, ReprKind::Struct(fields))
}

fn repr_from_int_kind(db: &dyn Db, kind: IntegerKind) -> Repr {
    let (int, sign) = match kind {
        | IntegerKind::I8 => (Integer::I8, true),
        | IntegerKind::I16 => (Integer::I16, true),
        | IntegerKind::I32 => (Integer::I32, true),
        | IntegerKind::I64 => (Integer::I64, true),
        | IntegerKind::I128 => (Integer::I128, true),
        | IntegerKind::Isize => (Integer::Int, true),
        | IntegerKind::U8 => (Integer::I8, false),
        | IntegerKind::U16 => (Integer::I16, false),
        | IntegerKind::U32 => (Integer::I32, false),
        | IntegerKind::U64 => (Integer::I64, false),
        | IntegerKind::U128 => (Integer::I128, false),
        | IntegerKind::Usize => (Integer::Int, false),
    };

    Repr::new(
        db,
        ReprKind::Scalar(Scalar {
            value: Primitive::Int(int, sign),
            valid_range: 0..=u128::MAX,
        }),
    )
}

fn repr_from_float_kind(db: &dyn Db, kind: FloatKind) -> Repr {
    let value = match kind {
        | FloatKind::F32 => Primitive::Float,
        | FloatKind::F64 => Primitive::Double,
    };

    Repr::new(
        db,
        ReprKind::Scalar(Scalar {
            value,
            valid_range: 0..=u128::MAX,
        }),
    )
}

fn repr_from_int(db: &dyn Db, args: &[Ty]) -> Repr {
    match args[0].kind(db) {
        | TyKind::Primitive(PrimitiveType::Integer(i)) => repr_from_int_kind(db, *i),
        | TyKind::Var(v) => Repr::new(db, ReprKind::TypeVar(*v)),
        | k => unreachable!("{k:?}"),
    }
}

fn repr_from_float(db: &dyn Db, args: &[Ty]) -> Repr {
    match args[0].kind(db) {
        | TyKind::Primitive(PrimitiveType::Float(f)) => repr_from_float_kind(db, *f),
        | TyKind::Var(v) => Repr::new(db, ReprKind::TypeVar(*v)),
        | _ => unreachable!(),
    }
}

fn repr_from_attrs(
    db: &dyn Db,
    group: &AttrInputGroup,
    args: &[Ty],
    repr_of: &mut dyn FnMut(&dyn Db, Ty) -> Repr,
) -> Repr {
    let mut repr = ReprKind::Opaque;

    if group.ident("uninhabited") {
        repr = ReprKind::Uninhabited;
    }

    if let Some(val) = group.field("scalar").and_then(AttrInput::string) {
        repr = ReprKind::Scalar(Scalar {
            value: primitive_from_attr(val),
            valid_range: 0..=u128::MAX,
        });
    }

    if let Some(val) = group.field("valid_range_start").and_then(AttrInput::int) {
        if let ReprKind::Scalar(scalar) = &mut repr {
            scalar.valid_range = val as u128..=*scalar.valid_range.end();
        }
    }

    if let Some(val) = group.field("valid_range_end").and_then(AttrInput::int) {
        if let ReprKind::Scalar(scalar) = &mut repr {
            scalar.valid_range = *scalar.valid_range.start()..=val as u128;
        }
    }

    if let Some(ptr) = group.field("ptr").and_then(AttrInput::group) {
        let nonnull = matches!(group.field("valid_range_start").and_then(AttrInput::int), Some(1..));

        if let Some(elem) = ptr.field("elem") {
            let elem = if let Some(idx) = elem.int() {
                repr_of(db, args[idx as usize])
            } else {
                repr_from_attrs(db, elem.group().unwrap(), args, repr_of)
            };

            repr = ReprKind::Ptr(elem, false, nonnull);
        }
    }

    if let Some(ptr) = group.field("fatptr").and_then(AttrInput::group) {
        let nonnull = matches!(group.field("valid_range_start").and_then(AttrInput::int), Some(1..));

        if let Some(elem) = ptr.field("elem") {
            let elem = if let Some(idx) = elem.int() {
                repr_of(db, args[idx as usize])
            } else {
                repr_from_attrs(db, elem.group().unwrap(), args, repr_of)
            };

            repr = ReprKind::Ptr(elem, true, nonnull);
        }
    }

    if let Some(slice) = group.field("slice").and_then(AttrInput::group) {
        if let Some(elem) = slice.field("elem") {
            let elem = if let Some(idx) = elem.int() {
                repr_of(db, args[idx as usize])
            } else {
                repr_from_attrs(db, elem.group().unwrap(), args, repr_of)
            };

            repr = ReprKind::Slice(elem);
        }
    }

    if let Some(arr) = group.field("array").and_then(AttrInput::group) {
        if let (Some(elem), Some(len)) = (arr.field("elem"), arr.field("len")) {
            let elem = if let Some(idx) = elem.int() {
                repr_of(db, args[idx as usize])
            } else {
                repr_from_attrs(db, elem.group().unwrap(), args, repr_of)
            };

            let len = if let Some(idx) = len.int() {
                match args[idx as usize].kind(db) {
                    | TyKind::Literal(Literal::Int(l)) => ArrayLen::Const(*l as usize),
                    | TyKind::Var(v) => ArrayLen::TypeVar(*v),
                    | _ => unreachable!(),
                }
            } else {
                ArrayLen::Const(len.string().unwrap().parse().unwrap())
            };

            repr = ReprKind::Array(len, elem)
        }
    }

    if let Some(count) = group.field("fields").and_then(AttrInput::int) {
        let mut fields = Vec::new();

        for i in 0..count {
            let name = format!("f{i}");

            if let Some(group) = group.field(&name).and_then(AttrInput::group) {
                fields.push(repr_from_attrs(db, group, args, repr_of));
            }
        }

        repr = ReprKind::Struct(fields.into_boxed_slice());
    }

    Repr::new(db, repr)
}

fn primitive_from_attr(attr: &str) -> Primitive {
    match attr {
        | "u8" => Primitive::Int(Integer::I8, false),
        | "u16" => Primitive::Int(Integer::I16, false),
        | "u32" => Primitive::Int(Integer::I32, false),
        | "u64" => Primitive::Int(Integer::I64, false),
        | "u128" => Primitive::Int(Integer::I128, false),
        | "i8" => Primitive::Int(Integer::I8, true),
        | "i16" => Primitive::Int(Integer::I16, true),
        | "i32" => Primitive::Int(Integer::I32, true),
        | "i64" => Primitive::Int(Integer::I64, true),
        | "i128" => Primitive::Int(Integer::I128, true),
        | "f32" => Primitive::Float,
        | "f64" => Primitive::Double,
        | "ptr" => Primitive::Pointer,
        | "usize" => Primitive::Int(Integer::Int, false),
        | "isize" => Primitive::Int(Integer::Int, true),
        | _ => panic!("invalid scalar repr '{}'", attr),
    }
}

#[salsa::tracked]
pub fn needs_drop(db: &dyn Db, repr: Repr) -> bool {
    match repr.kind(db) {
        | ReprKind::Box(BoxKind::Box, _) => true,
        | _ => false,
    }
}
