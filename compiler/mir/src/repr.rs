use std::ops::RangeInclusive;

use hir::attrs::{AttrInput, AttrInputGroup};
use hir::id::{CtorId, LocalCtorId, TypeCtorId};
use hir::ty::{Ty, TyKind};
use hir_def::data::CtorData;

use crate::db::MirDatabase;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Repr {
    Opaque,
    Scalar(Scalar),
    Struct(Box<[Repr]>),
    Enum(Box<[Repr]>),
    Ptr(Box<Repr>),
    Box(Box<Repr>),
    Func(Box<Signature>, bool),
    ReprOf(Ty),
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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
}

pub fn repr_of_query(db: &dyn MirDatabase, ty: Ty) -> Repr {
    match ty.lookup(db.upcast()) {
        | TyKind::Error(_) => Repr::Opaque,
        | TyKind::Ctor(ctor) => repr_of_ctor(db, ctor, &[]),
        | TyKind::App(base, args) => match base.lookup(db.upcast()) {
            | TyKind::Ctor(ctor) => repr_of_ctor(db, ctor, &args),
            | _ => unreachable!(),
        },
        | k => todo!("{:?}", k),
    }
}

pub fn repr_of_cycle(_db: &dyn MirDatabase, _cycle: &Vec<String>, ty: &Ty) -> Repr {
    Repr::ReprOf(*ty)
}

fn repr_of_ctor(db: &dyn MirDatabase, id: TypeCtorId, args: &[Ty]) -> Repr {
    let attrs = db.attrs(id.into());
    let mut attrs = attrs.by_key("repr").attrs();

    if let Some(attr) = attrs.next().and_then(|a| a.group()) {
        return repr_from_attrs(db, attr, args);
    }

    let data = db.type_ctor_data(id);

    if data.ctors.is_empty() {
        Repr::Struct(Box::new([]))
    } else if let Some((local_id, ctor)) = data.ctors.iter().next() {
        repr_of_variant(db, local_id, ctor, id, args)
    } else {
        let variants = data
            .ctors
            .iter()
            .map(|(local_id, ctor)| repr_of_variant(db, local_id, ctor, id, args))
            .collect();

        Repr::Enum(variants)
    }
}

fn repr_of_variant(db: &dyn MirDatabase, local_id: LocalCtorId, ctor: &CtorData, id: TypeCtorId, args: &[Ty]) -> Repr {
    let lower = db.ctor_ty(CtorId { local_id, parent: id });
    let data = db.type_ctor_data(id);
    let fields = ctor
        .types
        .iter()
        .map(|&t| {
            let ty = lower.types[t];
            let ty = args
                .iter()
                .zip(data.type_vars.iter())
                .fold(ty, |r, (&t, &v)| r.replace_var(db.upcast(), v, t));

            db.repr_of(ty)
        })
        .collect();

    Repr::Struct(fields)
}

fn repr_from_attrs(db: &dyn MirDatabase, group: &AttrInputGroup, args: &[Ty]) -> Repr {
    let mut repr = Repr::Opaque;

    if let Some(val) = group.field("scalar").and_then(AttrInput::string) {
        repr = Repr::Scalar(Scalar {
            value: primitive_from_attr(val),
            valid_range: 0..=u128::MAX,
        });
    }

    if let Some(val) = group.field("valid_range_start").and_then(AttrInput::int) {
        if let Repr::Scalar(scalar) = &mut repr {
            scalar.valid_range = val as u128..=*scalar.valid_range.end();
        }
    }

    if let Some(val) = group.field("valid_range_end").and_then(AttrInput::int) {
        if let Repr::Scalar(scalar) = &mut repr {
            scalar.valid_range = *scalar.valid_range.start()..=val as u128;
        }
    }

    if let Some(ptr) = group.field("ptr").and_then(AttrInput::group) {
        if let Some(elem) = ptr.field("elem") {
            let elem = if let Some(idx) = elem.int() {
                db.repr_of(args[idx as usize])
            } else {
                repr_from_attrs(db, elem.group().unwrap(), args)
            };

            repr = Repr::Ptr(Box::new(elem));
        }
    }

    if let Some(func) = group.field("func").and_then(AttrInput::group) {
        if let Some(arg) = func.field("arg") {
            if let Some(ret) = func.field("ret") {
                let arg = if let Some(idx) = arg.int() {
                    db.repr_of(args[idx as usize])
                } else {
                    repr_from_attrs(db, arg.group().unwrap(), args)
                };

                let ret = if let Some(idx) = ret.int() {
                    db.repr_of(args[idx as usize])
                } else {
                    repr_from_attrs(db, ret.group().unwrap(), args)
                };

                repr = Repr::Func(
                    Box::new(Signature {
                        params: Box::new([arg]),
                        ret,
                    }),
                    false,
                )
            }
        }
    }

    repr
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
        | "ptr_sized_uint" => Primitive::Int(Integer::Int, false),
        | "ptr_sized_int" => Primitive::Int(Integer::Int, true),
        | _ => panic!("invalid scalar repr"),
    }
}
