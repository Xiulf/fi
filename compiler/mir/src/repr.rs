use std::ops::RangeInclusive;

use hir::attrs::{AttrInput, AttrInputGroup};
use hir::id::{CtorId, DefWithBodyId, LocalCtorId, TypeAliasId, TypeCtorId, TypeVarId};
use hir::ty::{Ty, TyKind};
use hir_def::data::CtorData;
use hir_def::lang_item;

use crate::db::MirDatabase;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Repr {
    Opaque,
    Scalar(Scalar),
    Struct(Box<[Repr]>),
    Enum(Box<[Repr]>),
    Ptr(Box<Repr>, bool, bool),
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
}

impl Repr {
    pub fn unit() -> Self {
        Self::Struct(Box::new([]))
    }

    pub fn i32() -> Self {
        Self::Scalar(Scalar {
            value: Primitive::Int(Integer::I32, true),
            valid_range: 0..=u128::MAX,
        })
    }

    pub fn discr(&self) -> Self {
        match self {
            | Repr::Enum(variants) if variants.len() > 1 => {
                let value = Primitive::Int(self.align(), false);

                Self::Scalar(Scalar {
                    value,
                    valid_range: 0..=variants.len() as u128 - 1,
                })
            },
            | _ => unreachable!(),
        }
    }

    fn align(&self) -> Integer {
        match self {
            | Repr::Opaque => Integer::Int,
            | Repr::Scalar(s) => match s.value {
                | Primitive::Int(i, _) => i,
                | Primitive::Float => Integer::I32,
                | Primitive::Double => Integer::I64,
                | Primitive::Pointer => Integer::Int,
            },
            | Repr::Box(_) => Integer::Int,
            | Repr::Ptr(_, _, _) => Integer::Int,
            | Repr::Func(_, _) => Integer::Int,
            | Repr::Struct(fs) => fs.iter().map(Self::align).max().unwrap_or(Integer::I8),
            | Repr::Enum(vs) => vs.iter().map(Self::align).max().unwrap_or(Integer::I8),
            | Repr::ReprOf(_) => todo!(),
        }
    }
}

pub fn repr_of_query(db: &dyn MirDatabase, ty: Ty) -> Repr {
    match ty.lookup(db.upcast()) {
        | TyKind::Error(_) => Repr::Opaque,
        | TyKind::Ctor(ctor) => repr_of_ctor(db, ctor, &[]),
        | TyKind::Alias(alias) => repr_of_alias(db, alias, &[]),
        | TyKind::App(base, args) => match base.lookup(db.upcast()) {
            | TyKind::Ctor(ctor) => repr_of_ctor(db, ctor, &args),
            | TyKind::Alias(alias) => repr_of_alias(db, alias, &args),
            | _ => unreachable!(),
        },
        | TyKind::Where(_, ty) => repr_of_query(db, ty),
        | k => todo!("{:?}", k),
    }
}

pub fn repr_of_cycle(_db: &dyn MirDatabase, _cycle: &Vec<String>, ty: &Ty) -> Repr {
    Repr::ReprOf(*ty)
}

fn repr_of_alias(db: &dyn MirDatabase, id: TypeAliasId, args: &[Ty]) -> Repr {
    let lower = db.type_for_alias(id);
    let data = db.type_alias_data(id);
    let ty = args.iter().zip(data.type_vars.iter()).fold(lower.ty.ty, |r, (&t, &v)| {
        r.replace_var(
            db.upcast(),
            TypeVarId {
                owner: hir::id::TypedDefId::TypeAliasId(id).into(),
                local_id: v,
            },
            t,
        )
    });

    db.repr_of(ty)
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
    } else if data.ctors.len() == 1 {
        let (local_id, ctor) = data.ctors.iter().next().unwrap();
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
            let ty = args.iter().zip(data.type_vars.iter()).fold(ty, |r, (&t, &v)| {
                r.replace_var(
                    db.upcast(),
                    TypeVarId {
                        owner: hir::id::TypedDefId::TypeCtorId(id).into(),
                        local_id: v,
                    },
                    t,
                )
            });

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
        let nonnull = matches!(group.field("valid_range_start").and_then(AttrInput::int), Some(1..));

        if let Some(elem) = ptr.field("elem") {
            let elem = if let Some(idx) = elem.int() {
                db.repr_of(args[idx as usize])
            } else {
                repr_from_attrs(db, elem.group().unwrap(), args)
            };

            repr = Repr::Ptr(Box::new(elem), false, nonnull);
        }
    }

    if let Some(ptr) = group.field("fatptr").and_then(AttrInput::group) {
        let nonnull = matches!(group.field("valid_range_start").and_then(AttrInput::int), Some(1..));

        if let Some(elem) = ptr.field("elem") {
            let elem = if let Some(idx) = elem.int() {
                db.repr_of(args[idx as usize])
            } else {
                repr_from_attrs(db, elem.group().unwrap(), args)
            };

            repr = Repr::Ptr(Box::new(elem), true, nonnull);
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

    if let Some(count) = group.field("fields").and_then(AttrInput::int) {
        let mut fields = Vec::new();

        for i in 0..count {
            let name = format!("f{i}");

            if let Some(group) = group.field(&name).and_then(AttrInput::group) {
                fields.push(repr_from_attrs(db, group, args));
            }
        }

        repr = Repr::Struct(fields.into_boxed_slice());
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
        | "usize" => Primitive::Int(Integer::Int, false),
        | "isize" => Primitive::Int(Integer::Int, true),
        | _ => panic!("invalid scalar repr '{}'", attr),
    }
}

pub fn func_signature_query(db: &dyn MirDatabase, func: hir::Func) -> Signature {
    let hir_db: &dyn hir::db::HirDatabase = db.upcast();
    let infer = db.infer(DefWithBodyId::FuncId(func.into()));
    let lib = func.lib(hir_db).into();
    let func_ctor = db.lang_item(lib, lang_item::FN_TYPE).unwrap().as_type_ctor().unwrap();
    let mut ret = infer.self_type.ty;
    let mut args = Vec::new();

    while let TyKind::Where(_, ty) = ret.lookup(hir_db) {
        ret = ty;
    }

    if func.has_body(hir_db) {
        let body = db.body(DefWithBodyId::FuncId(func.into()));
        let mut n = body.params().len();

        while let Some(ty_args) = ret.match_ctor(hir_db, func_ctor) {
            if n == 0 {
                break;
            }

            args.push(ty_args[0]);
            ret = ty_args[1];
            n -= 1;
        }
    } else if func.is_foreign(hir_db) {
        while let Some(ty_args) = ret.match_ctor(hir_db, func_ctor) {
            args.push(ty_args[0]);
            ret = ty_args[1];
        }
    } else {
        if let Some(ty_args) = ret.match_ctor(hir_db, func_ctor) {
            args.push(ty_args[0]);
            ret = ty_args[1];
        }
    }

    let params = args.into_iter().map(|a| db.repr_of(a)).collect();
    let ret = db.repr_of(ret);

    Signature { params, ret }
}
