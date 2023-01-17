use std::ops::RangeInclusive;

use hir::attrs::{AttrInput, AttrInputGroup};
use hir::id::{CtorId, DefWithBodyId, LocalCtorId, TypeAliasId, TypeCtorId, TypeVarId};
use hir::ty::{Ty, TyKind, TypeVar};
use hir::{DefWithBody, HirDisplay};
use hir_def::data::CtorData;
use hir_def::lang_item;

use crate::db::MirDatabase;
use crate::instance::{Instance, InstanceDef};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Repr {
    Opaque,
    Uninhabited,
    TypeVar(TypeVar),
    Scalar(Scalar),
    Struct(Box<[Repr]>),
    Enum(Box<[Repr]>),
    Array(ArrayLen, Box<Repr>),
    Ptr(Box<Repr>, bool, bool),
    Box(Box<Repr>),
    Func(Box<Signature>, bool),
    Discr(Box<Repr>),
    ReprOf(Ty),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ArrayLen {
    Const(usize),
    TypeVar(TypeVar),
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

    pub fn usize() -> Self {
        Self::Scalar(Scalar {
            value: Primitive::Int(Integer::Int, false),
            valid_range: 0..=u128::MAX,
        })
    }

    pub fn isize() -> Self {
        Self::Scalar(Scalar {
            value: Primitive::Int(Integer::Int, true),
            valid_range: 0..=u128::MAX,
        })
    }

    pub fn i32() -> Self {
        Self::Scalar(Scalar {
            value: Primitive::Int(Integer::I32, true),
            valid_range: 0..=u128::MAX,
        })
    }
}

pub fn repr_of_query(db: &dyn MirDatabase, ty: Ty) -> Repr {
    tracing::debug!("{}", ty.display(db.upcast()));
    match ty.lookup(db.upcast()) {
        | TyKind::Error(e) => unreachable!("{e:?}"),
        | TyKind::Ctor(ctor) => repr_of_ctor(db, ctor, &[]),
        | TyKind::Alias(alias) => repr_of_alias(db, alias, &[]),
        | TyKind::App(mut base, args) => {
            let mut args = args.into_vec();
            while let TyKind::App(b, a) = base.lookup(db.upcast()) {
                args = [a.into_vec(), args].concat();
                base = b;
            }

            match base.lookup(db.upcast()) {
                | TyKind::Ctor(ctor) => repr_of_ctor(db, ctor, &args),
                | TyKind::Alias(alias) => repr_of_alias(db, alias, &args),
                | _ => unreachable!("{}", base.display(db.upcast())),
            }
        },
        | TyKind::TypeVar(var) => Repr::TypeVar(var),
        | TyKind::Where(_, ty) => db.repr_of(ty),
        | TyKind::ForAll(_, ty, _, _) => db.repr_of(ty),
        | k => todo!("{k:?}"),
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

    if data.is_foreign {
        Repr::Opaque
    } else if data.ctors.is_empty() {
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

    if group.ident("uninhabited") {
        repr = Repr::Uninhabited;
    }

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

    if let Some(arr) = group.field("array").and_then(AttrInput::group) {
        if let (Some(elem), Some(len)) = (arr.field("elem"), arr.field("len")) {
            let elem = if let Some(idx) = elem.int() {
                db.repr_of(args[idx as usize])
            } else {
                repr_from_attrs(db, elem.group().unwrap(), args)
            };

            let len = if let Some(idx) = len.int() {
                match args[idx as usize].lookup(db.upcast()) {
                    | TyKind::Figure(l) => ArrayLen::Const(l as usize),
                    | TyKind::TypeVar(v) => ArrayLen::TypeVar(v),
                    | _ => unreachable!(),
                }
            } else {
                ArrayLen::Const(len.string().unwrap().parse().unwrap())
            };

            repr = Repr::Array(len, Box::new(elem))
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

#[tracing::instrument(skip(db))]
pub fn func_signature_query(db: &dyn MirDatabase, instance: Instance) -> Signature {
    let func = match instance.def {
        | InstanceDef::Def(DefWithBody::Func(f)) => f,
        | _ => unreachable!(),
    };

    let hir_db: &dyn hir::db::HirDatabase = db.upcast();
    let lib = func.lib(hir_db).into();
    let func_ctor = db.lang_item(lib, lang_item::FN_TYPE).unwrap().as_type_ctor().unwrap();
    let mut ret = db.value_ty(hir::id::ValueTyDefId::FuncId(func.into())).ty;
    let mut args = Vec::new();

    tracing::debug!("{}", ret.display(db.upcast()));

    // if let Some(assoc_item) = func.as_assoc_item(hir_db) {
    //     if let hir::AssocItemContainer::Member(member) = assoc_item.container(hir_db) {
    //         let class = member.class(hir_db);

    //         if let Some(hir::AssocItem::Func(func)) = class.item(hir_db, &func.name(hir_db)) {
    //             ret = db.value_ty(hir::id::ValueTyDefId::FuncId(func.into())).ty;
    //         }
    //     }
    // }

    loop {
        match ret.lookup(hir_db) {
            | TyKind::ForAll(_, ty, _, _) => ret = ty,
            | TyKind::Where(_, ty) => ret = ty,
            | _ => break,
        }
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
    let sig = Signature { params, ret };

    if let Some(subst) = &instance.subst {
        subst.subst_signature(db, &sig)
    } else {
        sig
    }
}
