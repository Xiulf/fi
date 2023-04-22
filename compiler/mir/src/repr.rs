use std::ops::RangeInclusive;

use hir_def::attrs::{self, AttrInput, AttrInputGroup};
use hir_def::display::HirDisplay;
use hir_def::id::{CtorId, TypeCtorId, TypeVarId};
use hir_def::{item_tree, lang_item};
use hir_ty::ty::{FloatKind, IntegerKind, PrimitiveType, Ty, TyKind};
use triomphe::Arc;

use crate::Db;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Repr {
    Opaque,
    Uninhabited,
    TypeVar(TypeVarId),
    Scalar(Scalar),
    Struct(Box<[Arc<Repr>]>),
    Enum(Box<[Arc<Repr>]>),
    Array(ArrayLen, Arc<Repr>),
    Ptr(Arc<Repr>, bool, bool),
    Box(Arc<Repr>),
    Func(Signature, Option<Arc<Repr>>),
    Discr(Arc<Repr>),
    ReprOf(Ty),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ArrayLen {
    Const(usize),
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
    pub params: Box<[Arc<Repr>]>,
    pub ret: Arc<Repr>,
    pub is_varargs: bool,
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

#[salsa::tracked(recovery_fn = repr_of_cycle)]
pub fn repr_of(db: &dyn Db, ty: Ty) -> Arc<Repr> {
    tracing::trace!("{}", ty.display(db));
    match ty.kind(db) {
        | TyKind::Error => unreachable!(),
        | TyKind::Var(var) => Arc::new(Repr::TypeVar(*var)),
        | TyKind::Ctor(ctor) => repr_of_ctor(db, *ctor, &[]),
        | TyKind::Primitive(prim) => match prim {
            | PrimitiveType::Integer(kind) => repr_from_int_kind(*kind),
            | PrimitiveType::Float(kind) => repr_from_float_kind(*kind),
        },
        | TyKind::App(mut base, args) => {
            let mut args = args.to_vec();
            while let TyKind::App(b, a) = base.kind(db) {
                args = [a.to_vec(), args].concat();
                base = *b;
            }

            match base.kind(db) {
                | TyKind::Ctor(ctor) => repr_of_ctor(db, *ctor, &args),
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
            let env = if *env == Repr::unit() { None } else { Some(env) };

            Arc::new(Repr::Func(signature, env))
        },
        | k => todo!("{k:?}"),
    }
}

pub fn repr_of_cycle(_db: &dyn Db, _cycle: &salsa::Cycle, ty: Ty) -> Arc<Repr> {
    Arc::new(Repr::ReprOf(ty))
}

fn repr_of_ctor(db: &dyn Db, id: TypeCtorId, args: &[Ty]) -> Arc<Repr> {
    let lib = id.module(db).lib(db);
    let attrs = attrs::query(db, id.into());

    if let Some(attr) = attrs.by_key("repr").groups().next() {
        return repr_from_attrs(db, attr, args);
    }

    if let Some(kind) = db.type_cache().ctor_int_kind(db, id) {
        return repr_from_int_kind(kind);
    }

    if let Some(kind) = db.type_cache().ctor_float_kind(db, id) {
        return repr_from_float_kind(kind);
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

    if data.is_foreign {
        Arc::new(Repr::Opaque)
    } else if data.ctors.is_empty() {
        Arc::new(Repr::unit())
    } else if data.ctors.len() == 1 {
        let &local_id = data.ctors.iter().next().unwrap();
        let ctor = CtorId::new(db, id, local_id);
        repr_of_variant(db, ctor, args)
    } else {
        let variants = data
            .ctors
            .iter()
            .map(|&local_id| repr_of_variant(db, CtorId::new(db, id, local_id), args))
            .collect();

        Arc::new(Repr::Enum(variants))
    }
}

fn repr_of_variant(db: &dyn Db, ctor: CtorId, args: &[Ty]) -> Arc<Repr> {
    let ty = hir_ty::ctor_ty(db, ctor);
    let ty = ty.replace_vars(db, args);
    let fields = match ty.kind(db) {
        | TyKind::Func(func) => &*func.params,
        | _ => &[],
    };

    let fields = fields.iter().map(|&f| repr_of(db, f)).collect();

    Arc::new(Repr::Struct(fields))
}

fn repr_from_int_kind(kind: IntegerKind) -> Arc<Repr> {
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

    Arc::new(Repr::Scalar(Scalar {
        value: Primitive::Int(int, sign),
        valid_range: 0..=u128::MAX,
    }))
}

fn repr_from_float_kind(kind: FloatKind) -> Arc<Repr> {
    let value = match kind {
        | FloatKind::F32 => Primitive::Float,
        | FloatKind::F64 => Primitive::Double,
    };

    Arc::new(Repr::Scalar(Scalar {
        value,
        valid_range: 0..=u128::MAX,
    }))
}

fn repr_from_int(db: &dyn Db, args: &[Ty]) -> Arc<Repr> {
    match args[0].kind(db) {
        | TyKind::Primitive(PrimitiveType::Integer(i)) => repr_from_int_kind(*i),
        | TyKind::Var(v) => Arc::new(Repr::TypeVar(*v)),
        | k => unreachable!("{k:?}"),
    }
}

fn repr_from_float(db: &dyn Db, args: &[Ty]) -> Arc<Repr> {
    match args[0].kind(db) {
        | TyKind::Primitive(PrimitiveType::Float(f)) => repr_from_float_kind(*f),
        | TyKind::Var(v) => Arc::new(Repr::TypeVar(*v)),
        | _ => unreachable!(),
    }
}

fn repr_from_attrs(db: &dyn Db, group: &AttrInputGroup, args: &[Ty]) -> Arc<Repr> {
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
                repr_of(db, args[idx as usize])
            } else {
                repr_from_attrs(db, elem.group().unwrap(), args)
            };

            repr = Repr::Ptr(elem, false, nonnull);
        }
    }

    if let Some(ptr) = group.field("fatptr").and_then(AttrInput::group) {
        let nonnull = matches!(group.field("valid_range_start").and_then(AttrInput::int), Some(1..));

        if let Some(elem) = ptr.field("elem") {
            let elem = if let Some(idx) = elem.int() {
                repr_of(db, args[idx as usize])
            } else {
                repr_from_attrs(db, elem.group().unwrap(), args)
            };

            repr = Repr::Ptr(elem, true, nonnull);
        }
    }

    if let Some(arr) = group.field("array").and_then(AttrInput::group) {
        if let (Some(elem), Some(len)) = (arr.field("elem"), arr.field("len")) {
            let elem = if let Some(idx) = elem.int() {
                repr_of(db, args[idx as usize])
            } else {
                repr_from_attrs(db, elem.group().unwrap(), args)
            };

            let len = if let Some(idx) = len.int() {
                match args[idx as usize].kind(db) {
                    // | TyKind::Figure(l) => ArrayLen::Const(l as usize),
                    | TyKind::Var(v) => ArrayLen::TypeVar(*v),
                    | _ => unreachable!(),
                }
            } else {
                ArrayLen::Const(len.string().unwrap().parse().unwrap())
            };

            repr = Repr::Array(len, elem)
        }
    }

    // if let Some(func) = group.field("func").and_then(AttrInput::group) {
    //     if let Some(arg) = func.field("arg") {
    //         if let Some(ret) = func.field("ret") {
    //             let arg = if let Some(idx) = arg.int() {
    //                 repr_of(db, args[idx as usize])
    //             } else {
    //                 repr_from_attrs(db, arg.group().unwrap(), args)
    //             };

    //             let ret = if let Some(idx) = ret.int() {
    //                 repr_of(db, args[idx as usize])
    //             } else {
    //                 repr_from_attrs(db, ret.group().unwrap(), args)
    //             };

    //             repr = Repr::Func(
    //                 Box::new(Signature {
    //                     params: Arc::new([arg]),
    //                     ret,
    //                 }),
    //                 false,
    //             )
    //         }
    //     }
    // }

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

    Arc::new(repr)
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

// #[tracing::instrument(skip(db))]
// pub fn func_signature_query(db: &dyn MirDatabase, instance: Instance) -> Signature {
//     let func = match instance.def {
//         | InstanceDef::Def(DefWithBody::Func(f)) => f,
//         | _ => unreachable!(),
//     };

//     let hir_db: &dyn hir::db::HirDatabase = db.upcast();
//     let lib = func.lib(hir_db).into();
//     let func_ctor = db.lang_item(lib, lang_item::FN_TYPE).unwrap().as_type_ctor().unwrap();
//     let mut ret = db.value_ty(hir::id::ValueTyDefId::FuncId(func.into())).ty;
//     let mut args = Vec::new();

//     tracing::debug!("{}", ret.display(db.upcast()));

//     // if let Some(assoc_item) = func.as_assoc_item(hir_db) {
//     //     if let hir::AssocItemContainer::Member(member) = assoc_item.container(hir_db) {
//     //         let class = member.class(hir_db);

//     //         if let Some(hir::AssocItem::Func(func)) = class.item(hir_db, &func.name(hir_db)) {
//     //             ret = db.value_ty(hir::id::ValueTyDefId::FuncId(func.into())).ty;
//     //         }
//     //     }
//     // }

//     loop {
//         match ret.lookup(hir_db) {
//             | TyKind::ForAll(_, ty, _, _) => ret = ty,
//             | TyKind::Where(_, ty) => ret = ty,
//             | _ => break,
//         }
//     }

//     if func.has_body(hir_db) {
//         let body = db.body(DefWithBodyId::FuncId(func.into()));
//         let mut n = body.params().len();

//         while let Some(ty_args) = ret.match_ctor(hir_db, func_ctor) {
//             if n == 0 {
//                 break;
//             }

//             args.push(ty_args[0]);
//             ret = ty_args[1];
//             n -= 1;
//         }
//     } else if func.is_foreign(hir_db) {
//         while let Some(ty_args) = ret.match_ctor(hir_db, func_ctor) {
//             args.push(ty_args[0]);
//             ret = ty_args[1];
//         }
//     } else {
//         if let Some(ty_args) = ret.match_ctor(hir_db, func_ctor) {
//             args.push(ty_args[0]);
//             ret = ty_args[1];
//         }
//     }

//     let params = args.into_iter().map(|a| db.repr_of(a)).collect();
//     let ret = db.repr_of(ret);
//     let sig = Signature { params, ret };

//     if let Some(subst) = &instance.subst {
//         subst.subst_signature(db, &sig)
//     } else {
//         sig
//     }
// }
