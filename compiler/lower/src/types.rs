use crate::db::LowerDatabase;
use arena::ArenaMap;
use hir::attrs::{AttrInput, AttrInputGroup};
use hir::id::TypeCtorId;
use hir::ty::{TyKind, TypeVarScopeId};
use ir::layout::{Integer, Primitive};
use ir::ty::GenericVar;

pub fn is_boxed(db: &dyn LowerDatabase, id: TypeCtorId) -> bool {
    let lower = db.kind_for_ctor(id);

    for (_, ty) in lower.types.iter() {
        match ty.lookup(db.upcast()) {
            | TyKind::Ctor(id2) if id == id2 => return true,
            | _ => {},
        }
    }

    false
}

pub fn type_ir(db: &dyn LowerDatabase, ty: hir::TypeCtor) -> ir::TypeDefId {
    let name = ty.name(db.upcast()).to_string();
    let id = ir::TypeDef::declare(db.upcast(), name);
    let def = id.lookup(db.upcast());
    let data = db.type_ctor_data(ty.into());

    match data.ctors.len() {
        | 0 => def.define_struct([]),
        | 1 => {
            let (local_id, ctor) = data.ctors.iter().next().unwrap();
            let lower = db.ctor_ty(hir::id::CtorId {
                local_id,
                parent: ty.into(),
            });

            let fields = ctor.types.iter().enumerate().map(|(i, &t)| ir::TypeDefField {
                name: i.to_string(),
                ty: db.lower_type(lower.types[t]),
            });

            def.define_struct(fields);
        },
        | _ => {
            let variants = data.ctors.iter().map(|(local_id, ctor)| {
                let lower = db.ctor_ty(hir::id::CtorId {
                    local_id,
                    parent: ty.into(),
                });

                let payload = if !ctor.types.is_empty() {
                    let fields = ctor.types.iter().map(|&t| db.lower_type(lower.types[t]));
                    let fields = ir::ty::Ty::tuple(db.upcast(), fields);

                    Some(fields)
                } else {
                    None
                };

                ir::TypeDefVariant {
                    name: ctor.name.to_string(),
                    payload,
                }
            });

            def.define_enum(variants)
        },
    }

    id
}

pub fn lower_type(db: &dyn LowerDatabase, ty: hir::ty::Ty) -> ir::ty::Ty {
    lower_type_inner(db, ty, &mut ArenaMap::default())
}

fn lower_type_inner(
    db: &dyn LowerDatabase,
    ty: hir::ty::Ty,
    type_vars: &mut ArenaMap<TypeVarScopeId, Vec<GenericVar>>,
) -> ir::ty::Ty {
    let (ty, args) = match ty.lookup(db.upcast()) {
        | TyKind::App(base, args) => (base, Some(args)),
        | _ => (ty, None),
    };

    match ty.lookup(db.upcast()) {
        | TyKind::Error(_) => ir::ty::Ty::unit(db.upcast()),
        | TyKind::Row(_, _) | TyKind::Figure(_) | TyKind::Symbol(_) | TyKind::App(_, _) => unreachable!(),
        | TyKind::TypeVar(tv) => ir::ty::Ty::new(
            db.upcast(),
            ir::ty::TypeKind::Var(type_vars[tv.scope()][tv.idx() as usize]),
            ir::Flags::EMPTY,
        ),
        | TyKind::Tuple(tys) => {
            let tys = tys.iter().map(|&t| lower_type_inner(db, t, type_vars));

            ir::ty::Ty::tuple(db.upcast(), tys)
        },
        | TyKind::Func(args, ret) => {
            let mut sig = ir::ty::Signature::new();

            for &arg in args.iter() {
                sig = sig.param(db.upcast(), lower_type_inner(db, arg, type_vars));
            }

            sig = sig.ret(db.upcast(), lower_type_inner(db, ret, type_vars));

            ir::ty::Ty::new(db.upcast(), ir::ty::TypeKind::Func(sig), ir::Flags::EMPTY)
        },
        | TyKind::Ctor(id) => {
            let attrs = db.attrs(id.into());
            let mut attrs = attrs.by_key("repr").attrs();

            if let Some(attr) = attrs.next().and_then(|a| a.group()) {
                lower_repr(db, attr, args.as_deref().unwrap_or(&[]), type_vars)
            } else {
                let args = args.map(|a| {
                    a.iter()
                        .map(|&a| match a.lookup(db.upcast()) {
                            | TyKind::Figure(i) => ir::ty::Subst::Figure(i as u128),
                            | TyKind::Symbol(s) => ir::ty::Subst::Symbol(s.into()),
                            | _ => ir::ty::Subst::Type(lower_type_inner(db, a, type_vars)),
                        })
                        .collect::<Vec<_>>()
                });

                let boxed = is_boxed(db, id);
                let id = db.type_ir(id.into());
                let ty = ir::ty::Ty::def(db.upcast(), id, args);

                if boxed {
                    ty.boxed(ir::ty::BoxKind::Rc, db.upcast())
                } else {
                    ty
                }
            }
        },
        | TyKind::Ctnt(_, inner) => {
            // TODO: handle constraint
            lower_type_inner(db, inner, type_vars)
        },
        | TyKind::ForAll(kinds, inner, scope) => {
            let mut gen = ir::ty::Ty::generic();
            let mut params = Vec::with_capacity(kinds.len());

            for _ in kinds.iter() {
                params.push(gen.add_param(ir::ty::GenericParam::Type));
            }

            type_vars.insert(scope, params);
            gen.finish(db.upcast(), lower_type_inner(db, inner, type_vars))
        },
    }
}

fn lower_repr(
    db: &dyn LowerDatabase,
    group: &AttrInputGroup,
    args: &[hir::ty::Ty],
    type_vars: &mut ArenaMap<TypeVarScopeId, Vec<GenericVar>>,
) -> ir::ty::Ty {
    let mut repr = ir::ty::Repr::default();
    let mut kind = ir::ty::TypeKind::Unit;

    if group.ident("uninhabited") {
        repr.uninhabited = true;
    }

    if let Some(val) = group.field("scalar").and_then(AttrInput::string) {
        repr.scalar = Some(scalar_from_repr(val));
    }

    if let Some(val) = group.field("valid_range_start").and_then(AttrInput::int) {
        repr.valid_range_start = Some(val as u128);
    }

    if let Some(val) = group.field("valid_range_end").and_then(AttrInput::int) {
        repr.valid_range_end = Some(val as u128);
    }

    if let Some(ptr) = group.field("ptr").and_then(AttrInput::group) {
        if let Some(elem) = ptr.field("elem") {
            let elem = if let Some(idx) = elem.int() {
                lower_type_inner(db, args[idx as usize], type_vars)
            } else {
                lower_repr(db, elem.group().unwrap(), args, type_vars)
            };

            kind = ir::ty::TypeKind::Ptr(elem);
        }
    }

    if let Some(array) = group.field("array").and_then(AttrInput::group) {
        if let (Some(elem), Some(len)) = (array.field("elem"), array.field("len")) {
            let elem = if let Some(idx) = elem.int() {
                lower_type_inner(db, args[idx as usize], type_vars)
            } else {
                lower_repr(db, elem.group().unwrap(), args, type_vars)
            };

            let len = if let Some(len) = len.int() {
                match args[len as usize].lookup(db.upcast()) {
                    | TyKind::Figure(len) => len as u64,
                    | _ => panic!(),
                }
            } else {
                len.string().and_then(|s| s.parse().ok()).unwrap()
            };

            kind = ir::ty::TypeKind::Array(elem, len);
        }
    }

    if let Some(record) = group.field("record").and_then(AttrInput::group) {
        if let Some(fields) = record.field("fields").and_then(AttrInput::int) {
            if let TyKind::Row(fields, _tail) = args[fields as usize].lookup(db.upcast()) {
                let fields = fields.iter().map(|f| lower_type_inner(db, f.ty, type_vars)).collect();

                kind = ir::ty::TypeKind::Tuple(fields);
            }
        }
    }

    if let Some(count) = group.field("fields").and_then(AttrInput::int) {
        let mut fields = Vec::with_capacity(count as usize);

        for i in 0..count {
            if let Some(field) = group.field(&format!("f{}", i)).and_then(AttrInput::group) {
                fields.push(lower_repr(db, field, args, type_vars));
            }
        }

        kind = ir::ty::TypeKind::Tuple(fields);
    }

    db.intern_type(
        ir::ty::Type {
            flags: ir::Flags::EMPTY,
            repr,
            kind,
        }
        .into(),
    )
}

fn scalar_from_repr(repr: &str) -> Primitive {
    match repr {
        | "u8" => Primitive::Int(Integer::I8, false),
        | "u16" => Primitive::Int(Integer::I16, false),
        | "u32" => Primitive::Int(Integer::I32, false),
        | "u64" => Primitive::Int(Integer::I64, false),
        | "u128" => Primitive::Int(Integer::I128, false),
        | "usize" => Primitive::Int(Integer::ISize, false),
        | "i8" => Primitive::Int(Integer::I8, true),
        | "i16" => Primitive::Int(Integer::I16, true),
        | "i32" => Primitive::Int(Integer::I32, true),
        | "i64" => Primitive::Int(Integer::I64, true),
        | "i128" => Primitive::Int(Integer::I128, true),
        | "isize" => Primitive::Int(Integer::ISize, true),
        | "f32" => Primitive::F32,
        | "f64" => Primitive::F64,
        | "ptr" => Primitive::Pointer,
        | _ => panic!("invalid scalar repr: {}", repr),
    }
}
