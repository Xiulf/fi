use crate::db::LowerDatabase;
use arena::ArenaMap;
use hir::id::{TypeCtorId, TypeVarId};
use hir::ty::{TyKind, TypeVarScopeId};
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
    let name = ty.path(db.upcast()).to_string();
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
        | TyKind::App(base, args) => (base, args),
        | _ => (ty, Box::new([]) as Box<[_]>),
    };

    match ty.lookup(db.upcast()) {
        | TyKind::Error(_) => ir::ty::Ty::unit(db.upcast()),
        | TyKind::Row(_, _) | TyKind::Figure(_) | TyKind::Symbol(_) | TyKind::App(_, _) => unreachable!(),
        | TyKind::TypeVar(tv) => ir::ty::Ty::new(
            db.upcast(),
            ir::ty::TypeKind::Var(type_vars[tv.scope()][tv.idx() as usize]),
            ir::Flags::EMPTY,
        ),
        | TyKind::Func(args, ret) => {
            let mut sig = ir::ty::Signature::new();

            for &arg in args.iter() {
                sig = sig.param(db.upcast(), lower_type_inner(db, arg, type_vars));
            }

            sig = sig.ret(db.upcast(), lower_type_inner(db, ret, type_vars));

            ir::ty::Ty::new(db.upcast(), ir::ty::TypeKind::Func(sig), ir::Flags::EMPTY)
        },
        | TyKind::Ctor(id) => {
            let ir = db.type_ir(id.into());
            let boxed = is_boxed(db, id);

            todo!("{:?}", ir.lookup(db.upcast()))
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
        | t => todo!("{:?}", t),
    }
}
