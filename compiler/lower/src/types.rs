use crate::db::LowerDatabase;
use hir::id::TypeCtorId;
use hir::ty::TyKind;

pub fn is_boxed(db: &dyn LowerDatabase, id: TypeCtorId) -> bool {
    let lower = db.type_for_ctor(id);

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
        | _ => todo!(),
    }

    id
}

pub fn lower_type(db: &dyn LowerDatabase, mut ty: hir::ty::Ty) -> ir::ty::Ty {
    let mut args = Vec::new();

    while let TyKind::App(a, b) = ty.lookup(db.upcast()) {
        args.push(b);
        ty = a;
    }

    args.reverse();

    match ty.lookup(db.upcast()) {
        | TyKind::Error => ir::ty::Ty::unit(db.upcast()),
        | TyKind::Unknown(_)
        | TyKind::Skolem(_, _)
        | TyKind::Row(_, _)
        | TyKind::Figure(_)
        | TyKind::Symbol(_)
        | TyKind::App(_, _) => unreachable!(),
        | TyKind::Ctor(id) => {
            let ir = db.type_ir(id.into());
            let boxed = db.is_boxed(id);

            todo!("{:?}", ir.lookup(db.upcast()))
        },
        | t => todo!("{:?}", t),
    }
}
