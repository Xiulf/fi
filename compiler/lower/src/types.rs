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
