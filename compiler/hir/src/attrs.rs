pub use hir_def::attrs::*;
use hir_def::id::AttrDefId;
use hir_ty::db::HirDatabase;

use crate::*;

pub trait HasAttrs {
    fn attrs(self, db: &dyn HirDatabase) -> AttrsWithOwner;
}

macro_rules! impl_has_attrs {
    ($(($def:ident, $def_id:ident),)*) => {$(
        impl HasAttrs for $def {
            fn attrs(self, db: &dyn HirDatabase) -> AttrsWithOwner {
                let def = AttrDefId::$def_id(self.into());
                db.attrs(def)
            }
        }
    )*};
}

impl_has_attrs! {
    (Module, ModuleId),
    (Fixity, FixityId),
    (Func, FuncId),
    (Static, StaticId),
    (Const, ConstId),
    (TypeAlias, TypeAliasId),
    (TypeCtor, TypeCtorId),
    (Class, ClassId),
    (Member, MemberId),
}

impl HasAttrs for AssocItem {
    fn attrs(self, db: &dyn HirDatabase) -> AttrsWithOwner {
        match self {
            | Self::Func(it) => it.attrs(db),
            | Self::Static(it) => it.attrs(db),
        }
    }
}
