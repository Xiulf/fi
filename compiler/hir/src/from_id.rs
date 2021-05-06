use hir_def::id::*;

macro_rules! from_id {
    ($($id:path : $ty:path),* $(,)?) => {
        $(
            impl From<$id> for $ty {
                fn from(id: $id) -> $ty {
                    $ty { id }
                }
            }

            impl From<$ty> for $id {
                fn from(ty: $ty) -> $id {
                    ty.id
                }
            }
        )*
    };
}

from_id! {
    base_db::libs::LibId: crate::Lib,
    ModuleId: crate::Module,
    FixityId: crate::Fixity,
    FuncId: crate::Func,
    StaticId: crate::Static,
    ConstId: crate::Const,
    TypeAliasId: crate::TypeAlias,
    TypeCtorId: crate::TypeCtor,
    ClassId: crate::Class,
    TypeVarId: crate::TypeVar,
}

impl From<ModuleDefId> for crate::ModuleDef {
    fn from(id: ModuleDefId) -> Self {
        match id {
            | ModuleDefId::ModuleId(it) => crate::ModuleDef::Module(it.into()),
            | ModuleDefId::FixityId(it) => crate::ModuleDef::Fixity(it.into()),
            | ModuleDefId::FuncId(it) => crate::ModuleDef::Func(it.into()),
            | ModuleDefId::StaticId(it) => crate::ModuleDef::Static(it.into()),
            | ModuleDefId::ConstId(it) => crate::ModuleDef::Const(it.into()),
            | ModuleDefId::TypeAliasId(it) => crate::ModuleDef::TypeAlias(it.into()),
            | ModuleDefId::TypeCtorId(it) => crate::ModuleDef::TypeCtor(it.into()),
            | ModuleDefId::CtorId(it) => crate::ModuleDef::Ctor(it.into()),
            | ModuleDefId::ClassId(it) => crate::ModuleDef::Class(it.into()),
        }
    }
}

impl From<CtorId> for crate::Ctor {
    fn from(id: CtorId) -> Self {
        crate::Ctor {
            parent: id.parent.into(),
            id: id.local_id,
        }
    }
}
