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
    ForeignId: crate::Foreign,
    FuncId: crate::Func,
    StaticId: crate::Static,
    ConstId: crate::Const,
    TypeId: crate::Type,
    CtorId: crate::Ctor,
    ClassId: crate::Class,
}

impl From<ModuleDefId> for crate::ModuleDef {
    fn from(id: ModuleDefId) -> Self {
        match id {
            | ModuleDefId::ModuleId(it) => crate::ModuleDef::Module(it.into()),
            | ModuleDefId::FixityId(it) => crate::ModuleDef::Fixity(it.into()),
            | ModuleDefId::ForeignId(it) => crate::ModuleDef::Foreign(it.into()),
            | ModuleDefId::FuncId(it) => crate::ModuleDef::Func(it.into()),
            | ModuleDefId::StaticId(it) => crate::ModuleDef::Static(it.into()),
            | ModuleDefId::ConstId(it) => crate::ModuleDef::Const(it.into()),
            | ModuleDefId::TypeId(it) => crate::ModuleDef::Type(it.into()),
            | ModuleDefId::CtorId(it) => crate::ModuleDef::Ctor(it.into()),
            | ModuleDefId::ClassId(it) => crate::ModuleDef::Class(it.into()),
        }
    }
}
