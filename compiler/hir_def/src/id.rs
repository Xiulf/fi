use arena::Idx;
use base_db::libs::LibId;

use crate::item_tree::{self, ItemTreeId};
use crate::name::Name;
use crate::pat::PatId;
use crate::type_ref::LocalTypeVarId;
use crate::Db;

#[salsa::interned]
pub struct ModuleId {
    pub parent: ModuleParentId,
    pub name: Name,
}

#[salsa::interned]
pub struct FixityId {
    pub module: ModuleId,
    pub it: ItemTreeId<item_tree::Fixity>,
}

#[salsa::interned]
pub struct ValueId {
    pub container: ContainerId,
    pub it: ItemTreeId<item_tree::Value>,
}

#[salsa::interned]
pub struct TypeAliasId {
    pub module: ModuleId,
    pub it: ItemTreeId<item_tree::TypeAlias>,
}

#[salsa::interned]
pub struct TypeCtorId {
    pub module: ModuleId,
    pub it: ItemTreeId<item_tree::TypeCtor>,
}

#[salsa::interned]
pub struct CtorId {
    pub type_ctor: TypeCtorId,
    pub local_id: LocalCtorId,
}

pub type LocalCtorId = Idx<item_tree::Ctor>;

#[salsa::interned]
pub struct FieldId {
    pub ctor: CtorId,
    pub local_id: LocalFieldId,
}

pub type LocalFieldId = Idx<item_tree::Field>;

#[salsa::interned]
pub struct TraitId {
    pub module: ModuleId,
    pub it: ItemTreeId<item_tree::Trait>,
}

#[salsa::interned]
pub struct ImplId {
    pub module: ModuleId,
    pub it: ItemTreeId<item_tree::Impl>,
}

#[salsa::interned]
pub struct TypeVarId {
    pub owner: TypedItemId,
    pub local_id: LocalTypeVarId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ModuleParentId {
    LibId(LibId),
    ModuleId(ModuleId),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ContainerId {
    ModuleId(ModuleId),
    TraitId(TraitId),
    ImplId(ImplId),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ItemId {
    ModuleId(ModuleId),
    FixityId(FixityId),
    ValueId(ValueId),
    TypeAliasId(TypeAliasId),
    TypeCtorId(TypeCtorId),
    CtorId(CtorId),
    FieldId(FieldId),
    TraitId(TraitId),
    ImplId(ImplId),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TypedItemId {
    ValueId(ValueId),
    TypeAliasId(TypeAliasId),
    TypeCtorId(TypeCtorId),
    CtorId(CtorId),
    FieldId(FieldId),
    TraitId(TraitId),
    ImplId(ImplId),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ValueDefId {
    FixityId(FixityId),
    ValueId(ValueId),
    CtorId(CtorId),
    FieldId(FieldId),
    PatId(PatId),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TypeDefId {
    FixityId(FixityId),
    TypeAliasId(TypeAliasId),
    TypeCtorId(TypeCtorId),
    TraitId(TraitId),
    TypeVarId(TypeVarId),
}

#[salsa::interned]
pub struct ITypedItemId {
    pub as_item_id: TypedItemId,
}

pub trait HasModule: Copy {
    fn module(self, db: &dyn Db) -> ModuleId;
}

impl ModuleId {
    pub fn lib(self, db: &dyn Db) -> LibId {
        match self.parent(db) {
            | ModuleParentId::LibId(id) => id,
            | ModuleParentId::ModuleId(id) => id.lib(db),
        }
    }
}

impl HasModule for ContainerId {
    fn module(self, db: &dyn Db) -> ModuleId {
        match self {
            | Self::ModuleId(id) => id,
            | Self::TraitId(id) => id.module(db),
            | Self::ImplId(id) => id.module(db),
        }
    }
}

impl HasModule for TypedItemId {
    fn module(self, db: &dyn Db) -> ModuleId {
        match self {
            | Self::ValueId(id) => id.container(db).module(db),
            | Self::CtorId(id) => id.type_ctor(db).module(db),
            | Self::FieldId(id) => id.ctor(db).type_ctor(db).module(db),
            | Self::TypeAliasId(id) => id.module(db),
            | Self::TypeCtorId(id) => id.module(db),
            | Self::TraitId(id) => id.module(db),
            | Self::ImplId(id) => id.module(db),
        }
    }
}

impl TypedItemId {
    pub fn as_value_id(self) -> Option<ValueId> {
        match self {
            | Self::ValueId(id) => Some(id),
            | _ => None,
        }
    }
}

ra_ap_stdx::impl_from!(LibId, ModuleId for ModuleParentId);
ra_ap_stdx::impl_from!(ModuleId, TraitId, ImplId for ContainerId);
ra_ap_stdx::impl_from!(ModuleId, FixityId, ValueId, CtorId, FieldId, TypeAliasId, TypeCtorId, TraitId, ImplId for ItemId);
ra_ap_stdx::impl_from!(ValueId, CtorId, FieldId, TypeAliasId, TypeCtorId, TraitId, ImplId for TypedItemId);
ra_ap_stdx::impl_from!(FixityId, ValueId, CtorId, FieldId, PatId for ValueDefId);
ra_ap_stdx::impl_from!(FixityId, TypeAliasId, TypeCtorId, TraitId, TypeVarId for TypeDefId);

impl ra_ap_stdx::hash::NoHashHashable for ModuleId {
}
