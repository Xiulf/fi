use arena::Idx;
use base_db::libs::LibId;

use crate::item_tree::{self, ItemTreeId};
use crate::name::Name;

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

ra_ap_stdx::impl_from!(LibId, ModuleId for ModuleParentId);
ra_ap_stdx::impl_from!(ModuleId, TraitId, ImplId for ContainerId);
ra_ap_stdx::impl_from!(ModuleId, FixityId, ValueId, TypeAliasId, TypeCtorId, CtorId, TraitId, ImplId for ItemId);

impl ra_ap_stdx::hash::NoHashHashable for ModuleId {
}
