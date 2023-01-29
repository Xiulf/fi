use arena::Idx;
use base_db::libs::LibId;

use crate::item_tree::{self, ItemTreeId};
use crate::name::Name;
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
    TraitId(TraitId),
    ImplId(ImplId),
}

impl ItemId {
    pub fn name(self, db: &dyn Db) -> Option<Name> {
        match self {
            | Self::ModuleId(id) => Some(id.name(db)),
            | Self::FixityId(id) => {
                let it = id.it(db);
                let tree = item_tree::query(db, it.file);
                Some(tree[it.value].name)
            },
            | Self::ValueId(id) => {
                let it = id.it(db);
                let tree = item_tree::query(db, it.file);
                Some(tree[it.value].name)
            },
            | Self::TypeAliasId(id) => {
                let it = id.it(db);
                let tree = item_tree::query(db, it.file);
                Some(tree[it.value].name)
            },
            | Self::TypeCtorId(id) => {
                let it = id.it(db);
                let tree = item_tree::query(db, it.file);
                Some(tree[it.value].name)
            },
            | Self::CtorId(id) => {
                let it = id.type_ctor(db).it(db);
                let tree = item_tree::query(db, it.file);
                Some(tree[id.local_id(db)].name)
            },
            | Self::TraitId(id) => {
                let it = id.it(db);
                let tree = item_tree::query(db, it.file);
                Some(tree[it.value].name)
            },
            | Self::ImplId(_) => None,
        }
    }
}

ra_ap_stdx::impl_from!(LibId, ModuleId for ModuleParentId);
ra_ap_stdx::impl_from!(ModuleId, TraitId, ImplId for ContainerId);
ra_ap_stdx::impl_from!(ModuleId, FixityId, ValueId, TypeAliasId, TypeCtorId, CtorId, TraitId, ImplId for ItemId);

impl ra_ap_stdx::hash::NoHashHashable for ModuleId {
}
