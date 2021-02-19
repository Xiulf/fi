use crate::id::ModuleDefId;
use crate::item_scope::ItemInNs;

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct PerNs {
    pub types: Option<ModuleDefId>,
    pub values: Option<ModuleDefId>,
    pub modules: Option<ModuleDefId>,
}

impl PerNs {
    pub const fn none() -> Self {
        PerNs {
            types: None,
            values: None,
            modules: None,
        }
    }

    pub fn values(id: ModuleDefId) -> Self {
        PerNs {
            values: Some(id),
            ..Self::none()
        }
    }

    pub fn types(id: ModuleDefId) -> Self {
        PerNs {
            types: Some(id),
            ..Self::none()
        }
    }

    pub fn modules(id: ModuleDefId) -> Self {
        PerNs {
            modules: Some(id),
            ..Self::none()
        }
    }

    pub fn both(id: ModuleDefId) -> Self {
        PerNs {
            values: Some(id),
            types: Some(id),
            modules: None,
        }
    }

    pub fn is_none(&self) -> bool {
        self.types.is_none() && self.values.is_none() && self.modules.is_none()
    }

    pub fn or(self, other: Self) -> Self {
        PerNs {
            types: self.types.or(other.types),
            values: self.values.or(other.values),
            modules: self.modules.or(other.modules),
        }
    }

    pub fn iter_items(self) -> impl Iterator<Item = ItemInNs> {
        self.types
            .map(ItemInNs::Types)
            .into_iter()
            .chain(self.values.map(ItemInNs::Values))
            .chain(self.modules.map(ItemInNs::Modules))
    }
}

impl From<ModuleDefId> for PerNs {
    fn from(def: ModuleDefId) -> Self {
        match def {
            | ModuleDefId::ModuleId(_) => PerNs::modules(def),
            | ModuleDefId::FixityId(_) => PerNs::values(def),
            | ModuleDefId::FuncId(_) => PerNs::values(def),
            | ModuleDefId::StaticId(_) => PerNs::values(def),
            | ModuleDefId::ConstId(_) => PerNs::values(def),
            | ModuleDefId::TypeId(_) => PerNs::types(def),
            | ModuleDefId::CtorId(_) => PerNs::values(def),
            | ModuleDefId::ClassId(_) => PerNs::types(def),
        }
    }
}
