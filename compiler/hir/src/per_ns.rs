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

    pub fn filter_vis(self, f: impl Fn(&ModuleDefId) -> bool + Clone) -> Self {
        PerNs {
            types: self.types.filter(f.clone()),
            values: self.values.filter(f.clone()),
            modules: self.modules.filter(f),
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
            | ModuleDefId::ForeignId(id) => PerNs::values(def),
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

impl Iterator for PerNs {
    type Item = ModuleDefId;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(id) = self.types.take() {
            Some(id)
        } else if let Some(id) = self.values.take() {
            Some(id)
        } else {
            self.modules.take()
        }
    }
}
