use crate::id::ModuleDefId;
use crate::item_scope::ItemInNs;
use crate::visibility::Visibility;

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct PerNs<T = (ModuleDefId, Visibility)> {
    pub types: Option<T>,
    pub values: Option<T>,
    pub modules: Option<T>,
}

impl<T> PerNs<T> {
    pub const fn none() -> Self {
        PerNs {
            types: None,
            values: None,
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
}

impl<T> PerNs<(T, Visibility)> {
    pub fn values(id: T, v: Visibility) -> Self {
        PerNs {
            values: Some((id, v)),
            ..Self::none()
        }
    }

    pub fn types(id: T, v: Visibility) -> Self {
        PerNs {
            types: Some((id, v)),
            ..Self::none()
        }
    }

    pub fn modules(id: T, v: Visibility) -> Self {
        PerNs {
            modules: Some((id, v)),
            ..Self::none()
        }
    }

    pub fn both(id: T, v: Visibility) -> Self
    where
        T: Clone,
    {
        PerNs {
            values: Some((id.clone(), v)),
            types: Some((id, v)),
            modules: None,
        }
    }

    pub fn filter_vis(self, mut f: impl FnMut(Visibility) -> bool) -> Self {
        PerNs {
            types: self.types.filter(|&(_, v)| f(v)),
            values: self.values.filter(|&(_, v)| f(v)),
            modules: self.modules.filter(|&(_, v)| f(v)),
        }
    }

    pub fn with_vis(self, v: Visibility) -> Self {
        Self {
            values: self.values.map(|it| (it.0, v)),
            types: self.types.map(|it| (it.0, v)),
            modules: self.modules.map(|it| (it.0, v)),
        }
    }
}

impl PerNs<(ModuleDefId, Visibility)> {
    pub fn iter_items(self) -> impl Iterator<Item = ItemInNs> {
        self.types
            .map(|it| ItemInNs::Types(it.0))
            .into_iter()
            .chain(self.values.map(|it| ItemInNs::Values(it.0)))
            .chain(self.modules.map(|it| ItemInNs::Modules(it.0)))
    }
}

impl From<ModuleDefId> for PerNs<(ModuleDefId, Visibility)> {
    fn from(def: ModuleDefId) -> Self {
        match def {
            | ModuleDefId::ModuleId(_) => PerNs::modules(def, Visibility::Public),
            | ModuleDefId::FixityId(_) => PerNs::values(def, Visibility::Public),
            | ModuleDefId::FuncId(_) => PerNs::values(def, Visibility::Public),
            | ModuleDefId::StaticId(_) => PerNs::values(def, Visibility::Public),
            | ModuleDefId::ConstId(_) => PerNs::values(def, Visibility::Public),
            | ModuleDefId::TypeAliasId(_) => PerNs::types(def, Visibility::Public),
            | ModuleDefId::TypeCtorId(_) => PerNs::types(def, Visibility::Public),
            | ModuleDefId::CtorId(_) => PerNs::values(def, Visibility::Public),
            | ModuleDefId::ClassId(_) => PerNs::types(def, Visibility::Public),
        }
    }
}

impl<T> Iterator for PerNs<(T, Visibility)> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some((id, _)) = self.types.take() {
            Some(id)
        } else if let Some((id, _)) = self.values.take() {
            Some(id)
        } else {
            self.modules.take().map(|it| it.0)
        }
    }
}
