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
        Self {
            types: None,
            values: None,
            modules: None,
        }
    }

    pub fn values(t: T) -> Self {
        Self {
            values: Some(t),
            ..Self::none()
        }
    }

    pub fn types(t: T) -> Self {
        Self {
            types: Some(t),
            ..Self::none()
        }
    }

    pub fn modules(t: T) -> Self {
        Self {
            modules: Some(t),
            ..Self::none()
        }
    }

    pub fn both(t: T) -> Self
    where
        T: Clone,
    {
        Self {
            values: Some(t.clone()),
            types: Some(t),
            modules: None,
        }
    }

    pub fn is_none(&self) -> bool {
        self.types.is_none() && self.values.is_none() && self.modules.is_none()
    }

    pub fn or(self, other: Self) -> Self {
        Self {
            types: self.types.or(other.types),
            values: self.values.or(other.values),
            modules: self.modules.or(other.modules),
        }
    }

    pub fn map<U>(self, mut f: impl FnMut(T) -> U) -> PerNs<U> {
        PerNs {
            types: self.types.map(&mut f),
            values: self.values.map(&mut f),
            modules: self.modules.map(f),
        }
    }

    pub fn and_then<U>(self, mut f: impl FnMut(T) -> Option<U>) -> PerNs<U> {
        PerNs {
            types: self.types.and_then(&mut f),
            values: self.values.and_then(&mut f),
            modules: self.modules.and_then(f),
        }
    }

    pub fn as_ref(&self) -> PerNs<&T> {
        PerNs {
            types: self.types.as_ref(),
            values: self.values.as_ref(),
            modules: self.modules.as_ref(),
        }
    }
}

impl<T> PerNs<(T, Visibility)> {
    pub fn visibility(self) -> Option<Visibility> {
        let v = self.map(|(_, v)| v);

        v.types.or(v.values).or(v.modules)
    }

    pub fn filter_vis(self, mut f: impl FnMut(Visibility) -> bool) -> Self {
        Self {
            types: self.types.filter(|&(_, v)| f(v)),
            values: self.values.filter(|&(_, v)| f(v)),
            modules: self.modules.filter(|&(_, v)| f(v)),
        }
    }

    pub fn with_vis(self, v: Visibility) -> Self {
        self.map(|(t, _)| (t, v))
    }

    pub fn with_lower_vis(self, v: Visibility) -> Self {
        self.map(|(t, v2)| match v2 {
            | Visibility::Public => (t, v),
            | Visibility::Module(_) => (t, v2),
        })
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

impl From<ModuleDefId> for PerNs<ModuleDefId> {
    fn from(def: ModuleDefId) -> Self {
        match def {
            | ModuleDefId::ModuleId(_) => PerNs::modules(def),
            | ModuleDefId::FixityId(_) => PerNs::values(def),
            | ModuleDefId::FuncId(_) => PerNs::values(def),
            | ModuleDefId::StaticId(_) => PerNs::values(def),
            | ModuleDefId::ConstId(_) => PerNs::values(def),
            | ModuleDefId::TypeAliasId(_) => PerNs::types(def),
            | ModuleDefId::TypeCtorId(_) => PerNs::types(def),
            | ModuleDefId::CtorId(_) => PerNs::values(def),
            | ModuleDefId::ClassId(_) => PerNs::types(def),
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
