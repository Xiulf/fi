#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct PerNs<T> {
    pub types: Option<T>,
    pub values: Option<T>,
    pub modules: Option<T>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Namespace {
    Types,
    Values,
    Modules,
}

impl<T> PerNs<T> {
    pub fn new(types: Option<T>, values: Option<T>, modules: Option<T>) -> Self {
        Self { types, values, modules }
    }

    pub fn none() -> Self {
        Self::new(None, None, None)
    }

    pub fn with_types(self, types: T) -> Self {
        Self {
            types: Some(types),
            ..self
        }
    }

    pub fn with_values(self, values: T) -> Self {
        Self {
            values: Some(values),
            ..self
        }
    }

    pub fn with_modules(self, modules: T) -> Self {
        Self {
            modules: Some(modules),
            ..self
        }
    }

    pub fn is_none(&self) -> bool {
        self.types.is_none() && self.values.is_none() && self.modules.is_none()
    }

    pub fn to_option(self) -> Option<Self> {
        if self.is_none() {
            None
        } else {
            Some(self)
        }
    }

    pub fn or(self, other: Self) -> Self {
        Self {
            types: self.types.or(other.types),
            values: self.values.or(other.values),
            modules: self.modules.or(other.modules),
        }
    }
}
