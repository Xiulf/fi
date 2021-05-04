use crate::db::HirDatabase;
use hir_def::id::TypeCtorId;
use hir_def::name::Name;
use std::sync::Arc;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Ty(salsa::InternId);

pub type List<T> = Arc<[T]>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TyKind {
    Error,

    Unknown(Unknown),
    Placeholder(Placeholder),
    TypeVar(TypeVar),

    Figure(i128),
    Symbol(String),
    Row(List<Field>, Option<Ty>),

    Ctor(TypeCtorId),
    Tuple(List<Ty>),

    App(Ty, Ty),
    ForAll(),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Field {
    pub name: Name,
    pub ty: Ty,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Unknown(u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeVar(DebruijnIndex, usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct DebruijnIndex(u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Placeholder(UniverseIndex, usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct UniverseIndex(u32);

impl Ty {
    pub fn lookup(self, db: &dyn HirDatabase) -> TyKind {
        db.lookup_intern_ty(self)
    }
}

impl TyKind {
    pub fn intern(self, db: &dyn HirDatabase) -> Ty {
        db.intern_ty(self)
    }
}

impl Unknown {
    pub const fn from_raw(id: u32) -> Self {
        Unknown(id)
    }
}

impl TypeVar {
    pub const fn new(debruijn: DebruijnIndex, index: usize) -> Self {
        Self(debruijn, index)
    }

    pub fn to_ty(self, db: &dyn HirDatabase) -> Ty {
        TyKind::TypeVar(self).intern(db)
    }

    pub fn bound_within(self, debruijn: DebruijnIndex) -> bool {
        self.0.within(debruijn)
    }

    #[must_use]
    pub fn shifted_in(self) -> Self {
        Self(self.0.shifted_in(), self.1)
    }

    #[must_use]
    pub fn shifted_in_from(self, debruijn: DebruijnIndex) -> Self {
        Self(self.0.shifted_in_from(debruijn), self.1)
    }

    #[must_use]
    pub fn shifted_out(self) -> Option<Self> {
        self.0.shifted_out().map(|d| Self(d, self.1))
    }

    #[must_use]
    pub fn shifted_out_to(self, debruijn: DebruijnIndex) -> Option<Self> {
        self.0.shifted_out_to(debruijn).map(|d| Self(d, self.1))
    }

    pub fn index_if_inner(self) -> Option<usize> {
        self.index_if_bound_at(DebruijnIndex::INNER)
    }

    pub fn index_if_bound_at(self, debruijn: DebruijnIndex) -> Option<usize> {
        if self.0 == debruijn {
            Some(self.1)
        } else {
            None
        }
    }
}

impl DebruijnIndex {
    pub const INNER: Self = Self(0);
    pub const ONE: Self = Self(1);

    pub const fn new(depth: u32) -> Self {
        Self(depth)
    }

    pub const fn depth(self) -> u32 {
        self.0
    }

    pub fn within(self, other: Self) -> bool {
        self.0 < other.0
    }

    #[must_use]
    pub fn shifted_in(self) -> Self {
        self.shifted_in_from(Self::ONE)
    }

    pub fn shift_in(&mut self) {
        *self = self.shifted_in();
    }

    #[must_use]
    pub fn shifted_in_from(self, other: Self) -> Self {
        Self(self.0 + other.0)
    }

    #[must_use]
    pub fn shifted_out(self) -> Option<Self> {
        self.shifted_out_to(Self::ONE)
    }

    pub fn shift_out(&mut self) {
        *self = self.shifted_out().unwrap();
    }

    #[must_use]
    pub fn shifted_out_to(self, other: Self) -> Option<Self> {
        if self.within(other) {
            None
        } else {
            Some(Self(self.0 - other.0))
        }
    }
}

impl Placeholder {
    pub const fn new(universe: UniverseIndex, index: usize) -> Self {
        Self(universe, index)
    }

    pub fn to_ty(self, db: &dyn HirDatabase) -> Ty {
        TyKind::Placeholder(self).intern(db)
    }
}

impl UniverseIndex {
    pub const ROOT: Self = Self(0);

    pub fn can_see(self, other: Self) -> bool {
        self.0 >= other.0
    }

    pub fn next(self) -> Self {
        Self(self.0 + 1)
    }
}

impl salsa::InternKey for Ty {
    fn from_intern_id(v: salsa::InternId) -> Self {
        Self(v)
    }

    fn as_intern_id(&self) -> salsa::InternId {
        self.0
    }
}
