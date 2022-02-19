use crate::db::HirDatabase;
use hir_def::id::{ClassId, TypeCtorId};
use hir_def::name::Name;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Ty(salsa::InternId);

pub type List<T> = Box<[T]>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TyKind {
    Error,

    Figure(i128),
    Symbol(Box<str>),
    Row(List<Field>, Option<Ty>),

    Ctor(TypeCtorId),
    App(Ty, List<Ty>),

    Tuple(List<Ty>),
    Func(List<Ty>, Ty),

    Ctnt(Constraint, Ty),
    ForAll(List<Ty>, Ty),
    TypeVar(TypeVar),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Field {
    pub name: Name,
    pub ty: Ty,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Constraint {
    pub class: ClassId,
    pub types: List<Ty>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeVar(u32, DebruijnIndex);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct DebruijnIndex(u32);

impl Ty {
    pub fn lookup(self, db: &dyn HirDatabase) -> TyKind {
        db.lookup_intern_ty(self)
    }

    pub fn match_ctor(self, db: &dyn HirDatabase, id: TypeCtorId) -> Option<List<Ty>> {
        if let TyKind::App(ctor, args) = self.lookup(db) {
            if ctor.lookup(db) == TyKind::Ctor(id) {
                Some(args)
            } else {
                None
            }
        } else {
            None
        }
    }
}

impl TyKind {
    pub fn intern(mut self, db: &dyn HirDatabase) -> Ty {
        if let TyKind::Row(fields, tail) = self {
            let mut fields = fields.to_vec();

            fields.sort_by_key(|f| f.name.clone());

            self = TyKind::Row(fields.into(), tail);
        }

        db.intern_ty(self)
    }
}

impl Constraint {
    pub fn new(class: ClassId, tys: impl IntoIterator<Item = Ty>) -> Self {
        Constraint {
            class,
            types: tys.into_iter().collect(),
        }
    }
}

impl TypeVar {
    pub const fn new(idx: u32, debruijn: DebruijnIndex) -> Self {
        Self(idx, debruijn)
    }

    pub fn to_ty(self, db: &dyn HirDatabase) -> Ty {
        TyKind::TypeVar(self).intern(db)
    }

    pub fn bound_within(self, debruijn: DebruijnIndex) -> bool {
        self.1.within(debruijn)
    }

    pub fn idx(self) -> u32 {
        self.0
    }

    pub fn debruijn(self) -> DebruijnIndex {
        self.1
    }

    #[must_use]
    pub fn shifted_in(self) -> Self {
        Self(self.0, self.1.shifted_in())
    }

    #[must_use]
    pub fn shifted_in_from(self, debruijn: DebruijnIndex) -> Self {
        Self(self.0, self.1.shifted_in_from(debruijn))
    }

    #[must_use]
    pub fn shifted_out(self) -> Option<Self> {
        Some(Self(self.0, self.1.shifted_out()?))
    }

    #[must_use]
    pub fn shifted_out_to(self, debruijn: DebruijnIndex) -> Option<Self> {
        Some(Self(self.0, self.1.shifted_out_to(debruijn)?))
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

impl salsa::InternKey for Ty {
    fn from_intern_id(v: salsa::InternId) -> Self {
        Self(v)
    }

    fn as_intern_id(&self) -> salsa::InternId {
        self.0
    }
}
