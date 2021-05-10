use crate::db::HirDatabase;
use hir_def::id::{ClassId, TypeCtorId};
use hir_def::name::Name;
use std::sync::Arc;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Ty(salsa::InternId);

pub type List<T> = Arc<[T]>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TyKind {
    Error,

    Unknown(Unknown),
    Skolem(Skolem, Ty),
    TypeVar(TypeVar),

    Figure(i128),
    Symbol(String),
    Row(List<Field>, Option<Ty>),

    Ctor(TypeCtorId),
    Tuple(List<Ty>),

    App(Ty, Ty),
    Ctnt(Constraint, Ty),
    ForAll(Ty, Ty),
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
pub struct Unknown(u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeVar(DebruijnIndex);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct DebruijnIndex(u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Skolem(UniverseIndex);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct UniverseIndex(u32);

impl Ty {
    pub fn lookup(self, db: &dyn HirDatabase) -> TyKind {
        db.lookup_intern_ty(self)
    }

    pub fn everywhere<F>(self, db: &dyn HirDatabase, f: &mut F) -> Ty
    where
        F: FnMut(Ty) -> Ty,
    {
        match self.lookup(db) {
            | TyKind::Skolem(sk, k) => {
                let k = k.everywhere(db, f);

                f(sk.to_ty(db, k))
            },
            | TyKind::Row(fields, tail) => {
                let fields = fields
                    .iter()
                    .map(|field| Field {
                        name: field.name.clone(),
                        ty: field.ty.everywhere(db, f),
                    })
                    .collect();

                let tail = tail.map(|t| t.everywhere(db, f));

                f(TyKind::Row(fields, tail).intern(db))
            },
            | TyKind::Tuple(tys) => {
                let tys = tys.iter().map(|t| t.everywhere(db, f)).collect();

                f(TyKind::Tuple(tys).intern(db))
            },
            | TyKind::App(a, b) => {
                let a = a.everywhere(db, f);
                let b = b.everywhere(db, f);

                f(TyKind::App(a, b).intern(db))
            },
            | TyKind::Ctnt(ctnt, ty) => {
                let ctnt = Constraint {
                    class: ctnt.class,
                    types: ctnt.types.iter().map(|t| t.everywhere(db, f)).collect(),
                };

                let ty = ty.everywhere(db, f);

                f(TyKind::Ctnt(ctnt, ty).intern(db))
            },
            | TyKind::ForAll(k, t) => {
                let k = k.everywhere(db, f);
                let t = t.everywhere(db, f);

                f(TyKind::ForAll(k, t).intern(db))
            },
            | _ => f(self),
        }
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

    pub const fn raw(self) -> u32 {
        self.0
    }

    pub fn to_ty(self, db: &dyn HirDatabase) -> Ty {
        TyKind::Unknown(self).intern(db)
    }
}

impl TypeVar {
    pub const fn new(debruijn: DebruijnIndex) -> Self {
        Self(debruijn)
    }

    pub fn to_ty(self, db: &dyn HirDatabase) -> Ty {
        TyKind::TypeVar(self).intern(db)
    }

    pub fn bound_within(self, debruijn: DebruijnIndex) -> bool {
        self.0.within(debruijn)
    }

    pub fn debruijn(self) -> DebruijnIndex {
        self.0
    }

    #[must_use]
    pub fn shifted_in(self) -> Self {
        Self(self.0.shifted_in())
    }

    #[must_use]
    pub fn shifted_in_from(self, debruijn: DebruijnIndex) -> Self {
        Self(self.0.shifted_in_from(debruijn))
    }

    #[must_use]
    pub fn shifted_out(self) -> Option<Self> {
        self.0.shifted_out().map(Self)
    }

    #[must_use]
    pub fn shifted_out_to(self, debruijn: DebruijnIndex) -> Option<Self> {
        self.0.shifted_out_to(debruijn).map(Self)
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

impl Skolem {
    pub const fn new(universe: UniverseIndex) -> Self {
        Self(universe)
    }

    pub fn to_ty(self, db: &dyn HirDatabase, kind: Ty) -> Ty {
        TyKind::Skolem(self, kind).intern(db)
    }

    pub fn universe(self) -> UniverseIndex {
        self.0
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

    pub fn prev(self) -> Self {
        Self(self.0 - 1)
    }

    pub fn index(self) -> u32 {
        self.0
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
