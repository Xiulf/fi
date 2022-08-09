use hir_def::id::{ClassId, TypeAliasId, TypeCtorId};
use hir_def::name::Name;

use crate::db::HirDatabase;
use crate::info::TySource;
pub use crate::info::TypeVarScopeId;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TyAndSrc<T> {
    pub ty: T,
    pub src: TySource,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Ty(salsa::InternId);

pub type List<T> = Box<[T]>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TyKind {
    Error(Reason),

    Figure(i128),
    Symbol(Box<str>),
    Row(List<Field>, Option<Ty>),

    Ctor(TypeCtorId),
    Alias(TypeAliasId),
    App(Ty, List<Ty>),

    Tuple(List<Ty>),
    Func(List<Ty>, Ty),

    Where(WhereClause<Constraint>, Ty),
    ForAll(List<Ty>, Ty, TypeVarScopeId),
    TypeVar(TypeVar),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Reason {
    Error,
    Unknown,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Field {
    pub name: Name,
    pub ty: Ty,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct WhereClause<C> {
    pub constraints: List<C>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Constraint {
    pub class: ClassId,
    pub types: List<Ty>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeVar(u32, TypeVarScopeId);

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
    pub const fn new(idx: u32, scope: TypeVarScopeId) -> Self {
        Self(idx, scope)
    }

    pub fn to_ty(self, db: &dyn HirDatabase) -> Ty {
        TyKind::TypeVar(self).intern(db)
    }

    pub fn idx(self) -> u32 {
        self.0
    }

    pub fn scope(self) -> TypeVarScopeId {
        self.1
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
