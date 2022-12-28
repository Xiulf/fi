use hir_def::id::{ClassId, TypeAliasId, TypeCtorId, TypeVarId};
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
pub struct TypeVar(u32, TypeVarScopeId, Option<TypeVarId>);

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

    pub fn everything<F>(self, db: &dyn HirDatabase, f: &mut F)
    where
        F: FnMut(Ty),
    {
        match self.lookup(db) {
            | TyKind::Row(ref fields, tail) => {
                for field in fields.iter() {
                    field.ty.everything(db, f);
                }

                if let Some(tail) = tail {
                    tail.everything(db, f);
                }
            },
            | TyKind::App(base, ref args) => {
                base.everything(db, f);

                for ty in args.iter() {
                    ty.everything(db, f);
                }
            },
            | TyKind::Where(ref where_, ty) => {
                for ctnt in where_.constraints.iter() {
                    for ty in ctnt.types.iter() {
                        ty.everything(db, f);
                    }
                }

                ty.everything(db, f);
            },
            | TyKind::ForAll(ref k, t, _) => {
                for ty in k.iter() {
                    ty.everything(db, f);
                }

                t.everything(db, f);
            },
            | _ => {},
        }

        f(self)
    }

    pub fn replace_var(self, db: &dyn HirDatabase, var: TypeVarId, with: Ty) -> Ty {
        match self.lookup(db) {
            | TyKind::TypeVar(tv) if tv.2 == Some(var) => with,
            | TyKind::Row(fields, rest) => {
                let fields = fields
                    .iter()
                    .map(|f| Field {
                        name: f.name.clone(),
                        ty: f.ty.replace_var(db, var, with),
                    })
                    .collect();

                let rest = rest.map(|r| r.replace_var(db, var, with));

                TyKind::Row(fields, rest).intern(db)
            },
            | TyKind::App(base, args) => {
                let base = base.replace_var(db, var, with);
                let args = args.iter().map(|a| a.replace_var(db, var, with)).collect();

                TyKind::App(base, args).intern(db)
            },
            | TyKind::Where(clause, ty) => {
                let clause = WhereClause {
                    constraints: clause
                        .constraints
                        .iter()
                        .map(|c| Constraint {
                            class: c.class,
                            types: c.types.iter().map(|t| t.replace_var(db, var, with)).collect(),
                        })
                        .collect(),
                };

                let ty = ty.replace_var(db, var, with);

                TyKind::Where(clause, ty).intern(db)
            },
            | TyKind::ForAll(params, ty, scope) => {
                let ty = ty.replace_var(db, var, with);
                let params = params.iter().map(|p| p.replace_var(db, var, with)).collect();

                TyKind::ForAll(params, ty, scope).intern(db)
            },
            | _ => self,
        }
    }

    pub fn replace_local_vars(self, db: &dyn HirDatabase, types: &[Ty]) -> Ty {
        match self.lookup(db) {
            | TyKind::TypeVar(tv) => types[tv.idx() as usize],
            | TyKind::Row(fields, rest) => {
                let fields = fields
                    .iter()
                    .map(|f| Field {
                        name: f.name.clone(),
                        ty: f.ty.replace_local_vars(db, types),
                    })
                    .collect();

                let rest = rest.map(|r| r.replace_local_vars(db, types));

                TyKind::Row(fields, rest).intern(db)
            },
            | TyKind::App(base, args) => {
                let base = base.replace_local_vars(db, types);
                let args = args.iter().map(|a| a.replace_local_vars(db, types)).collect();

                TyKind::App(base, args).intern(db)
            },
            | TyKind::Where(clause, ty) => {
                let clause = WhereClause {
                    constraints: clause
                        .constraints
                        .iter()
                        .map(|c| Constraint {
                            class: c.class,
                            types: c.types.iter().map(|t| t.replace_local_vars(db, types)).collect(),
                        })
                        .collect(),
                };

                let ty = ty.replace_local_vars(db, types);

                TyKind::Where(clause, ty).intern(db)
            },
            | TyKind::ForAll(params, ty, scope) => {
                let ty = ty.replace_local_vars(db, types);
                let params = params.iter().map(|p| p.replace_local_vars(db, types)).collect();

                TyKind::ForAll(params, ty, scope).intern(db)
            },
            | _ => self,
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
    pub const fn new(idx: u32, scope: TypeVarScopeId, src: Option<TypeVarId>) -> Self {
        Self(idx, scope, src)
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

    pub fn src(self) -> Option<TypeVarId> {
        self.2
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
