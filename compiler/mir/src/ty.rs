use crate::db::MirDatabase;
use crate::layout::Primitive;
use hir::ty::{Ty, TyKind, TypeVar};
use std::fmt;
use std::sync::Arc;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Type {
    pub repr: ReprOptions,
    pub kind: TypeKind,
}

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash)]
pub struct ReprOptions {
    pub uninhabited: bool,
    pub scalar: Option<Primitive>,
    pub valid_range_start: Option<u128>,
    pub valid_range_end: Option<u128>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeKind {
    Void,
    Var(TypeVar),
    Ptr(Arc<Type>),
    Array(Arc<Type>, usize),
    And(Arc<[Arc<Type>]>),
    Or(Arc<[Arc<Type>]>, bool),
    Func(Signature),
    Clos(Signature),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Signature {
    pub params: Arc<[Arc<Type>]>,
    pub ret: Arc<Type>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeVarKind {
    Type,
    Figure,
    Symbol,
}

impl Type {
    pub fn mir_type_query(db: &dyn MirDatabase, mut ty: Ty) -> Arc<Type> {
        let mut args = Vec::new();

        while let TyKind::App(a, b) = ty.lookup(db.upcast()) {
            args.push(b);
            ty = a;
        }

        args.reverse();

        let kind = match ty.lookup(db.upcast()) {
            | TyKind::Error
            | TyKind::Unknown(_)
            | TyKind::Skolem(_, _)
            | TyKind::Row(_, _)
            | TyKind::Figure(_)
            | TyKind::Symbol(_)
            | TyKind::App(_, _) => unreachable!(),
            | TyKind::ForAll(_, ty) => return db.mir_type(ty),
            | TyKind::Ctnt(_, ty) => return db.mir_type(ty),
            | TyKind::TypeVar(tv) => TypeKind::Var(tv),
            | TyKind::Tuple(ts) => {
                let ts = ts.iter().map(|&t| db.mir_type(t)).collect();

                TypeKind::And(ts)
            },
            | _ => TypeKind::Void,
        };

        Arc::new(Type {
            repr: ReprOptions::default(),
            kind,
        })
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.kind.fmt(f)
    }
}

impl fmt::Display for TypeKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            | TypeKind::Void => write!(f, "void"),
            | TypeKind::Var(var) => var.fmt(f),
            | TypeKind::Ptr(to) => write!(f, "*{}", to),
            | TypeKind::Array(of, len) => write!(f, "[{}]{}", len, of),
            | TypeKind::And(tys) => {
                write!(f, "(")?;

                for (i, ty) in tys.iter().enumerate() {
                    if i != 0 {
                        write!(f, ", ")?;
                    }

                    ty.fmt(f)?;
                }

                write!(f, ")")
            },
            | TypeKind::Or(tys, tagged) => {
                write!(f, "(")?;

                for (i, ty) in tys.iter().enumerate() {
                    if i != 0 {
                        if *tagged {
                            write!(f, " / ")?;
                        } else {
                            write!(f, " | ")?;
                        }
                    }

                    ty.fmt(f)?;
                }

                write!(f, ")")
            },
            | TypeKind::Func(sig) => sig.fmt(f),
            | TypeKind::Clos(sig) => write!(f, "closure {}", sig),
        }
    }
}

impl fmt::Display for Signature {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "(")?;

        for (i, ty) in self.params.iter().enumerate() {
            if i != 0 {
                write!(f, ", ")?;
            }

            ty.fmt(f)?;
        }

        write!(f, ") -> {}", self.ret)
    }
}
