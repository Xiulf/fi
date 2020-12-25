use crate::error::Result;
use hir::ir::{DefId, HirId, Span, Symbol};
use std::collections::HashMap;
use std::iter::FromIterator;
use std::sync::Arc;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Ty(Arc<(Type, Span)>);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct List<T>(Arc<[T]>);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Error,
    Unknown(Unknown),
    Var(TypeVar),
    Int(u128),
    String(String),
    Ctor(DefId),
    Row(List<Field>, Option<Ty>),
    App(Ty, List<Ty>),
    ForAll(List<TypeVar>, Ty, Option<SkolemScope>),
    Ctnt(Ctnt, Ty),
    Skolem(TypeVar, Skolem, SkolemScope),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Unknown(pub u64);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeVar(pub HirId);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Skolem(pub u64);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SkolemScope(pub u64);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Field {
    pub span: Span,
    pub name: Symbol,
    pub ty: Ty,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Ctnt {
    pub trait_: DefId,
    pub tys: List<Ty>,
}

impl Ty {
    pub fn new(ty: Type, span: Span) -> Self {
        Ty(Arc::new((ty, span)))
    }

    pub fn error(span: Span) -> Self {
        Ty::new(Type::Error, span)
    }

    pub fn unknown(span: Span, uk: Unknown) -> Self {
        Ty::new(Type::Unknown(uk), span)
    }

    pub fn var(span: Span, var: TypeVar) -> Self {
        Ty::new(Type::Var(var), span)
    }

    pub fn int(span: Span, val: u128) -> Self {
        Ty::new(Type::Int(val), span)
    }

    pub fn string(span: Span, val: String) -> Self {
        Ty::new(Type::String(val), span)
    }

    pub fn ctor(span: Span, def: DefId) -> Self {
        Ty::new(Type::Ctor(def), span)
    }

    pub fn row(
        span: Span,
        fields: impl IntoIterator<Item = Field>,
        tail: impl Into<Option<Ty>>,
    ) -> Self {
        Ty::new(Type::Row(List::from_iter(fields), tail.into()), span)
    }

    pub fn app(span: Span, base: Ty, args: impl IntoIterator<Item = Ty>) -> Self {
        Ty::new(Type::App(base, List::from_iter(args)), span)
    }

    pub fn forall(
        span: Span,
        vars: impl IntoIterator<Item = TypeVar>,
        ty: Ty,
        scope: impl Into<Option<SkolemScope>>,
    ) -> Self {
        Ty::new(Type::ForAll(List::from_iter(vars), ty, scope.into()), span)
    }

    pub fn ctnt(span: Span, ctnt: Ctnt, ty: Ty) -> Self {
        Ty::new(Type::Ctnt(ctnt, ty), span)
    }

    pub fn skolem(span: Span, var: TypeVar, skolem: Skolem, scope: SkolemScope) -> Self {
        Ty::new(Type::Skolem(var, skolem, scope), span)
    }

    pub fn span(&self) -> Span {
        self.0 .1
    }

    pub fn is_mono_type(&self) -> bool {
        match **self {
            Type::ForAll(..) => false,
            _ => true,
        }
    }

    pub fn replace_vars(self, vars: HashMap<TypeVar, Ty>) -> Ty {
        self.everywhere(|t| match &*t {
            Type::Var(v) if vars.contains_key(v) => vars[v].clone(),
            _ => t,
        })
    }

    pub fn everywhere<F>(self, mut f: F) -> Ty
    where
        F: FnMut(Ty) -> Ty,
    {
        match &*self {
            Type::App(t1, t2) => {
                let t1 = f(t1.clone());
                let t2 = t2.iter().map(|t2| f(t2.clone())).collect::<List<_>>();

                f(Ty::app(self.span(), t1, t2))
            }
            Type::ForAll(vars, t1, scope) => {
                let t1 = f(t1.clone());

                f(Ty::forall(self.span(), vars, t1, *scope))
            }
            Type::Ctnt(ctnt, t2) => {
                let tys = ctnt.tys.iter().map(|t1| f(t1.clone())).collect::<List<_>>();
                let t2 = f(t2.clone());
                let ctnt = Ctnt {
                    tys,
                    trait_: ctnt.trait_,
                };

                f(Ty::ctnt(self.span(), ctnt, t2))
            }
            Type::Row(fields, tail) => {
                let fields = fields
                    .iter()
                    .map(|f1| Field {
                        span: f1.span,
                        name: f1.name,
                        ty: f(f1.ty.clone()),
                    })
                    .collect::<List<_>>();

                let tail = tail.map(f);

                f(Ty::row(self.span(), fields, tail))
            }
            _ => f(self),
        }
    }

    pub fn everywhere_result<F>(self, mut f: F) -> Result<Ty>
    where
        F: FnMut(Ty) -> Result<Ty>,
    {
        match &*self {
            Type::App(t1, t2) => {
                let t1 = f(t1.clone())?;
                let t2 = t2
                    .iter()
                    .map(|t2| f(t2.clone()))
                    .collect::<Result<List<_>>>()?;

                f(Ty::app(self.span(), t1, t2))
            }
            Type::ForAll(vars, t1, scope) => {
                let t1 = f(t1.clone())?;

                f(Ty::forall(self.span(), vars, t1, *scope))
            }
            Type::Ctnt(ctnt, t2) => {
                let tys = ctnt
                    .tys
                    .iter()
                    .map(|t1| f(t1.clone()))
                    .collect::<Result<_>>()?;
                let t2 = f(t2.clone())?;
                let ctnt = Ctnt {
                    tys,
                    trait_: ctnt.trait_,
                };

                f(Ty::ctnt(self.span(), ctnt, t2))
            }
            Type::Row(fields, tail) => {
                let fields = fields
                    .iter()
                    .map(|f1| {
                        Ok(Field {
                            span: f1.span,
                            name: f1.name,
                            ty: f(f1.ty.clone())?,
                        })
                    })
                    .collect::<Result<List<_>>>()?;

                let tail = tail.map(f).transpose()?;

                f(Ty::row(self.span(), fields, tail))
            }
            _ => f(self),
        }
    }
}

impl std::ops::Deref for Ty {
    type Target = Type;

    fn deref(&self) -> &Self::Target {
        &self.0 .0
    }
}

impl<T> List<T> {
    pub fn empty() -> Self {
        List(Arc::new([]))
    }
}

impl<T> std::ops::Deref for List<T> {
    type Target = [T];

    fn deref(&self) -> &Self::Target {
        &*self.0
    }
}

impl<T> FromIterator<T> for List<T> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        List(Vec::from_iter(iter).into())
    }
}

impl<T: Clone> IntoIterator for List<T> {
    type Item = T;
    type IntoIter = ListIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        ListIter(self, 0)
    }
}

impl<'a, T: Clone> IntoIterator for &'a List<T> {
    type Item = T;
    type IntoIter = std::iter::Cloned<std::slice::Iter<'a, T>>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter().cloned()
    }
}

pub struct ListIter<T>(List<T>, usize);

impl<T: Clone> Iterator for ListIter<T> {
    type Item = T;

    fn next(&mut self) -> Option<T> {
        if self.1 >= self.0.len() {
            None
        } else {
            self.1 += 1;

            Some(self.0[self.1 - 1].clone())
        }
    }
}
