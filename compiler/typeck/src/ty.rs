use crate::error::Result;
use hir::ir::{DefId, HirId, Span, Symbol};
use source::FileId;
use std::collections::HashMap;
use std::iter::FromIterator;
use std::sync::Arc;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Ty(Arc<(Type, Span, FileId)>);

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
    Tuple(List<Ty>),
    Row(List<Field>, Option<Ty>),
    App(Ty, List<Ty>),
    KindApp(Ty, List<Ty>),
    ForAll(List<(TypeVar, Option<Ty>)>, Ty, Option<SkolemScope>),
    Ctnt(Ctnt, Ty),
    Skolem(TypeVar, Option<Ty>, Skolem, SkolemScope),
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
pub struct Variant {
    pub id: DefId,
    pub tys: List<Ty>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Ctnt {
    pub trait_: DefId,
    pub tys: List<Ty>,
}

impl Ty {
    pub fn new(ty: Type, span: Span, file: FileId) -> Self {
        Ty(Arc::new((ty, span, file)))
    }

    pub fn error(span: Span, file: FileId) -> Self {
        Ty::new(Type::Error, span, file)
    }

    pub fn unknown(span: Span, file: FileId, uk: Unknown) -> Self {
        Ty::new(Type::Unknown(uk), span, file)
    }

    pub fn var(span: Span, file: FileId, var: TypeVar) -> Self {
        Ty::new(Type::Var(var), span, file)
    }

    pub fn int(span: Span, file: FileId, val: u128) -> Self {
        Ty::new(Type::Int(val), span, file)
    }

    pub fn string(span: Span, file: FileId, val: String) -> Self {
        Ty::new(Type::String(val), span, file)
    }

    pub fn ctor(span: Span, file: FileId, def: DefId) -> Self {
        Ty::new(Type::Ctor(def), span, file)
    }

    pub fn tuple(span: Span, file: FileId, tys: impl IntoIterator<Item = Ty>) -> Self {
        Ty::new(Type::Tuple(List::from_iter(tys)), span, file)
    }

    pub fn row(
        span: Span,
        file: FileId,
        fields: impl IntoIterator<Item = Field>,
        tail: impl Into<Option<Ty>>,
    ) -> Self {
        Ty::new(Type::Row(List::from_iter(fields), tail.into()), span, file)
    }

    pub fn app(span: Span, file: FileId, base: Ty, args: impl IntoIterator<Item = Ty>) -> Self {
        Ty::new(Type::App(base, List::from_iter(args)), span, file)
    }

    pub fn kind_app(
        span: Span,
        file: FileId,
        base: Ty,
        args: impl IntoIterator<Item = Ty>,
    ) -> Self {
        Ty::new(Type::KindApp(base, List::from_iter(args)), span, file)
    }

    pub fn forall(
        span: Span,
        file: FileId,
        vars: impl IntoIterator<Item = (TypeVar, Option<Ty>)>,
        ty: Ty,
        scope: impl Into<Option<SkolemScope>>,
    ) -> Self {
        Ty::new(
            Type::ForAll(List::from_iter(vars), ty, scope.into()),
            span,
            file,
        )
    }

    pub fn ctnt(span: Span, file: FileId, ctnt: Ctnt, ty: Ty) -> Self {
        Ty::new(Type::Ctnt(ctnt, ty), span, file)
    }

    pub fn skolem(
        span: Span,
        file: FileId,
        var: TypeVar,
        kind: impl Into<Option<Ty>>,
        skolem: Skolem,
        scope: SkolemScope,
    ) -> Self {
        Ty::new(Type::Skolem(var, kind.into(), skolem, scope), span, file)
    }

    pub fn loc(&self) -> (Span, FileId) {
        (self.0 .1, self.0 .2)
    }

    pub fn span(&self) -> Span {
        self.0 .1
    }

    pub fn file(&self) -> FileId {
        self.0 .2
    }

    pub fn is_mono_type(&self) -> bool {
        match **self {
            Type::ForAll(..) => false,
            _ => true,
        }
    }

    pub fn needs_parens(&self) -> bool {
        match &**self {
            Type::ForAll(..) => true,
            Type::App(..) => true,
            Type::KindApp(..) => true,
            Type::Ctnt(..) => true,
            _ => false,
        }
    }

    pub fn replace_vars(self, vars: HashMap<TypeVar, Ty>) -> Ty {
        self.everywhere(|t| match &*t {
            Type::Var(v) if vars.contains_key(v) => vars[v].clone(),
            _ => t,
        })
    }

    pub fn equal(&self, other: &Self) -> bool {
        match (&**self, &**other) {
            (Type::Error, Type::Error) => true,
            (Type::Int(a), Type::Int(b)) => a == b,
            (Type::String(a), Type::String(b)) => a == b,
            (Type::Unknown(a), Type::Unknown(b)) => a == b,
            (Type::Var(a), Type::Var(b)) => a == b,
            (Type::Ctor(a), Type::Ctor(b)) => a == b,
            (Type::Skolem(a1, Some(b1), c1, d1), Type::Skolem(a2, Some(b2), c2, d2)) => {
                a1 == a2 && b1.equal(b2) && c1 == c2 && d1 == d2
            }
            (Type::Skolem(a1, None, c1, d1), Type::Skolem(a2, None, c2, d2)) => {
                a1 == a2 && c1 == c2 && d1 == d2
            }
            (Type::ForAll(a1, b1, c1), Type::ForAll(a2, b2, c2)) => {
                a1.iter()
                    .zip(a2.iter())
                    .all(|((v1, k1), (v2, k2))| match (k1, k2) {
                        (Some(k1), Some(k2)) => v1 == v2 && k1.equal(k2),
                        (None, None) => v1 == v2,
                        _ => false,
                    })
                    && b1.equal(b2)
                    && c1 == c2
            }
            (Type::Tuple(a), Type::Tuple(b)) => a.iter().zip(b.iter()).all(|(a, b)| a.equal(b)),
            (Type::App(a1, b1), Type::App(a2, b2))
            | (Type::KindApp(a1, b1), Type::KindApp(a2, b2)) => {
                a1.equal(a2) && b1.iter().zip(b2.iter()).all(|(a, b)| a.equal(b))
            }
            (Type::Ctnt(a1, b1), Type::Ctnt(a2, b2)) => {
                a1.trait_ == a2.trait_
                    && a1.tys.iter().zip(a2.tys.iter()).all(|(a, b)| a.equal(b))
                    && b1.equal(b2)
            }
            (Type::Row(a1, Some(b1)), Type::Row(a2, Some(b2))) => {
                a1.iter()
                    .zip(a2.iter())
                    .all(|(a, b)| a.name == b.name && a.ty.equal(&b.ty))
                    && b1.equal(b2)
            }
            (Type::Row(a1, None), Type::Row(a2, None)) => a1
                .iter()
                .zip(a2.iter())
                .all(|(a, b)| a.name == b.name && a.ty.equal(&b.ty)),
            (_, _) => false,
        }
    }

    pub fn everywhere<F>(self, mut f: F) -> Ty
    where
        F: FnMut(Ty) -> Ty,
    {
        match &*self {
            Type::App(t1, t2) => {
                let t1 = f(t1.clone());
                let t2 = t2.iter().map(|t2| f(t2.clone())).collect::<List<_>>();

                f(Ty::app(self.span(), self.file(), t1, t2))
            }
            Type::KindApp(t1, t2) => {
                let t1 = f(t1.clone());
                let t2 = t2.iter().map(|t2| f(t2.clone())).collect::<List<_>>();

                f(Ty::kind_app(self.span(), self.file(), t1, t2))
            }
            Type::ForAll(vars, t1, scope) => {
                let t1 = f(t1.clone());

                f(Ty::forall(self.span(), self.file(), vars, t1, *scope))
            }
            Type::Ctnt(ctnt, t2) => {
                let tys = ctnt.tys.iter().map(|t1| f(t1.clone())).collect::<List<_>>();
                let t2 = f(t2.clone());
                let ctnt = Ctnt {
                    tys,
                    trait_: ctnt.trait_,
                };

                f(Ty::ctnt(self.span(), self.file(), ctnt, t2))
            }
            Type::Tuple(tys) => {
                let tys = tys.iter().map(|t| f(t.clone())).collect::<List<_>>();

                f(Ty::tuple(self.span(), self.file(), tys))
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

                let tail = tail.clone().map(|t| f(t));

                f(Ty::row(self.span(), self.file(), fields, tail))
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

                f(Ty::app(self.span(), self.file(), t1, t2))
            }
            Type::KindApp(t1, t2) => {
                let t1 = f(t1.clone())?;
                let t2 = t2
                    .iter()
                    .map(|t2| f(t2.clone()))
                    .collect::<Result<List<_>>>()?;

                f(Ty::kind_app(self.span(), self.file(), t1, t2))
            }
            Type::ForAll(vars, t1, scope) => {
                let t1 = f(t1.clone())?;

                f(Ty::forall(self.span(), self.file(), vars, t1, *scope))
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

                f(Ty::ctnt(self.span(), self.file(), ctnt, t2))
            }
            Type::Tuple(tys) => {
                let tys = tys
                    .iter()
                    .map(|t| f(t.clone()))
                    .collect::<Result<List<_>>>()?;

                f(Ty::tuple(self.span(), self.file(), tys))
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

                let tail = tail.clone().map(|t| f(t)).transpose()?;

                f(Ty::row(self.span(), self.file(), fields, tail))
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

impl std::ops::BitXor<(Span, FileId)> for Ty {
    type Output = Self;

    fn bitxor(self, rhs: (Span, FileId)) -> Self {
        Ty::new(self.0 .0.clone(), rhs.0, rhs.1)
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

impl<T, const N: usize> From<[T; N]> for List<T> {
    fn from(src: [T; N]) -> Self {
        List(Arc::new(src))
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
