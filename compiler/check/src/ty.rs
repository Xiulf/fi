use hir::ir::{DefId, HirId, Symbol};
use std::collections::HashMap;
use std::fmt;
use std::sync::Arc;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Ty(Arc<Type>);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct List<T>(Arc<[T]>);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Error,
    Infer(InferVar),
    Var(TypeVar),
    ForAll(List<TypeVar>, Ty),
    Func(List<Ty>, Ty),
    Data(DefId, List<Variant>),
    Tuple(List<Ty>),
    Record(List<Field>, Option<Ty>),
    App(Ty, List<Ty>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct InferVar(pub u64);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeVar(pub HirId);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Variant {
    pub id: DefId,
    pub tys: Vec<Ty>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Field {
    pub name: Symbol,
    pub ty: Ty,
}

pub struct TyDisplay<'a>(&'a Ty, &'a dyn crate::TypeDatabase);
pub struct FieldDisplay<'a>(&'a Field, &'a dyn crate::TypeDatabase);

impl Ty {
    pub fn error() -> Self {
        Ty(Arc::new(Type::Error))
    }

    pub fn infer(var: InferVar) -> Self {
        Ty(Arc::new(Type::Infer(var)))
    }

    pub fn var(var: TypeVar) -> Self {
        Ty(Arc::new(Type::Var(var)))
    }

    pub fn for_all(vars: List<TypeVar>, ty: Ty) -> Self {
        Ty(Arc::new(Type::ForAll(vars, ty)))
    }

    pub fn func(params: List<Ty>, ret: Ty) -> Self {
        Ty(Arc::new(Type::Func(params, ret)))
    }

    pub fn data(id: DefId, variants: List<Variant>) -> Self {
        Ty(Arc::new(Type::Data(id, variants)))
    }

    pub fn tuple(tys: List<Ty>) -> Self {
        Ty(Arc::new(Type::Tuple(tys)))
    }

    pub fn record(fields: List<Field>, tail: Option<Ty>) -> Self {
        Ty(Arc::new(Type::Record(fields, tail)))
    }

    pub fn app(ty: Ty, args: List<Ty>) -> Self {
        Ty(Arc::new(Type::App(ty, args)))
    }

    pub fn display<'a>(&'a self, db: &'a dyn crate::TypeDatabase) -> TyDisplay<'a> {
        TyDisplay(self, db)
    }

    fn prec(&self) -> usize {
        match **self {
            Type::ForAll(..) => 1,
            Type::Func(..) => 1,
            Type::App(..) => 1,
            _ => 0,
        }
    }

    pub fn instantiate(&self, args: &List<Self>) -> Self {
        if let Type::ForAll(vars, ret) = &**self {
            ret.replace(&vars.into_iter().zip(args).collect())
        } else {
            self.clone()
        }
    }

    pub fn replace(&self, map: &HashMap<TypeVar, Self>) -> Self {
        match &**self {
            Type::Var(var) => {
                if let Some(ty) = map.get(var) {
                    ty.clone()
                } else {
                    self.clone()
                }
            }
            Type::ForAll(vars, ty) => Ty::for_all(vars.clone(), ty.replace(map)),
            Type::Func(params, ret) => Ty::func(
                params.into_iter().map(|t| t.replace(map)).collect(),
                ret.replace(map),
            ),
            Type::Data(id, variants) => Ty::data(
                *id,
                variants
                    .into_iter()
                    .map(|v| Variant {
                        id: v.id,
                        tys: v.tys.into_iter().map(|t| t.replace(map)).collect(),
                    })
                    .collect(),
            ),
            Type::Tuple(tys) => Ty::tuple(tys.into_iter().map(|t| t.replace(map)).collect()),
            Type::Record(fields, tail) => Ty::record(
                fields
                    .into_iter()
                    .map(|f| Field {
                        name: f.name,
                        ty: f.ty.replace(map),
                    })
                    .collect(),
                tail.as_ref().map(|t| t.replace(map)),
            ),
            _ => self.clone(),
        }
    }
}

impl<T> List<T> {
    pub fn new() -> Self {
        List(Arc::new([]))
    }
}

impl Field {
    pub fn display<'a>(&'a self, db: &'a dyn crate::TypeDatabase) -> FieldDisplay<'a> {
        FieldDisplay(self, db)
    }
}

impl std::ops::Deref for Ty {
    type Target = Type;

    fn deref(&self) -> &Self::Target {
        &*self.0
    }
}

impl<T> std::iter::FromIterator<T> for List<T> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        List(<Vec<T>>::from_iter(iter).into())
    }
}

impl<'a, T: Clone> IntoIterator for &'a List<T> {
    type IntoIter = std::iter::Cloned<std::slice::Iter<'a, T>>;
    type Item = T;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter().cloned()
    }
}

impl<T> From<Vec<T>> for List<T> {
    fn from(src: Vec<T>) -> Self {
        List(src.into())
    }
}

impl<T> std::ops::Deref for List<T> {
    type Target = [T];

    fn deref(&self) -> &Self::Target {
        &*self.0
    }
}

impl fmt::Display for TyDisplay<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &**self.0 {
            Type::Error => write!(f, "[error]"),
            Type::Infer(var) => write!(f, "{}", var),
            Type::Var(var) => write!(f, "{}", var),
            Type::ForAll(vars, ty) => {
                write!(f, "forall")?;

                for var in vars {
                    write!(f, " {}", var)?;
                }

                write!(f, ". {}", ty.display(self.1))
            }
            Type::Func(params, ret) => {
                if params.len() == 1 {
                    write!(
                        f,
                        "{} -> {}",
                        params[0].display(self.1),
                        ret.display(self.1)
                    )
                } else {
                    write!(f, "(")?;

                    for (i, param) in params.iter().enumerate() {
                        if i != 0 {
                            write!(f, ", ")?;
                        }

                        write!(f, "{}", param.display(self.1))?;
                    }

                    write!(f, ") -> {}", ret.display(self.1))
                }
            }
            Type::Data(id, _) => {
                let file = self.1.module_tree(id.lib).file(id.module);
                let module = self.1.module_hir(file);
                let def = module.def(*id);

                match def {
                    hir::ir::Def::Item(item) => write!(f, "{}.{}", module.name, item.name),
                    hir::ir::Def::TraitItem(item) => write!(f, "{}.{}", module.name, item.name),
                    hir::ir::Def::ImplItem(item) => write!(f, "{}.{}", module.name, item.name),
                }
            }
            Type::Tuple(tys) => {
                write!(f, "(")?;

                for (i, ty) in tys.iter().enumerate() {
                    if i != 0 {
                        write!(f, ", ")?;
                    }

                    write!(f, "{}", ty.display(self.1))?;
                }

                write!(f, ")")
            }
            Type::Record(fields, tail) => {
                write!(f, "{{")?;

                for (i, field) in fields.iter().enumerate() {
                    if i != 0 {
                        write!(f, ",")?;
                    }

                    write!(f, " {}", field.display(self.1))?;
                }

                if let Some(tail) = tail {
                    write!(f, " | {}", tail.display(self.1))?;
                }

                write!(f, " }}")
            }
            Type::App(ty, args) => {
                ty.display(self.1).fmt(f)?;

                let ty_prec = self.0.prec();

                for arg in args {
                    if arg.prec() >= ty_prec {
                        write!(f, " ({})", arg.display(self.1))?;
                    } else {
                        write!(f, " {}", arg.display(self.1))?;
                    }
                }

                Ok(())
            }
        }
    }
}

impl fmt::Display for InferVar {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "${}", self.0)
    }
}

impl fmt::Display for TypeVar {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut num = self.0.local_id.0;
        let mut s = String::new();

        while num >= 26 {
            s.push((b'a' + (num % 26) as u8) as char);
            num = num.saturating_sub(26);
        }

        s.push((b'a' + num as u8) as char);

        write!(f, "{}", s)
    }
}

impl fmt::Display for FieldDisplay<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} :: {}", self.0.name, self.0.ty.display(self.1))
    }
}
