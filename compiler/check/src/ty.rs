use hir::ir::{DefId, HirId, Symbol};
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Ty {
    Error,
    Infer(InferVar),
    Var(TypeVar),
    ForAll(Vec<TypeVar>, Box<Ty>),
    Func(Vec<Ty>, Box<Ty>),
    Data(DefId, Vec<Variant>),
    Tuple(Vec<Ty>),
    Record(Vec<Field>, Option<Box<Ty>>),
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

impl Ty {
    pub fn instantiate(self, args: Vec<Self>) -> Self {
        if let Ty::ForAll(vars, ret) = self {
            ret.replace(&vars.into_iter().zip(args).collect())
        } else {
            self
        }
    }

    pub fn replace(self, map: &HashMap<TypeVar, Self>) -> Self {
        match self {
            Ty::Var(var) => {
                if let Some(ty) = map.get(&var) {
                    ty.clone()
                } else {
                    self
                }
            }
            Ty::ForAll(vars, ty) => Ty::ForAll(vars, Box::new(ty.replace(map))),
            Ty::Func(params, ret) => Ty::Func(
                params.into_iter().map(|t| t.replace(map)).collect(),
                Box::new(ret.replace(map)),
            ),
            Ty::Data(id, variants) => Ty::Data(
                id,
                variants
                    .into_iter()
                    .map(|v| Variant {
                        id: v.id,
                        tys: v.tys.into_iter().map(|t| t.replace(map)).collect(),
                    })
                    .collect(),
            ),
            Ty::Tuple(tys) => Ty::Tuple(tys.into_iter().map(|t| t.replace(map)).collect()),
            Ty::Record(fields, tail) => Ty::Record(
                fields
                    .into_iter()
                    .map(|f| Field {
                        name: f.name,
                        ty: f.ty.replace(map),
                    })
                    .collect(),
                tail.map(|t| Box::new(t.replace(map))),
            ),
            _ => self,
        }
    }
}
