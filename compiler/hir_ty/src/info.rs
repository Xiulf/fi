use crate::{
    db::HirDatabase,
    ty::{DebruijnIndex, List, TypeVar},
};
use hir_def::{
    arena::{Arena, ArenaMap, Idx},
    id::{ClassId, TypeCtorId},
    in_file::InFile,
    name::Name,
};
use syntax::TextRange;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TyId(Idx<TyInfo>);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TyInfo {
    Error,

    Unknown(Unknown),
    Skolem(TypeVar, TyId),
    TypeVar(TypeVar),

    Figure(i128),
    Symbol(Box<str>),
    Row(List<FieldInfo>, Option<TyId>),

    Ctor(TypeCtorId),
    App(TyId, List<TyId>),

    Tuple(List<TyId>),
    Func(List<TyId>, TyId),

    Ctnt(CtntInfo, TyId),
    ForAll(List<TyId>, TyId),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Unknown(u32);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FieldInfo {
    pub name: Name,
    pub ty: TyId,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CtntInfo {
    pub class: ClassId,
    pub types: List<TyId>,
}

pub type Span = InFile<TextRange>;

#[derive(Default)]
pub struct Types {
    types: Arena<TyInfo>,
    spans: ArenaMap<Idx<TyInfo>, Span>,
}

pub trait ToInfo {
    type Output;

    fn to_info(self, db: &dyn HirDatabase, types: &mut Types, span: Span) -> Self::Output;
}

pub trait FromInfo {
    type Input;

    fn from_info(db: &dyn HirDatabase, types: &Types, input: Self::Input) -> Self;
}

impl Types {
    pub fn insert(&mut self, ty: TyInfo, span: Span) -> TyId {
        let id = self.types.alloc(ty);
        self.spans.insert(id, span);
        TyId(id)
    }

    pub fn update(&mut self, id: TyId, ty: TyInfo) -> TyId {
        if self[id] == ty {
            id
        } else {
            let span = self.span(id);
            self.insert(ty, span)
        }
    }

    pub fn span(&self, id: TyId) -> Span {
        self.spans[id.0]
    }
}

impl std::ops::Index<TyId> for Types {
    type Output = TyInfo;

    fn index(&self, id: TyId) -> &Self::Output {
        &self.types[id.0]
    }
}

impl Unknown {
    pub const fn from_raw(id: u32) -> Self {
        Unknown(id)
    }

    pub const fn raw(self) -> u32 {
        self.0
    }

    pub fn to_ty(self, types: &mut Types, span: Span) -> TyId {
        types.insert(TyInfo::Unknown(self), span)
    }
}

impl TyId {
    pub fn everywhere<F>(self, types: &mut Types, f: &mut F) -> TyId
    where
        F: FnMut(&mut Types, TyId) -> TyId,
    {
        match types[self].clone() {
            | TyInfo::Skolem(sk, k) => {
                let k = k.everywhere(types, f);
                let ty = types.update(self, TyInfo::Skolem(sk, k));

                f(types, ty)
            },
            | TyInfo::Row(fields, tail) => {
                let fields = fields
                    .iter()
                    .map(|field| FieldInfo {
                        name: field.name.clone(),
                        ty: field.ty.everywhere(types, f),
                    })
                    .collect();

                let tail = tail.map(|t| t.everywhere(types, f));
                let ty = types.update(self, TyInfo::Row(fields, tail));

                f(types, ty)
            },
            | TyInfo::App(base, args) => {
                let base = base.everywhere(types, f);
                let args = args.iter().map(|t| t.everywhere(types, f)).collect();
                let ty = types.update(self, TyInfo::App(base, args));

                f(types, ty)
            },
            | TyInfo::Tuple(tys) => {
                let tys = tys.iter().map(|t| t.everywhere(types, f)).collect();
                let ty = types.update(self, TyInfo::Tuple(tys));

                f(types, ty)
            },
            | TyInfo::Func(args, ret) => {
                let args = args.iter().map(|t| t.everywhere(types, f)).collect();
                let ret = ret.everywhere(types, f);
                let ty = types.update(self, TyInfo::Func(args, ret));

                f(types, ty)
            },
            | TyInfo::Ctnt(ctnt, inner) => {
                let ctnt = CtntInfo {
                    class: ctnt.class,
                    types: ctnt.types.iter().map(|t| t.everywhere(types, f)).collect(),
                };

                let inner = inner.everywhere(types, f);
                let ty = types.update(self, TyInfo::Ctnt(ctnt, inner));

                f(types, ty)
            },
            | TyInfo::ForAll(k, t) => {
                let k = k.iter().map(|k| k.everywhere(types, f)).collect();
                let t = t.everywhere(types, f);
                let ty = types.update(self, TyInfo::ForAll(k, t));

                f(types, ty)
            },
            | _ => f(types, self),
        }
    }

    pub fn everything<F>(self, types: &Types, f: &mut F)
    where
        F: FnMut(TyId),
    {
        match types[self] {
            | TyInfo::Skolem(_, k) => k.everything(types, f),
            | TyInfo::Row(ref fields, tail) => {
                for field in fields.iter() {
                    field.ty.everything(types, f);
                }

                if let Some(tail) = tail {
                    tail.everything(types, f);
                }
            },
            | TyInfo::App(base, ref args) => {
                base.everything(types, f);

                for ty in args.iter() {
                    ty.everything(types, f);
                }
            },
            | TyInfo::Tuple(ref tys) => {
                for ty in tys.iter() {
                    ty.everything(types, f);
                }
            },
            | TyInfo::Func(ref args, ret) => {
                for ty in args.iter() {
                    ty.everything(types, f);
                }

                ret.everything(types, f);
            },
            | TyInfo::Ctnt(ref ctnt, ty) => {
                for ty in ctnt.types.iter() {
                    ty.everything(types, f);
                }

                ty.everything(types, f);
            },
            | TyInfo::ForAll(ref k, t) => {
                for ty in k.iter() {
                    ty.everything(types, f);
                }

                t.everything(types, f);
            },
            | _ => {},
        }

        f(self)
    }

    pub fn normalize(self, types: &mut Types) -> TyId {
        self.everywhere(types, &mut |types, ty| match types[ty] {
            | TyInfo::Row(ref f1, Some(tail)) => match types[tail] {
                | TyInfo::Row(ref f2, None) => {
                    let fields = f1.iter().cloned().chain(f2.iter().cloned()).collect();

                    types.update(ty, TyInfo::Row(fields, None))
                },
                | TyInfo::TypeVar(_) | TyInfo::Unknown(_) | TyInfo::Error => ty,
                | _ => unreachable!("{:?}", tail),
            },
            | _ => ty,
        })
    }

    pub fn replace_vars(self, types: &mut Types, with: &[TyId]) -> TyId {
        Self::replace_vars_impl(types, self, with, DebruijnIndex::INNER)
    }

    fn replace_vars_impl(types: &mut Types, ty: TyId, with: &[TyId], depth: DebruijnIndex) -> TyId {
        match types[ty].clone() {
            | TyInfo::TypeVar(var) if var.debruijn() == depth => with[var.idx() as usize],
            | TyInfo::Skolem(sk, k) => {
                let k = Self::replace_vars_impl(types, k, with, depth);

                types.update(ty, TyInfo::Skolem(sk, k))
            },
            | TyInfo::Row(fields, tail) => {
                let fields = fields
                    .iter()
                    .map(|f| FieldInfo {
                        name: f.name.clone(),
                        ty: Self::replace_vars_impl(types, f.ty, with, depth),
                    })
                    .collect();

                let tail = tail.map(|t| Self::replace_vars_impl(types, t, with, depth));

                types.update(ty, TyInfo::Row(fields, tail))
            },
            | TyInfo::App(base, args) => {
                let base = Self::replace_vars_impl(types, base, with, depth);
                let args = args
                    .iter()
                    .map(|&t| Self::replace_vars_impl(types, t, with, depth))
                    .collect();

                types.update(ty, TyInfo::App(base, args))
            },
            | TyInfo::Tuple(tys) => {
                let tys = tys
                    .iter()
                    .map(|&t| Self::replace_vars_impl(types, t, with, depth))
                    .collect();

                types.update(ty, TyInfo::Tuple(tys))
            },
            | TyInfo::Func(args, ret) => {
                let args = args
                    .iter()
                    .map(|&t| Self::replace_vars_impl(types, t, with, depth))
                    .collect();

                let ret = Self::replace_vars_impl(types, ret, with, depth);

                types.update(ty, TyInfo::Func(args, ret))
            },
            | TyInfo::Ctnt(ctnt, inner) => {
                let ctnt = CtntInfo {
                    class: ctnt.class,
                    types: ctnt
                        .types
                        .iter()
                        .map(|&t| Self::replace_vars_impl(types, t, with, depth))
                        .collect(),
                };

                let inner = Self::replace_vars_impl(types, inner, with, depth);

                types.update(ty, TyInfo::Ctnt(ctnt, inner))
            },
            | TyInfo::ForAll(k, inner) => {
                let k = k
                    .iter()
                    .map(|&k| Self::replace_vars_impl(types, k, with, depth))
                    .collect();
                let inner = Self::replace_vars_impl(types, inner, with, depth.shifted_in());

                types.update(ty, TyInfo::ForAll(k, inner))
            },
            | _ => ty,
        }
    }

    pub fn to_row_list(self, types: &Types) -> (List<FieldInfo>, Option<TyId>) {
        match types[self] {
            | TyInfo::Row(ref fields, tail) => (fields.clone(), tail),
            | _ => (vec![].into(), None),
        }
    }

    pub fn align_rows_with<R>(
        types: &Types,
        mut f: impl FnMut(TyId, TyId) -> R,
        t1: TyId,
        t2: TyId,
    ) -> (
        Vec<R>,
        ((List<FieldInfo>, Option<TyId>), (List<FieldInfo>, Option<TyId>)),
    ) {
        let (s1, tail1) = t1.to_row_list(types);
        let (s2, tail2) = t2.to_row_list(types);

        return go((types, &mut f, tail1, tail2), s1.iter().cloned(), s2.iter().cloned());

        fn go<R>(
            (types, f, t1, t2): (&Types, &mut impl FnMut(TyId, TyId) -> R, Option<TyId>, Option<TyId>),
            mut s1: impl Iterator<Item = FieldInfo> + Clone,
            mut s2: impl Iterator<Item = FieldInfo> + Clone,
        ) -> (
            Vec<R>,
            ((List<FieldInfo>, Option<TyId>), (List<FieldInfo>, Option<TyId>)),
        ) {
            let lhs = s1.clone();
            let rhs = s2.clone();

            match (s1.next(), s2.next()) {
                | (None, _) => (Vec::new(), ((vec![].into(), t1), (rhs.collect(), t2))),
                | (_, None) => (Vec::new(), ((lhs.collect(), t1), (vec![].into(), t2))),
                | (Some(f1), Some(f2)) => {
                    if f1.name < f2.name {
                        let (vals, (mut lhs, rhs)) = go((types, f, t1, t2), s1, rhs);

                        lhs.0 = std::iter::once(f1).chain(lhs.0.iter().cloned()).collect();
                        (vals, (lhs, rhs))
                    } else if f2.name < f1.name {
                        let (vals, (lhs, mut rhs)) = go((types, f, t1, t2), lhs, s2);

                        rhs.0 = std::iter::once(f2).chain(rhs.0.iter().cloned()).collect();
                        (vals, (lhs, rhs))
                    } else {
                        let (mut vals, rest) = go((types, f, t1, t2), s1, s2);

                        vals.insert(0, f(f1.ty, f2.ty));
                        (vals, rest)
                    }
                },
            }
        }
    }
}
