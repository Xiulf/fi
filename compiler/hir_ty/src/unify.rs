use diagnostics::Diagnostics;
use ena::unify::{InPlace, UnificationTable, UnifyKey, UnifyValue};
use ra_ap_stdx::hash::NoHashHashMap;

use crate::ctx::Ctx;
use crate::diagnostics::{RecursiveType, TypeMismatch};
use crate::ty::{GeneralizedType, Ty, TyKind, Unknown};
use crate::TyOrigin;

const RECURSION_LIMIT: u32 = 32;

#[derive(Default, Debug)]
pub struct Substitution {
    pub(crate) solved: NoHashHashMap<Unknown, Ty>,
    unsolved: NoHashHashMap<Unknown, Ty>,
    table: UnificationTable<InPlace<Unknown>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct UnkLevel(pub(crate) u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnifyResult {
    Ok,
    Fail,
    RecursiveType(Ty),
}

impl UnifyResult {
    fn and(self, other: Self) -> Self {
        match self {
            | Self::Ok => other,
            | _ => self,
        }
    }
}

impl Ctx<'_> {
    pub fn fresh_type_with_kind(&mut self, level: UnkLevel, kind: Ty) -> Ty {
        let u = self.subst.table.new_key(level);
        self.subst.unsolved.insert(u, kind);
        Ty::new(self.db, TyKind::Unknown(u))
    }

    pub fn fresh_type_without_kind(&mut self, level: UnkLevel) -> Ty {
        let kind = self.fresh_type(level);
        self.fresh_type_with_kind(level, kind)
    }

    pub fn fresh_type(&mut self, level: UnkLevel) -> Ty {
        let kind = self.type_kind();
        self.fresh_type_with_kind(level, kind)
    }

    pub fn unify_generalized_types(
        &mut self,
        t1: &GeneralizedType,
        t2: &GeneralizedType,
        origin: TyOrigin,
    ) -> GeneralizedType {
        match (t1, t2) {
            | (GeneralizedType::Mono(t1), GeneralizedType::Mono(t2)) => {
                self.unify_types(*t1, *t2, origin);
                GeneralizedType::Mono(*t1)
            },
            | (GeneralizedType::Poly(v1, t1), GeneralizedType::Poly(v2, t2)) if v1.len() == v2.len() => {
                // TODO: check vars
                self.unify_types(*t1, *t2, origin);
                GeneralizedType::Poly(v1.clone(), *t1)
            },
            | _ => {
                // TODO: report error
                todo!()
            },
        }
    }

    pub fn unify_types(&mut self, t1: Ty, t2: Ty, origin: TyOrigin) {
        match self.unify(t1, t2) {
            | UnifyResult::Ok => {},
            | UnifyResult::Fail => {
                Diagnostics::emit(self.db, TypeMismatch {
                    a: self.resolve_type_fully(t1),
                    b: self.resolve_type_fully(t2),
                    owner: self.owner,
                    origin,
                });
            },
            | UnifyResult::RecursiveType(t) => {
                Diagnostics::emit(self.db, RecursiveType {
                    ty: self.resolve_type_fully(t),
                    owner: self.owner,
                    origin,
                });
            },
        }
    }

    fn unify(&mut self, t1: Ty, t2: Ty) -> UnifyResult {
        match (t1.kind(self.db), t2.kind(self.db)) {
            | (TyKind::Error, _) | (_, TyKind::Error) => UnifyResult::Ok,
            | (TyKind::Unknown(u1), TyKind::Unknown(u2)) if u1 == u2 => UnifyResult::Ok,
            | (TyKind::Ctor(c1), TyKind::Ctor(c2)) if c1 == c2 => UnifyResult::Ok,
            | (TyKind::Unknown(u), _) => self.unify_unknown(*u, t1, t2),
            | (_, TyKind::Unknown(u)) => self.unify_unknown(*u, t2, t1),
            | (TyKind::App(a_base, a_args), TyKind::App(b_base, b_args)) if a_args.len() == b_args.len() => self
                .unify(*a_base, *b_base)
                .and(self.unify_all(a_args.iter(), b_args.iter())),
            | (TyKind::Func(a), TyKind::Func(b)) => {
                if a.params.len() != b.params.len() {
                    if !(a.variadic && b.params.len() >= a.params.len())
                        && !(b.variadic && a.params.len() >= b.params.len())
                    {
                        return UnifyResult::Fail;
                    }
                }

                self.unify_all(a.params.iter(), b.params.iter())
                    .and(self.unify(a.ret, b.ret))
                    .and(self.unify(a.env, b.env))
            },
            | (_, _) => UnifyResult::Fail,
        }
    }

    fn unify_all<'a, 'b>(
        &mut self,
        mut a: impl Iterator<Item = &'a Ty>,
        mut b: impl Iterator<Item = &'b Ty>,
    ) -> UnifyResult {
        while let (Some(&a), Some(&b)) = (a.next(), b.next()) {
            let res = self.unify(a, b);
            if res != UnifyResult::Ok {
                return res;
            }
        }

        UnifyResult::Ok
    }

    fn unify_unknown(&mut self, u: Unknown, t1: Ty, t2: Ty) -> UnifyResult {
        match self.find_binding(u) {
            | Ok(t) => self.unify(t, t2),
            | Err((level, _kind)) => {
                let b = self.resolve_type_shallow(t2);

                if t1 == b {
                    return UnifyResult::Ok;
                }

                if self.occurs(u, level, b, RECURSION_LIMIT) {
                    return UnifyResult::RecursiveType(b);
                }

                // TODO: check kind

                self.subst.solved.insert(u, b);
                UnifyResult::Ok
            },
        }
    }

    fn occurs(&mut self, u: Unknown, level: UnkLevel, ty: Ty, n: u32) -> bool {
        if n == 0 {
            panic!("recursion limit reached in occurs");
        }

        let n = n - 1;

        match ty.kind(self.db) {
            | TyKind::Unknown(u2) => match self.find_binding(*u2) {
                | Ok(t) => self.occurs(u, level, t, n),
                | Err((_, _)) => u == *u2,
            },
            | TyKind::App(base, args) => {
                self.occurs(u, level, *base, n) || args.iter().any(|&a| self.occurs(u, level, a, n))
            },
            | TyKind::Func(func) => {
                func.params.iter().any(|&a| self.occurs(u, level, a, n))
                    || self.occurs(u, level, func.ret, n)
                    || self.occurs(u, level, func.env, n)
            },
            | _ => false,
        }
    }

    pub(crate) fn find_binding(&mut self, u: Unknown) -> Result<Ty, (UnkLevel, Ty)> {
        match self.subst.solved.get(&u).copied() {
            | Some(t) => Ok(t),
            | None => Err((self.subst.table.probe_value(u), self.subst.unsolved[&u])),
        }
    }

    pub fn resolve_type_shallow(&mut self, t: Ty) -> Ty {
        match t.kind(self.db) {
            | TyKind::Unknown(u) => match self.subst.solved.get(&self.subst.table.find(*u)).copied() {
                | Some(t) => self.resolve_type_shallow(t),
                | None => t,
            },
            | _ => t,
        }
    }

    pub fn resolve_type_fully(&mut self, t: Ty) -> Ty {
        t.fold(self.db, &mut |t| match t.kind(self.db) {
            | TyKind::Unknown(u) => match self.subst.solved.get(&self.subst.table.find(*u)).copied() {
                | Some(t) => self.resolve_type_fully(t),
                | None => t,
            },
            | _ => t,
        })
    }

    pub fn resolve_generalized_type_fully(&mut self, t: GeneralizedType) -> GeneralizedType {
        match t {
            | GeneralizedType::Mono(t) => GeneralizedType::Mono(self.resolve_type_fully(t)),
            | GeneralizedType::Poly(vars, t) => GeneralizedType::Poly(vars, self.resolve_type_fully(t)),
        }
    }
}

impl UnifyKey for Unknown {
    type Value = UnkLevel;

    fn index(&self) -> u32 {
        self.0
    }

    fn from_index(u: u32) -> Self {
        Self(u)
    }

    fn tag() -> &'static str {
        "unknown"
    }
}

impl UnifyValue for UnkLevel {
    type Error = ena::unify::NoError;

    fn unify_values(value1: &Self, value2: &Self) -> Result<Self, Self::Error> {
        Ok(*value1.min(value2))
    }
}

impl ra_ap_stdx::hash::NoHashHashable for Unknown {
}
