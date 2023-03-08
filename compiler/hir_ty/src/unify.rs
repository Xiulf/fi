use diagnostics::Diagnostics;
use hir_def::display::HirDisplay;
use ra_ap_stdx::hash::NoHashHashMap;

use crate::ctx::Ctx;
use crate::diagnostics::{RecursiveType, TypeMismatch};
use crate::ty::{Constraint, GeneralizedType, PrimitiveType, Ty, TyKind, Unknown};
use crate::{Db, TyOrigin};

const RECURSION_LIMIT: u32 = 32;

#[derive(Default, Debug)]
pub struct Substitution {
    pub(crate) solved: UnifyBindings,
    unsolved: NoHashHashMap<Unknown, (UnkLevel, Ty)>,
    next_unknown: u32,
}

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct UnifyBindings(pub NoHashHashMap<Unknown, Ty>);

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
    fn fresh_unknown(&mut self) -> Unknown {
        let u = Unknown(self.subst.next_unknown);
        self.subst.next_unknown += 1;
        u
    }

    pub fn fresh_type_with_kind(&mut self, level: UnkLevel, kind: Ty, skolem: bool) -> Ty {
        let u = self.fresh_unknown();
        self.subst.unsolved.insert(u, (level, kind));
        Ty::new(self.db, TyKind::Unknown(u, skolem))
    }

    pub fn fresh_type(&mut self, level: UnkLevel, skolem: bool) -> Ty {
        let kind = self.type_kind();
        self.fresh_type_with_kind(level, kind, skolem)
    }

    pub fn unify_generalized_types(
        &mut self,
        t1: &GeneralizedType,
        t2: GeneralizedType,
        origin: TyOrigin,
    ) -> GeneralizedType {
        match (t1, t2) {
            | (GeneralizedType::Mono(t1), GeneralizedType::Mono(t2)) => {
                self.unify_types(*t1, t2, origin);
                GeneralizedType::Mono(*t1)
            },
            | (GeneralizedType::Poly(v1, t1), GeneralizedType::Poly(v2, t2)) if v1.len() == v2.len() => {
                // TODO: check vars
                self.unify_types(*t1, t2, origin);
                GeneralizedType::Poly(v1.clone(), *t1)
            },
            | (GeneralizedType::Mono(t1), t2) => {
                let (t2, _) = self.instantiate(t2, Vec::new(), true);
                self.unify_types(*t1, t2, origin);
                GeneralizedType::Mono(*t1)
            },
            | (GeneralizedType::Poly(vars, t1), GeneralizedType::Mono(t2)) => {
                self.unify_types(*t1, t2, origin);
                GeneralizedType::Poly(vars.clone(), *t1)
            },
            | (t1, t2) => {
                // TODO: report error
                tracing::error!("{} == {}", t1.display(self.db), t2.display(self.db));
                todo!()
            },
        }
    }

    pub fn unify_types(&mut self, t1: Ty, t2: Ty, origin: TyOrigin) {
        match self.unify(t1, t2) {
            | UnifyResult::Ok => {},
            | UnifyResult::Fail => {
                Diagnostics::emit(self.db, TypeMismatch {
                    a: self.resolve_type_fully(t2),
                    b: self.resolve_type_fully(t1),
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
        let bindings = unsafe { std::mem::transmute(&mut self.subst.solved) };
        self.unify_into(t1, t2, bindings)
    }

    pub(crate) fn unify_into(&self, t1: Ty, t2: Ty, bindings: &mut UnifyBindings) -> UnifyResult {
        tracing::trace!("unify_into({}, {})", t1.display(self.db), t2.display(self.db));
        match (t1.kind(self.db), t2.kind(self.db)) {
            | (TyKind::Error, _) | (_, TyKind::Error) => UnifyResult::Ok,
            | (TyKind::Unknown(u1, _), TyKind::Unknown(u2, _)) if u1 == u2 => UnifyResult::Ok,
            | (TyKind::Primitive(p1), TyKind::Primitive(p2)) if p1 == p2 => UnifyResult::Ok,
            | (TyKind::Var(v1), TyKind::Var(v2)) if v1 == v2 => UnifyResult::Ok,
            | (TyKind::Ctor(c1), TyKind::Ctor(c2)) if c1 == c2 => UnifyResult::Ok,
            | (TyKind::Unknown(u, false), _) => self.unify_unknown(*u, t1, t2, bindings),
            | (_, TyKind::Unknown(u, false)) => self.unify_unknown(*u, t2, t1, bindings),
            | (TyKind::Ctor(c), _) => {
                use PrimitiveType::*;
                let (base, kind) = if let Some(kind) = self.ctor_int_kind(*c) {
                    (self.int_type(), Ty::new(self.db, TyKind::Primitive(Integer(kind))))
                } else if let Some(kind) = self.ctor_float_kind(*c) {
                    (self.float_type(), Ty::new(self.db, TyKind::Primitive(Float(kind))))
                } else {
                    return UnifyResult::Fail;
                };

                let app = Ty::new(self.db, TyKind::App(base, Box::new([kind])));
                self.unify_into(app, t2, bindings)
            },
            | (_, TyKind::Ctor(_)) => self.unify_into(t2, t1, bindings),
            | (TyKind::App(a_base, a_args), TyKind::App(b_base, b_args)) if a_args.len() == b_args.len() => self
                .unify_into(*a_base, *b_base, bindings)
                .and(self.unify_all(a_args.iter(), b_args.iter(), bindings)),
            | (TyKind::Func(a), TyKind::Func(b)) => {
                if a.params.len() != b.params.len() {
                    if !(a.variadic && b.params.len() >= a.params.len())
                        && !(b.variadic && a.params.len() >= b.params.len())
                    {
                        return UnifyResult::Fail;
                    }
                }

                self.unify_all(a.params.iter(), b.params.iter(), bindings)
                    .and(self.unify_into(a.ret, b.ret, bindings))
                    .and(self.unify_into(a.env, b.env, bindings))
            },
            | (_, _) => UnifyResult::Fail,
        }
    }

    pub(crate) fn unify_all<'a, 'b>(
        &self,
        mut a: impl Iterator<Item = &'a Ty>,
        mut b: impl Iterator<Item = &'b Ty>,
        bindings: &mut UnifyBindings,
    ) -> UnifyResult {
        while let (Some(&a), Some(&b)) = (a.next(), b.next()) {
            let res = self.unify_into(a, b, bindings);
            if res != UnifyResult::Ok {
                return res;
            }
        }

        UnifyResult::Ok
    }

    fn unify_unknown(&self, u: Unknown, t1: Ty, t2: Ty, bindings: &mut UnifyBindings) -> UnifyResult {
        match self.find_binding(u, bindings) {
            | Ok(t) => self.unify_into(t, t2, bindings),
            | Err((level, _kind)) => {
                let b = bindings.resolve_type_shallow(self.db, t2);
                let b = self.resolve_type_shallow(b);

                if t1 == b {
                    return UnifyResult::Ok;
                }

                if self.occurs(u, level, b, RECURSION_LIMIT, bindings) {
                    return UnifyResult::RecursiveType(b);
                }

                // TODO: check kind

                bindings.0.insert(u, b);
                UnifyResult::Ok
            },
        }
    }

    fn occurs(&self, u: Unknown, level: UnkLevel, ty: Ty, n: u32, bindings: &UnifyBindings) -> bool {
        if n == 0 {
            panic!("recursion limit reached in occurs");
        }

        let n = n - 1;

        match ty.kind(self.db) {
            | TyKind::Unknown(u2, _) => match self.find_binding(*u2, bindings) {
                | Ok(t) => self.occurs(u, level, t, n, bindings),
                | Err((_, _)) => u == *u2,
            },
            | TyKind::App(base, args) => {
                self.occurs(u, level, *base, n, bindings) || args.iter().any(|&a| self.occurs(u, level, a, n, bindings))
            },
            | TyKind::Func(func) => {
                func.params.iter().any(|&a| self.occurs(u, level, a, n, bindings))
                    || self.occurs(u, level, func.ret, n, bindings)
                    || self.occurs(u, level, func.env, n, bindings)
            },
            | _ => false,
        }
    }

    pub(crate) fn find_binding(&self, u: Unknown, bindings: &UnifyBindings) -> Result<Ty, (UnkLevel, Ty)> {
        match bindings.0.get(&u).copied() {
            | Some(t) => Ok(t),
            | None => match self.subst.solved.0.get(&u).copied() {
                | Some(t) => Ok(t),
                | None => Err(self.subst.unsolved[&u]),
            },
        }
    }

    pub fn resolve_type_shallow(&self, t: Ty) -> Ty {
        self.subst.solved.resolve_type_shallow(self.db, t)
    }

    pub fn resolve_type_fully(&self, t: Ty) -> Ty {
        self.subst.solved.resolve_type_fully(self.db, t)
    }

    pub fn resolve_generalized_type_fully(&self, t: GeneralizedType) -> GeneralizedType {
        self.subst.solved.resolve_generalized_type_fully(self.db, t)
    }

    pub fn resolve_constraint_fully(&self, c: Constraint) -> Constraint {
        self.subst.solved.resolve_constraint_fully(self.db, c)
    }
}

impl UnifyBindings {
    pub fn resolve_type_shallow(&self, db: &dyn Db, t: Ty) -> Ty {
        match t.kind(db) {
            | TyKind::Unknown(u, _) => match self.0.get(u).copied() {
                | Some(t) => self.resolve_type_shallow(db, t),
                | None => t,
            },
            | _ => t,
        }
    }

    pub fn resolve_type_fully(&self, db: &dyn Db, t: Ty) -> Ty {
        t.fold(db, &mut |t| match t.kind(db) {
            | TyKind::Unknown(u, _) => match self.0.get(u).copied() {
                | Some(t) => self.resolve_type_fully(db, t),
                | None => t,
            },
            | _ => t,
        })
    }

    pub fn resolve_generalized_type_fully(&self, db: &dyn Db, t: GeneralizedType) -> GeneralizedType {
        match t {
            | GeneralizedType::Mono(t) => GeneralizedType::Mono(self.resolve_type_fully(db, t)),
            | GeneralizedType::Poly(vars, t) => GeneralizedType::Poly(vars, self.resolve_type_fully(db, t)),
        }
    }

    pub fn resolve_constraint_fully(&self, db: &dyn Db, c: Constraint) -> Constraint {
        Constraint {
            trait_id: c.trait_id,
            args: c.args.iter().map(|&t| self.resolve_type_fully(db, t)).collect(),
        }
    }
}

impl ra_ap_stdx::hash::NoHashHashable for Unknown {
}
