use diagnostics::Diagnostics;
use hir_def::display::HirDisplay;
use ra_ap_stdx::hash::NoHashHashMap;

use crate::ctx::Ctx;
use crate::diagnostics::{RecursiveType, TypeMismatch};
use crate::ty::{Constraint, GeneralizedType, Ty, TyKind, Unknown};
use crate::{Db, TyOrigin};

const RECURSION_LIMIT: u32 = 32;

#[derive(Default, Debug)]
pub struct Substitution {
    pub(crate) solved: UnifyBindings,
    pub(crate) unsolved: NoHashHashMap<Unknown, (UnkLevel, Ty)>,
    next_unknown: u32,
}

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct UnifyBindings(pub NoHashHashMap<Unknown, Ty>);

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
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
    pub fn fresh_unknown(&mut self) -> Unknown {
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

    pub fn fresh_lifetime(&mut self, level: UnkLevel) -> Unknown {
        let u = self.fresh_unknown();
        let kind = self.lifetime_kind();
        self.subst.unsolved.insert(u, (level, kind));
        u
    }

    pub fn coerce(&mut self, ty: Ty, expected: Ty, origin: TyOrigin) {
        let ty = self.resolve_type_shallow(ty);

        if let TyKind::Never = ty.kind(self.db) {
            return;
        }

        self.unify_types(ty, expected, origin);
    }

    pub fn unify_generalized_types(
        &mut self,
        t1: &GeneralizedType,
        t2: GeneralizedType,
        mut constraints: Vec<Constraint>,
        origin: TyOrigin,
    ) -> GeneralizedType {
        match (t1, t2) {
            | (GeneralizedType::Mono(t1), GeneralizedType::Mono(t2)) => {
                self.unify_types(*t1, t2, origin);
                self.result.constraints.append(&mut constraints);
                GeneralizedType::Mono(*t1)
            },
            | (GeneralizedType::Poly(v1, t1), GeneralizedType::Poly(v2, t2)) if v1.len() < v2.len() => {
                let t2 = GeneralizedType::Poly(v2, t2);
                let (t2, mut constraints) = self.instantiate(t2, constraints, None, true);
                self.unify_types(*t1, t2, origin);
                self.result.constraints.append(&mut constraints);
                GeneralizedType::Poly(v1.clone(), *t1)
            },
            | (GeneralizedType::Poly(v1, t1), GeneralizedType::Poly(v2, t2)) if v1.len() == v2.len() => {
                // TODO: check vars
                self.unify_types(*t1, t2, origin);
                self.result.constraints.append(&mut constraints);
                GeneralizedType::Poly(v1.clone(), *t1)
            },
            | (GeneralizedType::Mono(t1), t2) => {
                let (t2, mut constraints) = self.instantiate(t2, constraints, None, true);
                self.unify_types(*t1, t2, origin);
                self.result.constraints.append(&mut constraints);
                GeneralizedType::Mono(*t1)
            },
            | (GeneralizedType::Poly(vars, t1), GeneralizedType::Mono(t2)) => {
                self.unify_types(*t1, t2, origin);
                self.result.constraints.append(&mut constraints);
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

    pub(crate) fn unify(&mut self, t1: Ty, t2: Ty) -> UnifyResult {
        let bindings = unsafe { std::mem::transmute(&mut self.subst.solved) };
        self.unify_into(t1, t2, bindings)
    }

    pub fn unify_into(&self, t1: Ty, t2: Ty, bindings: &mut UnifyBindings) -> UnifyResult {
        tracing::trace!("unify_into({}, {})", t1.display(self.db), t2.display(self.db));
        match (t1.kind(self.db), t2.kind(self.db)) {
            | (TyKind::Error, _) | (_, TyKind::Error) => UnifyResult::Ok,
            | (TyKind::Unknown(u1, _), TyKind::Unknown(u2, _)) if u1 == u2 => UnifyResult::Ok,
            | (TyKind::Literal(l1), TyKind::Literal(l2)) if l1 == l2 => UnifyResult::Ok,
            | (TyKind::Primitive(p1), TyKind::Primitive(p2)) if p1 == p2 => UnifyResult::Ok,
            | (TyKind::Var(v1), TyKind::Var(v2)) if v1 == v2 => UnifyResult::Ok,
            | (TyKind::Ctor(c1), TyKind::Ctor(c2)) if c1 == c2 => UnifyResult::Ok,
            // | (TyKind::Unknown(_, true), TyKind::Var(_)) => UnifyResult::Ok,
            // | (TyKind::Var(_), TyKind::Unknown(_, true)) => UnifyResult::Ok,
            | (TyKind::Unknown(u, false), _) => self.unify_unknown(*u, t1, t2, bindings),
            | (_, TyKind::Unknown(u, false)) => self.unify_unknown(*u, t2, t1, bindings),
            | (TyKind::Ref(u1, t1), TyKind::Ref(u2, t2)) if u1 == u2 => self.unify_into(*t1, *t2, bindings),
            | (TyKind::Ref(u1, t1), TyKind::Ref(u2, t2)) => {
                let ut1 = Ty::new(self.db, TyKind::Unknown(*u1, false));
                let ut2 = Ty::new(self.db, TyKind::Unknown(*u2, false));
                self.unify_unknown(*u1, ut1, ut2, bindings)
                    .and(self.unify_into(*t1, *t2, bindings))
            },
            | (TyKind::App(a_base, a_args), TyKind::App(b_base, b_args)) if a_args.len() == b_args.len() => self
                .unify_into(*a_base, *b_base, bindings)
                .and(self.unify_all(a_args.iter(), b_args.iter(), bindings)),
            | (TyKind::Func(a), TyKind::Func(b)) => {
                if a.params.len() != b.params.len() {
                    if !(a.is_varargs && b.params.len() >= a.params.len())
                        && !(b.is_varargs && a.params.len() >= b.params.len())
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

    pub fn unify_all<'a, 'b>(
        &self,
        mut a: impl Iterator<Item = &'a Ty>,
        mut b: impl Iterator<Item = &'b Ty>,
        bindings: &mut UnifyBindings,
    ) -> UnifyResult {
        let mut res = UnifyResult::Ok;

        while let (Some(&a), Some(&b)) = (a.next(), b.next()) {
            res = res.and(self.unify_into(a, b, bindings));
        }

        res
    }

    fn unify_unknown(&self, u: Unknown, t1: Ty, t2: Ty, bindings: &mut UnifyBindings) -> UnifyResult {
        match self.find_binding(u, bindings) {
            | Ok(t) => self.unify_into(t, t2, bindings),
            | Err((level, kind)) => {
                let b = bindings.resolve_type_shallow(self.db, t2);
                let b = self.resolve_type_shallow(b);

                if t1 == b {
                    return UnifyResult::Ok;
                }

                if self.occurs(u, level, b, RECURSION_LIMIT, bindings) {
                    return UnifyResult::RecursiveType(b);
                }

                let kind_result = self.check_kind(b, kind, &mut UnifyBindings::default());

                if kind_result != UnifyResult::Ok {
                    // return kind_result;
                }

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
            // | TyKind::App(_, args) if args.len() == 1 => {
            //     if let TyKind::Primitive(prim) = args[0].kind(db) {
            //         let ctor = match prim {
            //             | PrimitiveType::Integer(i) => db.type_cache().ctor_for_int_kind(*i),
            //             | PrimitiveType::Float(f) => db.type_cache().ctor_for_float_kind(*f),
            //         };

            //         if let Some(ctor) = ctor {
            //             return Ty::new(db, TyKind::Ctor(ctor));
            //         }
            //     }

            //     t
            // },
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
