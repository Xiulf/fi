use super::{ExprOrPatId, InferenceContext};
use crate::info::{Span, TyId, Unknown};
use rustc_hash::FxHashMap;

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct UnkLevel(Vec<Unknown>);

#[derive(Default, Debug, Clone)]
pub(super) struct Substitution {
    next_unknown: u32,
    tys: FxHashMap<Unknown, TyId>,
    unsolved: FxHashMap<Unknown, (UnkLevel, TyId)>,
}

impl Substitution {
    pub fn unsolved(&self, u: Unknown) -> &(UnkLevel, TyId) {
        &self.unsolved[&u]
    }
}

impl InferenceContext<'_> {
    pub fn fresh_type_without_kind(&mut self, span: Span) -> TyId {
        let t1 = Unknown::from_raw(self.subst.next_unknown + 0);
        let t2 = Unknown::from_raw(self.subst.next_unknown + 1);
        let kind_type = self.lang_type("kind-kind");

        self.subst.next_unknown += 2;
        self.subst.unsolved.insert(t1, (UnkLevel::from(t1), kind_type));
        self.subst.unsolved.insert(t2, (UnkLevel::from(t2), t1.to_ty(self.db)));

        t2.to_ty(&mut self.types, span)
    }

    pub fn fresh_type_with_kind(&mut self, kind: Ty) -> Ty {
        let t = Unknown::from_raw(self.subst.next_unknown);

        self.subst.next_unknown += 1;
        self.subst.unsolved.insert(t, (UnkLevel::from(t), kind));

        t.to_ty(self.db)
    }

    pub fn fresh_type(&mut self) -> Ty {
        self.fresh_type_with_kind(self.lang_type("type-kind"))
    }

    pub fn fresh_kind(&mut self) -> Ty {
        self.fresh_type_with_kind(self.lang_type("kind-kind"))
    }

    pub fn solve_type(&mut self, u: Unknown, ty: Ty) {
        self.subst.tys.insert(u, ty);
    }

    pub fn subst_type(&self, ty: Ty) -> Ty {
        ty.everywhere(self.db, &mut |ty| match ty.lookup(self.db) {
            | TyKind::Unknown(u) => match self.subst.tys.get(&u) {
                | None => ty,
                | Some(&t2) => match t2.lookup(self.db) {
                    | TyKind::Unknown(u2) if u == u2 => t2,
                    | _ => self.subst_type(t2),
                },
            },
            | _ => ty,
        })
    }

    pub fn subst_ctnt(&self, ctnt: &Constraint) -> Constraint {
        Constraint {
            class: ctnt.class,
            types: ctnt.types.iter().map(|&t| self.subst_type(t)).collect(),
        }
    }

    pub fn replace_unknowns(&mut self, ty: Ty, with: Ty) -> Ty {
        ty.everywhere(self.db, &mut |ty| match ty.lookup(self.db) {
            | TyKind::Unknown(u) => {
                self.solve_type(u, with);
                with
            },
            | _ => ty,
        })
    }

    pub fn instantiate(&mut self, ty: Ty, id: ExprOrPatId) -> Ty {
        match ty.lookup(self.db) {
            | TyKind::ForAll(kinds, inner) => {
                let us = kinds.iter().map(|&k| self.fresh_type_with_kind(k)).collect::<Vec<_>>();
                let ty = inner.replace_vars(self.db, &us);

                if let ExprOrPatId::ExprId(e) = id {
                    *self.result.instances.entry(e).or_default() = us;
                }

                self.instantiate(ty, id)
            },
            | TyKind::Ctnt(ctnt, inner) => {
                self.constrain(id, ctnt);
                self.instantiate(inner, id)
            },
            | _ => ty,
        }
    }

    pub fn normalize(&mut self, ty: Ty) -> Ty {
        ty.everywhere(self.db, &mut |ty| match ty.lookup(self.db) {
            | TyKind::App(a, b) => match a.lookup(self.db) {
                | TyKind::ForAll(_, a) => a.replace_vars(self.db, &b),
                | _ => ty,
            },
            | _ => ty,
        })
    }

    pub fn generalize(&mut self, ty: Ty) -> Ty {
        let mut ty = self.subst_type(ty);
        let mut unknowns = FxHashMap::default();
        let mut find_unknown = |ty: Ty| match ty.lookup(self.db) {
            | TyKind::Unknown(u) => {
                unknowns.insert(u, self.subst.unsolved(u).1);
            },
            | _ => {},
        };

        ty.everything(self.db, &mut find_unknown);

        for (ctnt, _, _) in &self.constraints {
            for &ty in ctnt.types.iter() {
                ty.everything(self.db, &mut find_unknown);
            }
        }

        for (i, (&u, _)) in unknowns.iter().enumerate() {
            self.solve_type(u, TypeVar::new(i as u32, DebruijnIndex::INNER).to_ty(self.db));
        }

        for (ctnt, _, _) in &self.constraints {
            ty = TyKind::Ctnt(self.subst_ctnt(&ctnt), ty).intern(self.db);
        }

        let kinds = unknowns.into_values().collect();
        let ty = TyKind::ForAll(kinds, ty).intern(self.db);

        self.subst_type(ty)
    }

    pub fn unify_types(&mut self, t1: Ty, t2: Ty) -> bool {
        let t1 = self.subst_type(t1);
        let t2 = self.subst_type(t2);

        match (t1.lookup(self.db), t2.lookup(self.db)) {
            | (TyKind::Error, _) | (_, TyKind::Error) => true,
            | (TyKind::Unknown(u1), TyKind::Unknown(u2)) if u1 == u2 => true,
            | (TyKind::Unknown(u), _) => {
                self.solve_type(u, t2);
                true
            },
            | (_, TyKind::Unknown(u)) => {
                self.solve_type(u, t1);
                true
            },
            | (TyKind::Skolem(c1, k1), TyKind::Skolem(c2, k2)) => c1 == c2 && self.unify_types(k1, k2),
            | (TyKind::TypeVar(c1), TyKind::TypeVar(c2)) => c1 == c2,
            | (TyKind::Figure(c1), TyKind::Figure(c2)) => c1 == c2,
            | (TyKind::Symbol(c1), TyKind::Symbol(c2)) => c1 == c2,
            | (TyKind::Row(..), _) => self.unify_rows(t1, t2),
            | (_, TyKind::Row(..)) => self.unify_rows(t1, t2),
            | (TyKind::Ctor(c1), TyKind::Ctor(c2)) => c1 == c2,
            | (TyKind::Tuple(t1), TyKind::Tuple(t2)) if t1.len() == t2.len() => {
                t1.iter().zip(t2.iter()).all(|(t1, t2)| self.unify_types(*t1, *t2))
            },
            | (TyKind::App(a1, a2), TyKind::App(b1, b2)) if a2.len() == b2.len() => {
                self.unify_types(a1, b1) && a2.iter().zip(b2.iter()).all(|(&a2, &b2)| self.unify_types(a2, b2))
            },
            | (TyKind::Func(a1, a2), TyKind::Func(b1, b2)) if a1.len() == b1.len() => {
                a1.iter().zip(b1.iter()).all(|(&a1, &b1)| self.unify_types(a1, b1)) && self.unify_types(a2, b2)
            },
            | (TyKind::ForAll(k1, t1), TyKind::ForAll(k2, t2)) => {
                let sk1 = self.skolemize(&k1, t1);
                let sk2 = self.skolemize(&k2, t2);

                self.unify_types(sk1, sk2)
            },
            | (TyKind::ForAll(kinds, ty), _) => {
                let sk = self.skolemize(&kinds, ty);

                self.unify_types(sk, t2)
            },
            | (_, TyKind::ForAll(_, _)) => self.unify_types(t2, t1),
            | (TyKind::Ctnt(c1, t1), TyKind::Ctnt(c2, t2)) if c1.class == c2.class => {
                c1.types
                    .iter()
                    .zip(c2.types.iter())
                    .all(|(&t1, &t2)| self.unify_types(t1, t2))
                    && self.unify_types(t1, t2)
            },
            | (_, _) => false,
        }
    }

    fn unify_rows(&mut self, t1: Ty, t2: Ty) -> bool {
        let (matches, (lhs, rhs)) = Ty::align_rows_with(self.db, |t1, t2| self.unify_types(t1, t2), t1, t2);

        matches.into_iter().all(std::convert::identity) && self.unify_tails(lhs, rhs)
    }

    fn unify_tails(&mut self, (f1, t1): (List<Field>, Option<Ty>), (f2, t2): (List<Field>, Option<Ty>)) -> bool {
        match (t1.map(|t| t.lookup(self.db)), t2.map(|t| t.lookup(self.db))) {
            | (Some(TyKind::Unknown(u)), _) if f1.is_empty() => {
                self.solve_type(u, TyKind::Row(f2, t2).intern(self.db));
                true
            },
            | (_, Some(TyKind::Unknown(u))) if f2.is_empty() => {
                self.solve_type(u, TyKind::Row(f1, t1).intern(self.db));
                true
            },
            | (None, None) => f1.is_empty() && f2.is_empty(),
            | (Some(TyKind::TypeVar(v1)), Some(TyKind::TypeVar(v2))) => v1 == v2 && f1.is_empty() && f2.is_empty(),
            | (Some(TyKind::Skolem(_, s1)), Some(TyKind::Skolem(_, s2))) => s1 == s2 && f1.is_empty() && f2.is_empty(),
            | (Some(TyKind::Unknown(u1)), Some(TyKind::Unknown(u2))) if u1 != u2 => {
                unimplemented!();
            },
            | (_, _) => false,
        }
    }
}

impl PartialOrd for UnkLevel {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        if self.0.is_empty() && other.0.is_empty() {
            Some(std::cmp::Ordering::Equal)
        } else if other.0.is_empty() {
            Some(std::cmp::Ordering::Less)
        } else if self.0.is_empty() {
            Some(std::cmp::Ordering::Greater)
        } else {
            self.0.partial_cmp(&other.0)
        }
    }
}

impl Ord for UnkLevel {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.partial_cmp(other).unwrap()
    }
}

impl From<Unknown> for UnkLevel {
    fn from(u: Unknown) -> Self {
        Self(vec![u])
    }
}
