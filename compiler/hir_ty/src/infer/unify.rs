use rustc_hash::FxHashMap;

use super::diagnostics::{CtntExpected, CtntFound};
use super::{ExprOrPatId, InferenceContext};
use crate::info::{CtntInfo, FieldInfo, TyId, TyInfo, TySource, Types, Unknown};
use crate::ty::{List, TypeVar, WhereClause};

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

impl Substitution {
    pub fn subst_type(&self, types: &mut Types, ty: TyId) -> TyId {
        ty.everywhere(true, types, &mut |types, ty| match types[ty] {
            | TyInfo::Unknown(u) => match self.tys.get(&u) {
                | None => ty,
                | Some(&t2) => match types[t2] {
                    | TyInfo::Unknown(u2) if u == u2 => t2,
                    | _ => self.subst_type(types, t2),
                },
            },
            | _ => ty,
        })
    }

    pub fn subst_ctnt(&self, types: &mut Types, ctnt: &CtntInfo) -> CtntInfo {
        CtntInfo {
            class: ctnt.class,
            types: ctnt.types.iter().map(|&t| self.subst_type(types, t)).collect(),
        }
    }
}

impl InferenceContext<'_> {
    pub fn fresh_type_without_kind(&mut self, src: TySource) -> TyId {
        let t1 = Unknown::from_raw(self.subst.next_unknown + 0);
        let t2 = Unknown::from_raw(self.subst.next_unknown + 1);
        let ty_kind = self.type_kind(src);

        self.subst.next_unknown += 2;
        self.subst.unsolved.insert(t1, (UnkLevel::from(t1), ty_kind));
        self.subst
            .unsolved
            .insert(t2, (UnkLevel::from(t2), t1.to_ty(&mut self.types, src)));

        t2.to_ty(&mut self.types, src)
    }

    pub fn fresh_type_with_kind(&mut self, kind: TyId, src: TySource) -> TyId {
        let t = Unknown::from_raw(self.subst.next_unknown);

        self.subst.next_unknown += 1;
        self.subst.unsolved.insert(t, (UnkLevel::from(t), kind));

        t.to_ty(&mut self.types, src)
    }

    pub fn fresh_type(&mut self, src: TySource) -> TyId {
        let ty_kind = self.type_kind(src);

        self.fresh_type_with_kind(ty_kind, src)
    }

    pub fn solve_type(&mut self, u: Unknown, ty: TyId) {
        let &(_, kind) = self.subst.unsolved(u);

        self.check_kind(ty, kind);
        self.subst.tys.insert(u, ty);
    }

    pub fn replace_unknowns(&mut self, ty: TyId, with: TyId) -> TyId {
        let subst = &mut self.subst;

        ty.everywhere(false, &mut self.types, &mut |types, ty| match types[ty] {
            | TyInfo::Unknown(u) => {
                subst.tys.insert(u, with);
                with
            },
            | _ => ty,
        })
    }

    pub fn instantiate(&mut self, ty: TyId, id: ExprOrPatId) -> TyId {
        match self.types[ty].clone() {
            | TyInfo::ForAll(kinds, inner, scope) => {
                let us = kinds
                    .iter()
                    .map(|&k| {
                        let src = self.types.source(k);
                        self.fresh_type_with_kind(k, src)
                    })
                    .collect::<Vec<_>>();

                let ty = inner.replace_vars(&mut self.types, &us, scope);

                if let ExprOrPatId::ExprId(e) | ExprOrPatId::ExprIdInfix(e, _) = id {
                    self.result.instances.insert(e, us);
                }

                self.instantiate(ty, id)
            },
            | TyInfo::Where(ref where_, inner) => {
                for ctnt in where_.constraints.iter() {
                    self.constrain(CtntExpected::ExprOrPat(id), CtntFound::ExprOrPat(id), ctnt.clone());
                }

                self.instantiate(inner, id)
            },
            | _ => ty,
        }
    }

    pub fn subst_type(&mut self, ty: TyId) -> TyId {
        self.subst.subst_type(&mut self.types, ty)
    }

    pub fn subst_ctnt(&mut self, ctnt: &CtntInfo) -> CtntInfo {
        self.subst.subst_ctnt(&mut self.types, ctnt)
    }

    pub fn occurs(&self, u: Unknown, ty: TyId) -> bool {
        let mut occurs = false;

        ty.everything(&self.types, &mut |ty| match self.types[ty] {
            | TyInfo::Unknown(u2) => occurs |= u == u2,
            | _ => {},
        });

        occurs
    }

    pub fn generalize(&mut self, ty: TyId) -> TyId {
        let src = self.types.source(ty);
        let mut ty = self.subst_type(ty);
        let mut unknowns = FxHashMap::default();
        let mut find_unknown = |ty: TyId| match self.types[ty] {
            | TyInfo::Unknown(u) => {
                unknowns.insert(u, self.subst.unsolved(u).1);
            },
            | _ => {},
        };

        ty.everything(&self.types, &mut find_unknown);

        for (ctnt, _, _, _) in &self.constraints {
            for &ty in ctnt.types.iter() {
                ty.everything(&self.types, &mut find_unknown);
            }
        }

        let scope = self.type_vars.add_scope(unknowns.values().copied().collect());

        for (i, (&u, _)) in unknowns.iter().enumerate() {
            let tv = TypeVar::new(i as u32, scope);
            let ty = self.types.insert(TyInfo::TypeVar(tv), src);

            self.solve_type(u, ty);
        }

        if !self.constraints.is_empty() {
            self.constraints.sort_by_key(|c| c.0.class);
            self.constraints.dedup_by_key(|c| c.0.clone());

            let subst = &self.subst;
            let types = &mut self.types;
            let where_ = WhereClause {
                constraints: self
                    .constraints
                    .iter()
                    .map(|(ctnt, _, _, _)| subst.subst_ctnt(types, ctnt))
                    .collect(),
            };

            ty = self.types.insert(TyInfo::Where(where_, ty), src);
        }

        if unknowns.is_empty() {
            ty
        } else {
            let kinds = unknowns.into_values().collect();
            let ty = self.subst_type(ty);

            self.types.insert(TyInfo::ForAll(kinds, ty, scope), src)
        }
    }

    pub fn unify_types(&mut self, t1: TyId, t2: TyId) -> bool {
        let t1 = self.subst_type(t1);
        let t2 = self.subst_type(t2);

        match (self.types[t1].clone(), self.types[t2].clone()) {
            | (TyInfo::Error, _) | (_, TyInfo::Error) => true,
            | (TyInfo::Unknown(u1), TyInfo::Unknown(u2)) if u1 == u2 => true,
            | (TyInfo::Unknown(u), _) if !self.occurs(u, t2) => {
                self.solve_type(u, t2);
                true
            },
            | (_, TyInfo::Unknown(u)) if !self.occurs(u, t1) => {
                self.solve_type(u, t1);
                true
            },
            | (TyInfo::Skolem(c1, k1), TyInfo::Skolem(c2, k2)) => c1 == c2 && self.unify_types(k1, k2),
            | (TyInfo::TypeVar(c1), TyInfo::TypeVar(c2)) => c1 == c2,
            | (TyInfo::Figure(c1), TyInfo::Figure(c2)) => c1 == c2,
            | (TyInfo::Symbol(c1), TyInfo::Symbol(c2)) => c1 == c2,
            | (TyInfo::Row(..), _) => self.unify_rows(t1, t2),
            | (_, TyInfo::Row(..)) => self.unify_rows(t1, t2),
            | (TyInfo::Ctor(c1), TyInfo::Ctor(c2)) => c1 == c2,
            | (TyInfo::App(a1, a2), TyInfo::App(b1, b2)) if a2.len() == b2.len() => {
                self.unify_types(a1, b1) && a2.iter().zip(b2.iter()).all(|(&a2, &b2)| self.unify_types(a2, b2))
            },
            | (TyInfo::App(_, a2), TyInfo::App(b1, b2)) if a2.len() < b2.len() => {
                let src = self.types.source(t2);
                let c1 = self.types.insert(TyInfo::App(b1, b2[..a2.len()].into()), src);
                let c2 = self.types.insert(TyInfo::App(c1, b2[a2.len()..].into()), src);

                self.unify_types(t1, c2)
            },
            | (TyInfo::App(a1, a2), TyInfo::App(_, b2)) if a2.len() > b2.len() => {
                let src = self.types.source(t1);
                let c1 = self.types.insert(TyInfo::App(a1, a2[..b2.len()].into()), src);
                let c2 = self.types.insert(TyInfo::App(c1, a2[b2.len()..].into()), src);

                self.unify_types(c2, t2)
            },
            | (TyInfo::ForAll(k1, t1, s1), TyInfo::ForAll(k2, t2, s2)) => {
                let sk1 = self.skolemize(&k1, t1, s1);
                let sk2 = self.skolemize(&k2, t2, s2);

                self.unify_types(sk1, sk2)
            },
            | (TyInfo::ForAll(kinds, ty, scope), _) => {
                let sk = self.skolemize(&kinds, ty, scope);

                self.unify_types(sk, t2)
            },
            | (_, TyInfo::ForAll(_, _, _)) => self.unify_types(t2, t1),
            // | (TyInfo::Where(c1, t1), TyInfo::Where(c2, t2)) if c1.class == c2.class => {
            //     c1.types
            //         .iter()
            //         .zip(c2.types.iter())
            //         .all(|(&t1, &t2)| self.unify_types(t1, t2))
            //         && self.unify_types(t1, t2)
            // },
            | (_, _) => false,
        }
    }

    fn unify_rows(&mut self, t1: TyId, t2: TyId) -> bool {
        let safe = unsafe { &*(&self.types as *const Types) };
        let (matches, (lhs, rhs)) = TyId::align_rows_with(safe, |t1, t2| self.unify_types(t1, t2), t1, t2);

        matches.into_iter().all(std::convert::identity) && self.unify_tails(lhs, rhs)
    }

    fn unify_tails(
        &mut self,
        (f1, t1): (List<FieldInfo>, Option<TyId>),
        (f2, t2): (List<FieldInfo>, Option<TyId>),
    ) -> bool {
        match (t1.map(|t| &self.types[t]), t2.map(|t| &self.types[t])) {
            | (Some(&TyInfo::Unknown(u)), _) if f1.is_empty() => {
                let src = self.types.source(t1.unwrap());
                let ty = self.types.insert(TyInfo::Row(f2, t2), src);
                self.solve_type(u, ty);
                true
            },
            | (_, Some(&TyInfo::Unknown(u))) if f2.is_empty() => {
                let src = self.types.source(t2.unwrap());
                let ty = self.types.insert(TyInfo::Row(f1, t1), src);
                self.solve_type(u, ty);
                true
            },
            | (None, None) => f1.is_empty() && f2.is_empty(),
            | (Some(&TyInfo::TypeVar(v1)), Some(&TyInfo::TypeVar(v2))) => v1 == v2 && f1.is_empty() && f2.is_empty(),
            | (Some(&TyInfo::Skolem(_, s1)), Some(&TyInfo::Skolem(_, s2))) => {
                s1 == s2 && f1.is_empty() && f2.is_empty()
            },
            | (Some(&TyInfo::Unknown(u1)), Some(&TyInfo::Unknown(u2))) if u1 != u2 => {
                for f in f1.iter() {
                    if self.occurs(u2, f.ty) {
                        return false;
                    }
                }

                for f in f2.iter() {
                    if self.occurs(u1, f.ty) {
                        return false;
                    }
                }

                let &(_, kind) = self.subst.unsolved(u1);
                let src1 = self.types.source(t1.unwrap());
                let src2 = self.types.source(t2.unwrap());
                let rest = self.fresh_type_with_kind(kind, src1);
                let r1 = self.types.insert(TyInfo::Row(f2, Some(rest)), src1);
                let r2 = self.types.insert(TyInfo::Row(f1, Some(rest)), src2);

                self.solve_type(u1, r1);
                self.solve_type(u2, r2);
                true
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
