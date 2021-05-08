use super::InferenceContext;
use crate::display::HirDisplay;
use crate::ty::*;
use rustc_hash::FxHashMap;

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct UnkLevel(Vec<Unknown>);

#[derive(Default, Clone)]
pub(super) struct Substitution {
    next_unknown: u32,
    tys: FxHashMap<Unknown, Ty>,
    unsolved: FxHashMap<Unknown, (UnkLevel, Ty)>,
}

impl Substitution {
    pub fn unsolved(&self, u: Unknown) -> &(UnkLevel, Ty) {
        &self.unsolved[&u]
    }
}

impl InferenceContext<'_> {
    pub fn fresh_unknown(&mut self) -> Unknown {
        let u = Unknown::from_raw(self.subst.next_unknown);
        let _ = self.subst.next_unknown += 1;

        u
    }

    pub fn fresh_type(&mut self) -> Ty {
        let t1 = Unknown::from_raw(self.subst.next_unknown + 0);
        let t2 = Unknown::from_raw(self.subst.next_unknown + 1);
        let kind_type = self.lang_type("type-kind");

        self.subst.next_unknown += 2;
        self.subst.unsolved.insert(t1, (UnkLevel::from(t1), kind_type));
        self.subst.unsolved.insert(t2, (UnkLevel::from(t2), t1.to_ty(self.db)));

        t2.to_ty(self.db)
    }

    pub fn fresh_type_with_kind(&mut self, kind: Ty) -> Ty {
        let t = Unknown::from_raw(self.subst.next_unknown);

        self.subst.next_unknown += 1;
        self.subst.unsolved.insert(t, (UnkLevel::from(t), kind));

        t.to_ty(self.db)
    }

    pub fn fresh_kind(&mut self) -> Ty {
        self.fresh_type_with_kind(self.lang_type("type-kind"))
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

    pub fn unify_types(&mut self, t1: Ty, t2: Ty) -> bool {
        let t1 = self.subst_type(t1);
        let t2 = self.subst_type(t2);

        match (t1.lookup(self.db), t2.lookup(self.db)) {
            | (TyKind::Error, _) | (_, TyKind::Error) => true,
            | (TyKind::Unknown(u1), TyKind::Unknown(u2)) => u1 == u2,
            | (TyKind::Unknown(u), _) => {
                self.solve_type(u, t2);
                true
            },
            | (_, TyKind::Unknown(u)) => {
                self.solve_type(u, t1);
                true
            },
            | (TyKind::Skolem(c1, _), TyKind::Skolem(c2, _)) => c1 == c2,
            | (TyKind::TypeVar(c1), TyKind::TypeVar(c2)) => c1 == c2,
            | (TyKind::Figure(c1), TyKind::Figure(c2)) => c1 == c2,
            | (TyKind::Symbol(c1), TyKind::Symbol(c2)) => c1 == c2,
            | (TyKind::Ctor(c1), TyKind::Ctor(c2)) => c1 == c2,
            | (TyKind::App(a1, a2), TyKind::App(b1, b2)) => self.unify_types(a1, b1) && self.unify_types(a2, b2),
            | (TyKind::ForAll(k1, t1), TyKind::ForAll(k2, t2)) => {
                let skolem = self.enter_universe();
                let sk1 = self.skolemize(skolem, k1, t1);
                let sk2 = self.skolemize(skolem, k2, t2);
                let res = self.unify_types(sk1, sk2);

                self.exit_universe();
                res
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
