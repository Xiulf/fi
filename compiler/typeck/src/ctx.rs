use crate::ty::*;
use std::cmp::Ordering;
use std::collections::HashMap;

pub struct Ctx<'db> {
    crate db: &'db dyn crate::TypeDatabase,
    crate next_ty: u64,
    crate next_skolem: u64,
    crate next_scope: u64,
    crate subst: Substitution,
    crate tys: HashMap<hir::ir::HirId, Ty>,
}

#[derive(PartialEq, Eq, Hash)]
pub struct UnkLevel(Vec<Unknown>);

pub struct Substitution {
    pub tys: HashMap<Unknown, Ty>,
    pub unsolved: HashMap<UnkLevel, Ty>,
}

impl<'db> Ctx<'db> {
    pub fn new(db: &'db dyn crate::TypeDatabase) -> Self {
        Ctx {
            db,
            next_ty: 0,
            next_skolem: 0,
            next_scope: 0,
            subst: Substitution::empty(),
            tys: HashMap::new(),
        }
    }

    crate fn is_func(&self, ty: &Ty) -> bool {
        if let Type::Ctor(id) = **ty {
            id == self.db.lang_items().fn_ty().owner
        } else {
            false
        }
    }

    crate fn is_record(&self, ty: &Ty) -> bool {
        if let Type::Ctor(id) = **ty {
            id == self.db.lang_items().record_ty().owner
        } else {
            false
        }
    }

    crate fn instantiate(&mut self, ty: Ty) -> Ty {
        if let Type::ForAll(vars, ret, _) = &*ty {
            let subst = vars
                .into_iter()
                .map(|(v, k)| {
                    if let Some(k) = k {
                        (v, self.fresh_type_with_kind(ty.span(), k))
                    } else {
                        (v, self.fresh_type(ty.span()))
                    }
                })
                .collect();

            ret.clone().replace_vars(subst)
        } else if let Type::Ctnt(ctnt, ret) = &*ty {
            todo!();
            // self.instantiate(ret.clone())
        } else {
            ty
        }
    }
}

impl Substitution {
    pub fn empty() -> Self {
        Substitution {
            tys: HashMap::new(),
            unsolved: HashMap::new(),
        }
    }
}

impl PartialOrd for UnkLevel {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        if self.0.is_empty() && other.0.is_empty() {
            Some(Ordering::Equal)
        } else if other.0.is_empty() {
            Some(Ordering::Less)
        } else if self.0.is_empty() {
            Some(Ordering::Greater)
        } else {
            self.0.partial_cmp(&other.0)
        }
    }
}

impl Ord for UnkLevel {
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other).unwrap()
    }
}

impl From<Unknown> for UnkLevel {
    fn from(src: Unknown) -> Self {
        UnkLevel(vec![src])
    }
}
