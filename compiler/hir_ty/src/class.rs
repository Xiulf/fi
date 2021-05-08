use crate::db::HirDatabase;
use crate::ty::{Constraint, DebruijnIndex, Ty, TyKind, TypeVar};
use hir_def::id::{ClassId, InstanceId};
use rustc_hash::FxHashMap;
use std::sync::Arc;

#[derive(Debug, PartialEq, Eq)]
pub struct Class {
    pub id: ClassId,
    pub vars: Box<[Ty]>,
    pub fundeps: Box<[FunDep]>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct FunDep {
    pub determiners: Box<[TypeVar]>,
    pub determined: Box<[TypeVar]>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Instances {
    matchers: Box<[InstanceMatcher]>,
}

#[derive(Debug, PartialEq, Eq)]
struct InstanceMatcher {
    types: Box<[Ty]>,
    constraints: Box<[InstanceConstraint]>,
    instance: Option<InstanceId>,
}

#[derive(Debug, PartialEq, Eq)]
struct InstanceConstraint {
    constraint: Constraint,
    constraints: Box<[InstanceConstraint]>,
    instance: Option<InstanceId>,
}

pub(crate) fn lower_class_query(db: &dyn HirDatabase, id: ClassId) -> Arc<Class> {
    let data = db.class_data(id);
    let type_map = data.type_map();
    let vars = data
        .vars
        .iter()
        .enumerate()
        .map(|(i, &var)| {
            let debruijn = DebruijnIndex::new(i as u32);
            let type_var = TypeVar::new(debruijn);

            (&type_map[var].name, type_var)
        })
        .collect::<FxHashMap<_, _>>();

    let fundeps = data
        .fundeps
        .iter()
        .map(|dep| FunDep {
            determiners: dep.determiners.iter().map(|n| vars[n]).collect(),
            determined: dep.determined.iter().map(|n| vars[n]).collect(),
        })
        .collect();

    let vars = vars.into_iter().map(|(_, _)| TyKind::Error.intern(db)).collect();

    Arc::new(Class { id, vars, fundeps })
}
