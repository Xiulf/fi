use crate::db::HirDatabase;
use crate::display::HirDisplay;
use crate::infer::InferenceContext;
use crate::lower::diagnostics::LowerDiagnostic;
use crate::lower::InstanceLowerResult;
use crate::ty::{Constraint, Ty, TyKind, TypeVar, Unknown};
use hir_def::id::{ClassId, InstanceId, Lookup, TypedDefId};
use hir_def::resolver::Resolver;
use rustc_hash::{FxHashMap, FxHasher};
use std::collections::BTreeMap;
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
pub struct Instance {
    pub id: InstanceId,
    pub class: ClassId,
    pub vars: Box<[Ty]>,
    pub types: Box<[Ty]>,
    pub constraints: Box<[Constraint]>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Instances {
    pub(crate) matchers: Box<[Arc<InstanceLowerResult>]>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct InstanceMatchResult {
    pub instance: InstanceId,
    pub subst: FxHashMap<Unknown, Ty>,
}

impl Instances {
    pub(crate) fn instances_query(db: &dyn HirDatabase, class: ClassId) -> Arc<Instances> {
        let loc = class.lookup(db.upcast());
        let mut instances = Vec::new();
        let mut priority = FxHashMap::default();

        for lib in db.libs().dependant(loc.module.lib) {
            for (_, module) in db.def_map(lib).modules() {
                for inst in module.scope.instances() {
                    let lower = db.lower_instance(inst);

                    if lower.instance.class == class {
                        priority.insert(lower.instance.id, lower.instance.priority(db));
                        instances.push(lower);
                    }
                }
            }
        }

        instances.sort_by_key(|i| priority[&i.instance.id]);

        Arc::new(Instances {
            matchers: instances.into(),
        })
    }

    pub(crate) fn solve_constraint_query(
        db: &dyn HirDatabase,
        constraint: Constraint,
    ) -> Option<Arc<InstanceMatchResult>> {
        let instances = db.instances(constraint.class);

        instances.matches(db, &constraint.types).map(Arc::new)
    }

    pub(crate) fn matches(&self, db: &dyn HirDatabase, tys: &[Ty]) -> Option<InstanceMatchResult> {
        self.matchers.iter().filter_map(|m| m.instance.matches(db, tys)).next()
    }
}

impl Instance {
    fn matches(&self, db: &dyn HirDatabase, tys: &[Ty]) -> Option<InstanceMatchResult> {
        let mut subst = FxHashMap::default();
        let mut vars = BTreeMap::default();

        for (&ty, &with) in tys.iter().zip(self.types.iter()) {
            if !match_type(db, ty, with, &mut subst, &mut vars) {
                return None;
            }
        }

        for ctnt in self.constraints.iter().cloned() {
            if let None = db.solve_constraint(ctnt) {
                return None;
            }
        }

        // @TODO: check if this is always the right thing to do
        for ty in subst.values_mut() {
            *ty = ty.everywhere(db, &mut |t| match t.lookup(db) {
                | TyKind::TypeVar(v) => match vars.get(&v) {
                    | Some(ty) => *ty,
                    | None => t,
                },
                | _ => t,
            });
        }

        Some(InstanceMatchResult {
            instance: self.id,
            subst,
        })
    }

    fn priority(&self, db: &dyn HirDatabase) -> isize {
        let attrs = db.attrs(self.id.into());

        if attrs.by_key("default").exists() {
            isize::min_value()
        } else {
            let mut score = self.vars.len() as isize * 10;

            for &ty in self.types.iter() {
                score += type_score(db, ty);
            }

            score -= self.constraints.len() as isize;
            score
        }
    }
}

fn match_type(
    db: &dyn HirDatabase,
    ty: Ty,
    with: Ty,
    subst: &mut FxHashMap<Unknown, Ty>,
    vars: &mut BTreeMap<TypeVar, Ty>,
) -> bool {
    match (ty.lookup(db), with.lookup(db)) {
        | (_, TyKind::Skolem(..)) | (_, TyKind::Unknown(_)) | (TyKind::Skolem(..), _) => {
            unreachable!()
        },
        | (TyKind::Error, _) | (_, TyKind::Error) => true,
        | (TyKind::Unknown(u), _) => {
            subst.insert(u, with);
            true
        },
        | (_, TyKind::TypeVar(tv)) => {
            vars.insert(tv, ty);
            true
        },
        | (TyKind::Figure(c1), TyKind::Figure(c2)) => c1 == c2,
        | (TyKind::Symbol(c1), TyKind::Symbol(c2)) => c1 == c2,
        | (TyKind::Ctor(c1), TyKind::Ctor(c2)) => c1 == c2,
        | (TyKind::Tuple(t1), TyKind::Tuple(t2)) if t1.len() == t2.len() => t1
            .iter()
            .zip(t2.iter())
            .all(|(t1, t2)| match_type(db, *t1, *t2, subst, vars)),
        | (TyKind::App(a1, a2), TyKind::App(b1, b2)) => {
            match_type(db, a1, b1, subst, vars) && match_type(db, a2, b2, subst, vars)
        },
        | (_, _) => false,
    }
}

fn type_score(db: &dyn HirDatabase, ty: Ty) -> isize {
    match ty.lookup(db) {
        | TyKind::TypeVar(_) => 5,
        | TyKind::Skolem(_, t) => type_score(db, t),
        | TyKind::Row(fields, tail) => {
            let mut score = 0;

            for field in fields.iter() {
                score += type_score(db, field.ty);
            }

            if let Some(tail) = tail {
                score += type_score(db, tail);
            }

            score
        },
        | TyKind::Tuple(tys) => tys.iter().map(|&ty| type_score(db, ty)).sum(),
        | TyKind::App(a, b) => type_score(db, a) + type_score(db, b),
        | TyKind::Ctnt(ctnt, ty) => ctnt.types.iter().map(|&t| type_score(db, t)).sum::<isize>() + type_score(db, ty),
        | TyKind::ForAll(k, t) => type_score(db, k) + type_score(db, t),
        | _ => -5,
    }
}
