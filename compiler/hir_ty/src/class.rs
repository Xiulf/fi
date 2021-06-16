use crate::db::HirDatabase;
use crate::display::HirDisplay;
use crate::infer::InferenceContext;
use crate::lower::InstanceLowerResult;
use crate::ty::{Constraint, Ty, TyKind, TypeVar, Unknown};
use hir_def::arena::{Arena, Idx};
use hir_def::id::{ClassId, InstanceId, Lookup, TypedDefId};
use hir_def::resolver::Resolver;
use rustc_hash::{FxHashMap, FxHashSet};
use std::collections::BTreeMap;
use std::sync::Arc;

#[derive(Debug, PartialEq, Eq)]
pub struct Class {
    pub id: ClassId,
    pub vars: Box<[Ty]>,
    pub fundeps: Box<[FunDep]>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
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
    deps: Box<[FunDep]>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct InstanceMatchResult {
    pub instance: InstanceId,
    pub subst: FxHashMap<Unknown, Ty>,
}

#[derive(Default, Debug, PartialEq, Eq)]
pub struct ClassEnv {
    entries: Arena<ClassEnvEntry>,
    current: Option<ClassEnvScope>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct ClassEnvEntry {
    parent: Option<ClassEnvScope>,
    ctnt: Constraint,
    is_method: bool,
}

pub type ClassEnvScope = Idx<ClassEnvEntry>;

#[derive(Debug, PartialEq, Eq)]
pub struct ClassEnvMatchResult {
    pub scope: ClassEnvScope,
    pub subst: FxHashMap<Unknown, Ty>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Matched<T> {
    Match(T),
    Apart,
    Unknown,
}

impl Instances {
    pub(crate) fn instances_query(db: &dyn HirDatabase, class: ClassId) -> Arc<Instances> {
        let loc = class.lookup(db.upcast());
        let lower = db.lower_class(class);
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
            deps: lower.class.fundeps.clone(),
        })
    }

    pub(crate) fn solve_constraint_query(
        db: &dyn HirDatabase,
        constraint: Constraint,
    ) -> Option<Arc<InstanceMatchResult>> {
        let instances = db.instances(constraint.class);

        instances.matches(db, constraint).map(Arc::new)
    }

    pub(crate) fn matches(&self, db: &dyn HirDatabase, ctnt: Constraint) -> Option<InstanceMatchResult> {
        self.matchers
            .iter()
            .filter_map(|m| m.instance.matches(db, &ctnt, &self.deps))
            .next()
    }
}

impl InstanceMatchResult {
    pub(crate) fn apply(&self, icx: &mut InferenceContext) {
        for (&u, &ty) in self.subst.iter() {
            icx.solve_type(u, ty);
        }
    }
}

impl Instance {
    fn matches(&self, db: &dyn HirDatabase, ctnt: &Constraint, deps: &[FunDep]) -> Option<InstanceMatchResult> {
        let mut subst = FxHashMap::default();
        let mut vars = BTreeMap::default();
        let matches = ctnt
            .types
            .iter()
            .zip(self.types.iter())
            .map(|(&ty, &with)| match_type(db, ty, with, &mut subst, &mut vars))
            .collect::<Vec<_>>();

        if !verify(&matches, deps) {
            return None;
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

impl ClassEnv {
    pub fn push(&mut self, ctnt: Constraint, is_method: bool) {
        let scope = self.entries.alloc(ClassEnvEntry {
            ctnt,
            is_method,
            parent: self.current,
        });

        self.current = Some(scope);
    }

    pub fn pop(&mut self) {
        let cur = self.current.unwrap();

        self.current = self.entries[cur].parent;
    }

    pub fn current(&self) -> Option<ClassEnvScope> {
        self.current
    }

    fn in_socpe(&self, scope: Option<ClassEnvScope>) -> impl Iterator<Item = ClassEnvScope> + '_ {
        std::iter::successors(scope, move |&s| self.entries[s].parent)
    }

    pub fn solve(
        &self,
        db: &dyn HirDatabase,
        ctnt: Constraint,
        scope: Option<ClassEnvScope>,
    ) -> Option<ClassEnvMatchResult> {
        self.in_socpe(scope).find_map(|scope| {
            let entry = &self.entries[scope];
            let mut subst = FxHashMap::default();
            let mut vars = BTreeMap::new();

            for (&ty, &with) in ctnt.types.iter().zip(entry.ctnt.types.iter()) {
                if match_type(db, ty, with, &mut subst, &mut vars) != Matched::Match(()) {
                    return None;
                }
            }

            Some(ClassEnvMatchResult { scope, subst })
        })
    }
}

impl std::ops::Index<ClassEnvScope> for ClassEnv {
    type Output = ClassEnvEntry;

    fn index(&self, scope: ClassEnvScope) -> &Self::Output {
        &self.entries[scope]
    }
}

impl ClassEnvEntry {
    pub fn is_method(&self) -> bool {
        self.is_method
    }
}

impl Constraint {
    pub fn can_be_generalized(&self, db: &dyn HirDatabase) -> bool {
        self.types.iter().any(|t| t.can_be_generalized(db))
    }
}

impl Ty {
    pub fn can_be_generalized(self, db: &dyn HirDatabase) -> bool {
        match self.lookup(db) {
            | TyKind::Unknown(_) => true,
            | _ => false,
        }
    }
}

fn verify(matches: &[Matched<()>], deps: &[FunDep]) -> bool {
    let expected = (0..matches.len()).collect::<FxHashSet<_>>();
    let initial_set = matches
        .iter()
        .enumerate()
        .filter(|(_, m)| matches!(m, Matched::Match(_)))
        .map(|(i, _)| i)
        .collect::<FxHashSet<_>>();

    fn until_fixed_point(deps: &[FunDep], a: FxHashSet<usize>) -> FxHashSet<usize> {
        let b = apply_all(deps, a.clone());

        if b == a {
            b
        } else {
            until_fixed_point(deps, b)
        }
    }

    fn apply_all(deps: &[FunDep], s: FxHashSet<usize>) -> FxHashSet<usize> {
        deps.iter().rfold(s, |s, dep| {
            if dep
                .determiners
                .iter()
                .map(|d| d.debruijn().depth() as usize)
                .collect::<FxHashSet<_>>()
                .is_subset(&s)
            {
                s.into_iter()
                    .chain(dep.determined.iter().map(|d| d.debruijn().depth() as usize))
                    .collect()
            } else {
                s
            }
        })
    }

    until_fixed_point(deps, initial_set) == expected
}

fn match_type(
    db: &dyn HirDatabase,
    ty: Ty,
    with: Ty,
    subst: &mut FxHashMap<Unknown, Ty>,
    vars: &mut BTreeMap<TypeVar, Ty>,
) -> Matched<()> {
    match (ty.lookup(db), with.lookup(db)) {
        | (_, TyKind::Unknown(_)) => {
            unreachable!()
        },
        | (TyKind::Error, _) | (_, TyKind::Error) => Matched::Match(()),
        | (TyKind::Unknown(u), _) => {
            subst.insert(u, with);
            Matched::Unknown
        },
        | (TyKind::Skolem(s1, _), TyKind::Skolem(s2, _)) if s1 == s2 => Matched::Match(()),
        | (TyKind::Skolem(_, _), _) | (_, TyKind::Skolem(_, _)) => Matched::Unknown,
        | (_, TyKind::TypeVar(tv)) => {
            vars.insert(tv, ty);
            Matched::Match(())
        },
        | (TyKind::Figure(c1), TyKind::Figure(c2)) if c1 == c2 => Matched::Match(()),
        | (TyKind::Symbol(c1), TyKind::Symbol(c2)) if c1 == c2 => Matched::Match(()),
        | (TyKind::Ctor(c1), TyKind::Ctor(c2)) if c1 == c2 => Matched::Match(()),
        | (TyKind::Tuple(t1), TyKind::Tuple(t2)) if t1.len() == t2.len() => t1
            .iter()
            .zip(t2.iter())
            .map(|(t1, t2)| match_type(db, *t1, *t2, subst, vars))
            .fold(Matched::Match(()), Matched::then),
        | (TyKind::App(a1, a2), TyKind::App(b1, b2)) => {
            match_type(db, a1, b1, subst, vars).then(match_type(db, a2, b2, subst, vars))
        },
        | (_, _) => Matched::Apart,
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

impl Matched<()> {
    fn then(self, other: Self) -> Self {
        match (self, other) {
            | (Matched::Match(_), Matched::Match(_)) => Matched::Match(()),
            | (Matched::Apart, _) | (_, Matched::Apart) => Matched::Apart,
            | (_, _) => Matched::Unknown,
        }
    }

    fn cast<U>(self) -> Matched<U> {
        match self {
            | Matched::Match(_) => unreachable!(),
            | Matched::Apart => Matched::Apart,
            | Matched::Unknown => Matched::Unknown,
        }
    }
}

impl<T> Matched<T> {
    fn map<U>(self, f: impl FnOnce(T) -> U) -> Matched<U> {
        match self {
            | Matched::Match(t) => Matched::Match(f(t)),
            | Matched::Apart => Matched::Apart,
            | Matched::Unknown => Matched::Unknown,
        }
    }
}
