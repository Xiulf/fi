use std::sync::Arc;

use arena::{Arena, Idx};
use hir_def::id::{ClassId, Lookup, MemberId};
use rustc_hash::{FxHashMap, FxHashSet};

use crate::db::HirDatabase;
use crate::infer::InferenceContext;
use crate::info::{CtntInfo, ToInfo, TyId, TyInfo, TySource, TypeVars, Types, Unknown};
use crate::lower::MemberLowerResult;
use crate::ty::{Constraint, Ty, TyKind, TypeVar, WhereClause};

#[derive(Debug, PartialEq, Eq)]
pub struct Class<T, C> {
    pub id: ClassId,
    pub vars: Box<[T]>,
    pub fundeps: Box<[FunDep]>,
    pub where_clause: WhereClause<C>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunDep {
    pub determiners: Box<[TypeVar]>,
    pub determined: Box<[TypeVar]>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Member<T, C> {
    pub id: MemberId,
    pub class: ClassId,
    pub vars: Box<[T]>,
    pub types: Box<[T]>,
    pub where_clause: WhereClause<C>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Members {
    pub(crate) matchers: Box<[Arc<MemberLowerResult<Ty, Constraint>>]>,
    deps: Box<[FunDep]>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct MemberMatchResult {
    pub member: MemberId,
    pub subst: FxHashMap<Unknown, TyId>,
    pub constraints: Vec<Arc<MemberMatchResult>>,
}

#[derive(Default, Debug, PartialEq, Eq)]
pub struct ClassEnv {
    entries: Arena<ClassEnvEntry>,
    current: Option<ClassEnvScope>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct ClassEnvEntry {
    parent: Option<ClassEnvScope>,
    ctnt: CtntInfo,
    is_method: bool,
}

pub type ClassEnvScope = Idx<ClassEnvEntry>;

#[derive(Debug, PartialEq, Eq)]
pub struct ClassEnvMatchResult {
    pub scope: ClassEnvScope,
    pub subst: FxHashMap<Unknown, TyId>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Matched<T> {
    Match(T),
    Apart,
    Unknown,
}

impl Members {
    pub(crate) fn members_query(db: &dyn HirDatabase, class: ClassId) -> Arc<Members> {
        let loc = class.lookup(db.upcast());
        let lower = db.lower_class(class);
        let mut members = Vec::new();
        let mut priority = FxHashMap::default();

        for lib in db.libs().dependant(loc.module.lib) {
            for (_, module) in db.def_map(lib).modules() {
                for inst in module.scope.members() {
                    let lower = db.lower_member(inst);

                    if lower.member.class == class {
                        priority.insert(lower.member.id, lower.member.priority(db));
                        members.push(lower);
                    }
                }
            }
        }

        members.sort_by_key(|i| priority[&i.member.id]);

        Arc::new(Members {
            matchers: members.into(),
            deps: lower.class.fundeps.clone(),
        })
    }

    pub(crate) fn solve_constraint(
        db: &dyn HirDatabase,
        types: &mut Types,
        type_vars: &mut TypeVars,
        constraint: &CtntInfo,
        src: TySource,
    ) -> Option<Arc<MemberMatchResult>> {
        let members = db.members(constraint.class);

        // log::debug!(
        //     "solve {}{}",
        //     db.class_data(constraint.class).name,
        //     constraint
        //         .types
        //         .iter()
        //         .map(|t| format!(" ({})", t.display(db, types)))
        //         .collect::<Vec<_>>()
        //         .join("")
        // );

        members.matches(db, types, type_vars, constraint, src).map(Arc::new)
    }

    pub(crate) fn matches(
        &self,
        db: &dyn HirDatabase,
        types: &mut Types,
        type_vars: &mut TypeVars,
        ctnt: &CtntInfo,
        src: TySource,
    ) -> Option<MemberMatchResult> {
        self.matchers
            .iter()
            .find_map(|m| m.member.matches(db, types, type_vars, &ctnt, &self.deps, src))
    }
}

impl MemberMatchResult {
    pub(crate) fn apply(&self, icx: &mut InferenceContext) {
        for (&u, &ty) in self.subst.iter() {
            icx.solve_type(u, ty);
        }
    }
}

impl Member<Ty, Constraint> {
    fn matches(
        &self,
        db: &dyn HirDatabase,
        types: &mut Types,
        type_vars: &mut TypeVars,
        ctnt: &CtntInfo,
        deps: &[FunDep],
        src: TySource,
    ) -> Option<MemberMatchResult> {
        let mut subst = FxHashMap::default();
        let mut vars = FxHashMap::default();
        let matches = ctnt
            .types
            .iter()
            .zip(self.types.iter())
            .map(|(&ty, with)| {
                let with = with.to_info(db, types, type_vars, src);
                match_type(types, ty, with, &mut subst, &mut vars)
            })
            .collect::<Vec<_>>();

        // log::debug!("{:#?}", matches);

        if !verify(&matches, deps) || matches.iter().all(|m| m == &Matched::Apart) {
            return None;
        }

        let mut constraints = Vec::with_capacity(self.where_clause.constraints.len());

        for ctnt in self.where_clause.constraints.iter() {
            let ctnt = CtntInfo {
                class: ctnt.class,
                types: ctnt
                    .types
                    .iter()
                    .map(|t| match t.lookup(db) {
                        | TyKind::TypeVar(tv) => match vars.get(&tv) {
                            | Some(ty) => *ty,
                            | None => types.insert(TyInfo::TypeVar(tv), src),
                        },
                        | _ => t.to_info(db, types, type_vars, src),
                    })
                    .collect(),
            };

            match Members::solve_constraint(db, types, type_vars, &ctnt, src) {
                | None => return None,
                | Some(solution) => {
                    constraints.push(solution.clone());
                    constraints.extend(solution.constraints.iter().cloned());
                },
            }
        }

        // @TODO: check if this is always the right thing to do
        for ty in subst.values_mut() {
            *ty = ty.everywhere(false, types, &mut |types, t| match types[t] {
                | TyInfo::TypeVar(v) => match vars.get(&v) {
                    | Some(ty) => *ty,
                    | None => t,
                },
                | _ => t,
            });
        }

        Some(MemberMatchResult {
            member: self.id,
            subst,
            constraints,
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

            score -= self.where_clause.constraints.len() as isize;
            score
        }
    }
}

impl ClassEnv {
    pub fn push(&mut self, ctnt: CtntInfo, is_method: bool) {
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

    fn in_scope(&self, scope: Option<ClassEnvScope>) -> impl Iterator<Item = ClassEnvScope> + '_ {
        std::iter::successors(scope, move |&s| self.entries[s].parent)
    }

    pub fn solve(
        &self,
        _db: &dyn HirDatabase,
        types: &Types,
        ctnt: CtntInfo,
        scope: Option<ClassEnvScope>,
    ) -> Option<ClassEnvMatchResult> {
        self.in_scope(scope).find_map(|scope| {
            let entry = &self.entries[scope];
            let mut subst = FxHashMap::default();
            let mut vars = FxHashMap::default();

            for (&ty, &with) in ctnt.types.iter().zip(entry.ctnt.types.iter()) {
                if match_type(types, ty, with, &mut subst, &mut vars) != Matched::Match(()) {
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
    pub fn ctnt(&self) -> &CtntInfo {
        &self.ctnt
    }

    pub fn is_method(&self) -> bool {
        self.is_method
    }
}

impl CtntInfo {
    pub fn can_be_generalized(&self, types: &Types) -> bool {
        if self.types.is_empty() {
            true
        } else {
            self.types.iter().any(|t| t.can_be_generalized(types))
        }
    }
}

impl TyId {
    pub fn can_be_generalized(self, types: &Types) -> bool {
        match types[self] {
            | TyInfo::Unknown(_) => true,
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
                .map(|d| d.idx() as usize)
                .collect::<FxHashSet<_>>()
                .is_subset(&s)
            {
                s.into_iter()
                    .chain(dep.determined.iter().map(|d| d.idx() as usize))
                    .collect()
            } else {
                s
            }
        })
    }

    until_fixed_point(deps, initial_set) == expected
}

fn match_type(
    types: &Types,
    ty: TyId,
    with: TyId,
    subst: &mut FxHashMap<Unknown, TyId>,
    vars: &mut FxHashMap<TypeVar, TyId>,
) -> Matched<()> {
    match_type_inner(
        types,
        ty,
        with,
        &mut FxHashSet::default(),
        &mut FxHashSet::default(),
        subst,
        vars,
    )
}

fn match_type_inner(
    types: &Types,
    ty: TyId,
    with: TyId,
    ty_skolems: &mut FxHashSet<TypeVar>,
    with_skolems: &mut FxHashSet<TypeVar>,
    subst: &mut FxHashMap<Unknown, TyId>,
    vars: &mut FxHashMap<TypeVar, TyId>,
) -> Matched<()> {
    match (&types[ty], &types[with]) {
        | (_, TyInfo::Unknown(_) | TyInfo::Skolem(..)) => unreachable!(),
        | (TyInfo::Error, _) | (_, TyInfo::Error) => Matched::Match(()),
        | (TyInfo::Skolem(_, _), _) => Matched::Unknown,
        | (TyInfo::TypeVar(a), TyInfo::TypeVar(b))
            if ty_skolems.contains(a) && with_skolems.contains(b) && a.idx() == b.idx() =>
        {
            Matched::Match(())
        },
        | (_, &TyInfo::TypeVar(tv)) => {
            vars.insert(tv, ty);
            Matched::Match(())
        },
        | (&TyInfo::Unknown(u), _) => {
            subst.insert(u, with);
            Matched::Unknown
        },
        // @TODO: this case is not valid
        | (TyInfo::TypeVar(_), _) => Matched::Unknown,
        | (&TyInfo::Figure(c1), &TyInfo::Figure(c2)) if c1 == c2 => Matched::Match(()),
        | (&TyInfo::Symbol(ref c1), &TyInfo::Symbol(ref c2)) if c1 == c2 => Matched::Match(()),
        | (&TyInfo::Ctor(c1), &TyInfo::Ctor(c2)) if c1 == c2 => Matched::Match(()),
        | (&TyInfo::App(a1, ref a2), &TyInfo::App(b1, ref b2)) if a2.len() == b2.len() => {
            match_type_inner(types, a1, b1, ty_skolems, with_skolems, subst, vars).then(
                a2.iter()
                    .zip(b2.iter())
                    .map(|(a2, b2)| match_type_inner(types, *a2, *b2, ty_skolems, with_skolems, subst, vars))
                    .fold(Matched::Match(()), Matched::then),
            )
        },
        | (&TyInfo::ForAll(ref v1, a1, s1), &TyInfo::ForAll(ref v2, a2, s2)) if v1.len() == v2.len() => {
            let mut matched = Matched::Match(());

            for (i, (k1, k2)) in v1.iter().zip(v2.iter()).enumerate() {
                ty_skolems.insert(TypeVar::new(i as u32, s1));
                with_skolems.insert(TypeVar::new(i as u32, s2));
                matched = matched.then(match_type(types, *k1, *k2, subst, vars));
            }

            matched = matched.then(match_type_inner(types, a1, a2, ty_skolems, with_skolems, subst, vars));

            for i in 0..v1.len() as u32 {
                ty_skolems.remove(&TypeVar::new(i, s1));
                with_skolems.remove(&TypeVar::new(i, s2));
            }

            matched
        },
        | (_, _) => Matched::Apart,
    }
}

fn type_score(db: &dyn HirDatabase, ty: Ty) -> isize {
    match ty.lookup(db) {
        | TyKind::TypeVar(_) => 5,
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
        | TyKind::App(a, b) => type_score(db, a) + b.iter().map(|&b| type_score(db, b)).sum::<isize>(),
        // | TyKind::Where(ctnt, ty) => ctnt.types.iter().map(|&t| type_score(db, t)).sum::<isize>() + type_score(db, ty),
        | TyKind::ForAll(k, t, _) => k.iter().map(|&k| type_score(db, k)).sum::<isize>() + type_score(db, t),
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
}
