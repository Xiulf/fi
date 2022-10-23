use std::sync::Arc;

use arena::{Arena, Idx};
use base_db::libs::LibId;
use hir_def::id::{ClassId, MemberId};
use rustc_hash::{FxHashMap, FxHashSet};

use crate::db::HirDatabase;
use crate::display::HirDisplay;
use crate::infer::{ExprOrPatId, InferenceContext};
use crate::info::{CtntInfo, FieldInfo, ToInfo, TyId, TyInfo, TySource, TypeOrigin, TypeVars, Types, Unknown};
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
    pub subst: Box<[TyId]>,
    pub constraints: Box<[MatchConstraint]>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MatchConstraint {
    Member(MemberId),
    Env(ClassEnvScope),
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
    pub path: ClassEnvPath,
    pub subst: FxHashMap<Unknown, TyId>,
}

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
#[repr(transparent)]
pub struct ClassEnvPath(u128);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Matched<T> {
    Match(T),
    Apart,
    Unknown,
}

impl Members {
    pub(crate) fn members_query(db: &dyn HirDatabase, lib: LibId, class: ClassId) -> Arc<Members> {
        let lower = db.lower_class(class);
        let mut members = Vec::new();
        let mut priority = FxHashMap::default();

        for lib in db.libs().all_deps(lib) {
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
        env: &ClassEnv,
        types: &mut Types,
        type_vars: &mut TypeVars,
        lib: LibId,
        ctnt: &CtntInfo,
        src: TySource,
    ) -> Option<Arc<MemberMatchResult>> {
        let mut cycles = FxHashSet::default();

        tracing::debug!(
            "solve {}{}",
            db.class_data(ctnt.class).name,
            ctnt.types
                .iter()
                .map(|t| format!(" ({})", t.display(db, types)))
                .collect::<Vec<_>>()
                .join(""),
        );

        Self::solve_constraint_cyclic(db, env, types, type_vars, &mut cycles, lib, ctnt, src)
    }

    #[tracing::instrument(name = "solve", skip_all)]
    fn solve_constraint_cyclic(
        db: &dyn HirDatabase,
        env: &ClassEnv,
        types: &mut Types,
        type_vars: &mut TypeVars,
        cycles: &mut FxHashSet<CtntInfo>,
        lib: LibId,
        ctnt: &CtntInfo,
        src: TySource,
    ) -> Option<Arc<MemberMatchResult>> {
        if cycles.contains(ctnt) {
            return None;
        }

        tracing::debug!(
            "solve {}{}",
            db.class_data(ctnt.class).name,
            ctnt.types
                .iter()
                .map(|t| format!(" ({})", t.display(db, types)))
                .collect::<Vec<_>>()
                .join(""),
        );

        let members = db.members(lib, ctnt.class);
        let _ = cycles.insert(ctnt.clone());
        let res = members
            .matches(db, env, types, type_vars, cycles, lib, ctnt, src)
            .map(Arc::new);

        cycles.remove(ctnt);
        res
    }

    fn matches(
        &self,
        db: &dyn HirDatabase,
        env: &ClassEnv,
        types: &mut Types,
        type_vars: &mut TypeVars,
        cycles: &mut FxHashSet<CtntInfo>,
        lib: LibId,
        ctnt: &CtntInfo,
        src: TySource,
    ) -> Option<MemberMatchResult> {
        self.matchers.iter().find_map(|m| {
            m.member
                .matches(db, env, types, type_vars, cycles, lib, &ctnt, &self.deps, src)
        })
    }
}

impl MemberMatchResult {
    pub(crate) fn apply(&self, ctnt: CtntInfo, icx: &mut InferenceContext) {
        // for (&u, &ty) in self.subst.iter() {
        //     icx.solve_type(u, ty);
        // }

        for (&a, &b) in self.subst.iter().zip(ctnt.types.iter()) {
            if !icx.unify_types(a, b) {
                let id = match icx.types.source(a).1 {
                    | TypeOrigin::ExprIdInfix(id, i) => ExprOrPatId::ExprIdInfix(id, i),
                    | TypeOrigin::ExprId(id) => ExprOrPatId::ExprId(id),
                    | TypeOrigin::PatId(id) => ExprOrPatId::PatId(id),
                    | o => unreachable!("{:?}", o),
                };

                icx.report_mismatch(a, b, id);
            }
        }
    }
}

impl Member<Ty, Constraint> {
    fn matches(
        &self,
        db: &dyn HirDatabase,
        env: &ClassEnv,
        types: &mut Types,
        type_vars: &mut TypeVars,
        cycles: &mut FxHashSet<CtntInfo>,
        lib: LibId,
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
                match_type(db, types, ty, with, &mut subst, &mut vars)
            })
            .collect::<Vec<_>>();

        tracing::debug!(
            "member{}",
            self.types
                .iter()
                .map(|t| format!(" ({})", t.display(db)))
                .collect::<Vec<_>>()
                .join(""),
        );

        tracing::debug!("{:?}, {}", matches, verify(&matches, deps));

        if !verify(&matches, deps) || matches.iter().all(|m| matches!(m, Matched::Apart)) {
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

            let add = !db.class_data(ctnt.class).items.is_empty();

            if let Some(res) = env.solve(db, types, type_vars, &ctnt, env.current(), src) {
                if add {
                    constraints.push(MatchConstraint::Env(res.scope));
                }

                continue;
            }

            if let Some(solution) = Members::solve_constraint_cyclic(db, env, types, type_vars, cycles, lib, &ctnt, src)
            {
                if add {
                    constraints.push(MatchConstraint::Member(solution.member));
                }

                constraints.extend(solution.constraints.iter().copied());
                continue;
            }

            return None;
        }

        let subst = self
            .types
            .iter()
            .map(|ty| {
                let ty = ty.to_info(db, types, type_vars, src);

                ty.everywhere(true, types, &mut |types, t| match types[t] {
                    | TyInfo::TypeVar(v) => match vars.get(&v) {
                        | Some(ty) => *ty,
                        | None => t,
                    },
                    | _ => t,
                })
            })
            .collect();

        // @TODO: check if this is always the right thing to do
        // for ty in subst.values_mut() {
        //     *ty = ty.everywhere(false, types, &mut |types, t| match types[t] {
        //         | TyInfo::TypeVar(v) => match vars.get(&v) {
        //             | Some(ty) => *ty,
        //             | None => t,
        //         },
        //         | _ => t,
        //     });
        // }

        Some(MemberMatchResult {
            member: self.id,
            subst,
            constraints: constraints.into_boxed_slice(),
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

    pub fn index(&self, scope: ClassEnvScope) -> usize {
        self.in_scope(Some(scope)).count() - 1
    }

    fn in_scope(&self, scope: Option<ClassEnvScope>) -> impl Iterator<Item = ClassEnvScope> + '_ {
        std::iter::successors(scope, move |&s| self.entries[s].parent)
    }

    pub fn solve(
        &self,
        db: &dyn HirDatabase,
        types: &mut Types,
        type_vars: &mut TypeVars,
        ctnt: &CtntInfo,
        scope: Option<ClassEnvScope>,
        src: TySource,
    ) -> Option<ClassEnvMatchResult> {
        tracing::debug!(
            "solve env {}{}",
            db.class_data(ctnt.class).name,
            ctnt.types
                .iter()
                .map(|t| format!(" ({})", t.display(db, types)))
                .collect::<Vec<_>>()
                .join(""),
        );

        self.in_scope(scope).find_map(|scope| {
            let entry = &self.entries[scope];

            if entry.ctnt.class != ctnt.class {
                return self.solve_constraints(
                    db,
                    types,
                    type_vars,
                    ctnt,
                    &entry.ctnt,
                    scope,
                    &mut ClassEnvPath(0),
                    src,
                );
            }

            tracing::debug!(
                "member env {}{}",
                db.class_data(entry.ctnt.class).name,
                entry
                    .ctnt
                    .types
                    .iter()
                    .map(|t| format!(" ({})", t.display(db, types)))
                    .collect::<Vec<_>>()
                    .join(""),
            );

            let mut subst = FxHashMap::default();
            let mut vars = FxHashMap::default();
            let matches = ctnt
                .types
                .iter()
                .zip(entry.ctnt.types.iter())
                .map(|(&ty, &with)| match_type_env(db, types, ty, with, &mut subst, &mut vars))
                .collect::<Vec<_>>();

            tracing::debug!("{:?}", matches);

            if matches.iter().any(|m| matches!(m, Matched::Apart)) {
                return None;
            }

            Some(ClassEnvMatchResult {
                scope,
                subst,
                path: ClassEnvPath(0),
            })
        })
    }

    #[tracing::instrument(skip_all)]
    fn solve_constraints(
        &self,
        db: &dyn HirDatabase,
        types: &mut Types,
        type_vars: &mut TypeVars,
        ctnt: &CtntInfo,
        of: &CtntInfo,
        scope: ClassEnvScope,
        path: &mut ClassEnvPath,
        src: TySource,
    ) -> Option<ClassEnvMatchResult> {
        let info = db.lower_class(of.class);
        path.new_layer();

        for c in info.class.where_clause.constraints.iter() {
            let ci = CtntInfo {
                class: c.class,
                types: c
                    .types
                    .iter()
                    .map(|t| {
                        let ty = t.to_info(db, types, type_vars, src);

                        ty.everywhere(true, types, &mut |types, ty| match types[ty] {
                            | TyInfo::TypeVar(tv) if tv.scope() == type_vars.top_scope() => of.types[tv.idx() as usize],
                            | _ => ty,
                        })
                    })
                    .collect(),
            };

            if c.class != ctnt.class {
                if let Some(r) = self.solve_constraints(db, types, type_vars, ctnt, &ci, scope, path, src) {
                    return Some(r);
                }
            }

            tracing::debug!(
                "where {}{}",
                db.class_data(ci.class).name,
                ci.types
                    .iter()
                    .map(|t| format!(" ({})", t.display(db, types)))
                    .collect::<Vec<_>>()
                    .join(""),
            );

            let mut subst = FxHashMap::default();
            let mut vars = FxHashMap::default();
            let matches = ctnt
                .types
                .iter()
                .zip(ci.types.iter())
                .map(|(&ty, &with)| match_type_env(db, types, ty, with, &mut subst, &mut vars))
                .collect::<Vec<_>>();

            tracing::debug!("{:?}", matches);

            if matches.iter().any(|m| matches!(m, Matched::Apart)) {
                path.inc_last();
                continue;
            }

            return Some(ClassEnvMatchResult {
                scope,
                subst,
                path: *path,
            });
        }

        None
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

impl ClassEnvPath {
    const LEN_MASK: u128 = 0x1111;

    pub fn len(self) -> usize {
        (self.0 & Self::LEN_MASK) as usize
    }

    pub fn iter(self) -> ClassEnvPathIter {
        ClassEnvPathIter(self, 0)
    }

    fn new_layer(&mut self) {
        assert!(self.0 & Self::LEN_MASK < 14);
        self.0 += 1;
    }

    fn inc_last(&mut self) {
        self.0 += 1 << (self.len() * 8);
    }
}

pub struct ClassEnvPathIter(ClassEnvPath, usize);

impl Iterator for ClassEnvPathIter {
    type Item = usize;

    fn next(&mut self) -> Option<usize> {
        if self.1 >= self.0.len() {
            return None;
        }

        let val = ((self.0).0 >> ((self.1 + 1) * 8)) & 0x11111111;

        self.1 += 1;

        Some(val as usize)
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

#[tracing::instrument(skip_all)]
fn match_type(
    db: &dyn HirDatabase,
    types: &mut Types,
    ty: TyId,
    with: TyId,
    subst: &mut FxHashMap<Unknown, TyId>,
    vars: &mut FxHashMap<TypeVar, TyId>,
) -> Matched<()> {
    match_type_inner(
        db,
        types,
        ty,
        with,
        &mut FxHashSet::default(),
        &mut FxHashSet::default(),
        subst,
        vars,
    )
}

#[tracing::instrument(skip_all)]
fn match_type_env(
    db: &dyn HirDatabase,
    types: &mut Types,
    ty: TyId,
    with: TyId,
    subst: &mut FxHashMap<Unknown, TyId>,
    vars: &mut FxHashMap<TypeVar, TyId>,
) -> Matched<()> {
    match (&types[ty], &types[with]) {
        | (TyInfo::Unknown(u), _) => {
            subst.insert(*u, with);
            Matched::Unknown
        },
        | (TyInfo::TypeVar(a), TyInfo::TypeVar(b)) if a == b => Matched::Match(()),
        | (TyInfo::TypeVar(_), _) => Matched::Apart,
        | (_, TyInfo::TypeVar(_)) => Matched::Apart,
        | (_, _) => match_type_inner(
            db,
            types,
            ty,
            with,
            &mut FxHashSet::default(),
            &mut FxHashSet::default(),
            subst,
            vars,
        ),
    }
}

fn match_type_inner(
    db: &dyn HirDatabase,
    types: &mut Types,
    ty: TyId,
    with: TyId,
    ty_skolems: &mut FxHashSet<TypeVar>,
    with_skolems: &mut FxHashSet<TypeVar>,
    subst: &mut FxHashMap<Unknown, TyId>,
    vars: &mut FxHashMap<TypeVar, TyId>,
) -> Matched<()> {
    tracing::trace!("{} == {}", ty.display(db, types), with.display(db, types));
    match (types[ty].clone(), types[with].clone()) {
        | (_, TyInfo::Unknown(_)) => unreachable!(),
        | (TyInfo::Error, _) | (_, TyInfo::Error) => Matched::Match(()),
        | (TyInfo::Skolem(v1, t1), TyInfo::Skolem(v2, t2)) if v1 == v2 => {
            match_type_inner(db, types, t1, t2, ty_skolems, with_skolems, subst, vars)
        },
        | (TyInfo::Skolem(_, _), _) => Matched::Unknown,
        | (TyInfo::TypeVar(a), TyInfo::TypeVar(b))
            if ty_skolems.contains(&a) && with_skolems.contains(&b) && a.idx() == b.idx() =>
        {
            Matched::Match(())
        },
        // | (_, TyInfo::TypeVar(tv)) if !vars.contains_key(&tv) => {
        | (_, TyInfo::TypeVar(tv)) => {
            vars.insert(tv, ty);
            Matched::Match(())
        },
        | (TyInfo::Unknown(u), _) => {
            subst.insert(u, with);
            Matched::Unknown
        },
        // @TODO: this case is not valid
        | (TyInfo::TypeVar(_), _) => Matched::Unknown,
        | (TyInfo::Figure(c1), TyInfo::Figure(c2)) if c1 == c2 => Matched::Match(()),
        | (TyInfo::Symbol(ref c1), TyInfo::Symbol(ref c2)) if c1 == c2 => Matched::Match(()),
        | (TyInfo::Ctor(c1), TyInfo::Ctor(c2)) if c1 == c2 => Matched::Match(()),
        | (TyInfo::Alias(c1), TyInfo::Alias(c2)) if c1 == c2 => Matched::Match(()),
        | (TyInfo::App(a1, ref a2), TyInfo::App(b1, ref b2)) if a2.len() == b2.len() => {
            match_type_inner(db, types, a1, b1, ty_skolems, with_skolems, subst, vars).then(
                a2.iter()
                    .zip(b2.iter())
                    .map(|(a2, b2)| match_type_inner(db, types, *a2, *b2, ty_skolems, with_skolems, subst, vars))
                    .fold(Matched::Match(()), Matched::then),
            )
        },
        | (TyInfo::App(_, a2), TyInfo::App(b1, b2)) if a2.len() < b2.len() => {
            let src = types.source(with);
            let c1 = types.insert(TyInfo::App(b1, b2[..a2.len()].into()), src);
            let c2 = types.insert(TyInfo::App(c1, b2[a2.len()..].into()), src);

            match_type_inner(db, types, ty, c2, ty_skolems, with_skolems, subst, vars)
        },
        | (TyInfo::App(a1, a2), TyInfo::App(_, b2)) if a2.len() > b2.len() => {
            let src = types.source(ty);
            let c1 = types.insert(TyInfo::App(a1, a2[..b2.len()].into()), src);
            let c2 = types.insert(TyInfo::App(c1, a2[b2.len()..].into()), src);

            match_type_inner(db, types, c2, with, ty_skolems, with_skolems, subst, vars)
        },
        | (TyInfo::ForAll(ref v1, a1, s1), TyInfo::ForAll(ref v2, a2, s2)) if v1.len() == v2.len() => {
            let mut matched = Matched::Match(());

            for (i, (k1, k2)) in v1.iter().zip(v2.iter()).enumerate() {
                ty_skolems.insert(TypeVar::new(i as u32, s1, None));
                with_skolems.insert(TypeVar::new(i as u32, s2, None));
                matched = matched.then(match_type_inner(
                    db,
                    types,
                    *k1,
                    *k2,
                    ty_skolems,
                    with_skolems,
                    subst,
                    vars,
                ));
            }

            matched = matched.then(match_type_inner(
                db,
                types,
                a1,
                a2,
                ty_skolems,
                with_skolems,
                subst,
                vars,
            ));

            for i in 0..v1.len() as u32 {
                ty_skolems.remove(&TypeVar::new(i, s1, None));
                with_skolems.remove(&TypeVar::new(i, s2, None));
            }

            matched
        },
        | (TyInfo::Row(..), TyInfo::Row(..)) => {
            let safe = unsafe { &*(types as *const Types) };
            let (matches, (lhs, rhs)) = TyId::align_rows_with(
                safe,
                |t1, t2| match_type_inner(db, types, t1, t2, ty_skolems, with_skolems, subst, vars),
                ty,
                with,
            );

            let tails = match_row_tails(types, lhs, rhs, vars);

            matches.into_iter().rfold(tails, Matched::then)
        },
        | (_, _) => Matched::Apart,
    }
}

fn match_row_tails(
    types: &mut Types,
    (f1, t1): (Box<[FieldInfo]>, Option<TyId>),
    (f2, t2): (Box<[FieldInfo]>, Option<TyId>),
    vars: &mut FxHashMap<TypeVar, TyId>,
) -> Matched<()> {
    match (t1.map(|t| &types[t]), t2.map(|t| &types[t])) {
        | (None, None) if f1.is_empty() && f2.is_empty() => Matched::Match(()),
        | (Some(TyInfo::Unknown(u1)), Some(TyInfo::Unknown(u2))) if u1 == u2 => Matched::Match(()),
        | (Some(TyInfo::TypeVar(u1)), Some(TyInfo::TypeVar(u2))) if u1 == u2 => Matched::Match(()),
        | (Some(TyInfo::Skolem(u1, _)), Some(TyInfo::Skolem(u2, _))) if u1 == u2 => Matched::Match(()),
        | (Some(TyInfo::Unknown(_)), _) => Matched::Unknown,
        | (_, Some(&TyInfo::TypeVar(v))) => {
            let src = types.source(t2.unwrap());
            let tail = types.insert(TyInfo::Row(f1, t1), src);
            vars.insert(v, tail);
            Matched::Match(())
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
