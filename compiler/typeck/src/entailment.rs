use crate::ctx::*;
use crate::error::*;
use crate::ty::*;
use hir::ir;
use std::collections::{HashMap, HashSet};

impl<'db> Ctx<'db> {
    crate fn solve_ctnts(&mut self, should_generalize: bool) -> Result<Vec<Ctnt>> {
        loop {
            let solved = self.defer_pass(should_generalize)?;

            if !solved {
                return self.generalize_pass(should_generalize);
            }
        }
    }

    fn defer_pass(&mut self, should_generalize: bool) -> Result<bool> {
        let mut solved = false;

        for (i, (id, ctnt, ctx)) in self.ctnts.clone().into_iter().enumerate().rev() {
            if self.entails(SolverOpts::new(should_generalize, true), i, id, ctnt, ctx)? {
                solved = true;
            }
        }

        Ok(solved)
    }

    fn generalize_pass(&mut self, should_generalize: bool) -> Result<Vec<Ctnt>> {
        for (id, ctnt, ctx) in self.ctnts.clone() {
            self.entails(SolverOpts::new(should_generalize, false), 0, id, ctnt, ctx)?;
        }

        Ok(self.ctnts.drain(..).map(|(_, c, _)| c).collect())
    }

    fn all_instances(&mut self, class: ir::DefId) -> std::sync::Arc<Vec<Instance>> {
        if let Some(instances) = self.all_instances.get(&class) {
            instances.clone()
        } else {
            let mut instances = self
                .db
                .external_modules(self.db.lib())
                .iter()
                .flat_map(|module| {
                    if let Some(instances) = self.db.external_types(module.lib, module.id).instances.get(&class) {
                        instances.clone()
                    } else {
                        Vec::new()
                    }
                })
                .collect::<Vec<_>>();

            for module in self.db.module_tree(self.db.lib()).toposort(self.db.to_diag_db()) {
                if module.id != self.module {
                    if let Some(i) = self.db.typeck_module(self.db.lib(), module.id).instances.get(&class) {
                        instances.extend(i.clone());
                    }
                } else {
                    if let Some(i) = self.module_types.instances.get(&class) {
                        instances.extend(i.clone());
                    }

                    break;
                }
            }

            self.all_instances.insert(class, std::sync::Arc::new(instances));
            self.all_instances[&class].clone()
        }
    }
}

#[derive(Clone, Copy)]
struct SolverOpts {
    should_generialize: bool,
    defer_errors: bool,
}

enum EntailsResult<T> {
    Solved(T, Instance),
    Unsolved(Ctnt),
    Deferred,
}

#[derive(Debug)]
enum Matched<T> {
    Match(T),
    Apart,
    Unknown,
}

impl Matched<()> {
    fn then(self, other: Self) -> Self {
        match (self, other) {
            | (Matched::Match(_), Matched::Match(_)) => Matched::Match(()),
            | (Matched::Apart, _) => Matched::Apart,
            | (_, Matched::Apart) => Matched::Apart,
            | (_, _) => Matched::Unknown,
        }
    }
}

type Matching<T> = HashMap<TypeVar, T>;

trait InstanceLike: crate::display::TypedDisplay<()> {
    fn tys(&self) -> &List<Ty>;
    fn trait_(&self) -> ir::DefId;
}

impl InstanceLike for Instance {
    fn tys(&self) -> &List<Ty> {
        &self.tys
    }

    fn trait_(&self) -> ir::DefId {
        self.class
    }
}

impl InstanceLike for Ctnt {
    fn tys(&self) -> &List<Ty> {
        &self.tys
    }

    fn trait_(&self) -> ir::DefId {
        self.class
    }
}

impl SolverOpts {
    fn new(should_generialize: bool, defer_errors: bool) -> Self {
        SolverOpts {
            should_generialize,
            defer_errors,
        }
    }
}

impl<'db> Ctx<'db> {
    fn entails(&mut self, opts: SolverOpts, i: usize, id: ir::HirId, ctnt: Ctnt, ctx: Vec<Ctnt>) -> Result<bool> {
        let deps = if ctnt.class.lib == self.db.lib() {
            let file = self.db.module_tree(ctnt.class.lib).file(ctnt.class.module);
            let hir = self.db.module_hir(file);

            hir.items[&ctnt.class.into()].class().fundeps.clone()
        } else {
            let external = self.db.external_types(ctnt.class.lib, ctnt.class.module);

            external.classes[&ctnt.class].deps.clone()
        };

        let tys = (&ctnt.tys).into_iter().map(|t| self.subst_type(t)).collect::<Vec<_>>();

        let mut instances = ctx
            .into_iter()
            .filter(|c| c.class == ctnt.class)
            .filter_map(|instance| match self.matches(&deps, instance.clone(), tys.clone()) {
                | Matched::Apart => None,
                | Matched::Unknown => None,
                | Matched::Match(t) => Some((t, instance)),
            });

        if let Some((_subst, _instance)) = instances.next() {
            self.ctnts.remove(i);
            Ok(true)
        } else if ctnt.tys.is_empty() {
            Err(TypeError::NoImpl(ctnt))
        } else {
            let ctnt = self.subst_ctnt(ctnt);
            let instances = self.all_instances(ctnt.class);
            let matches = {
                use itertools::Itertools;
                let mut chain = List::empty();
                let mut instances = (&instances.iter().group_by(|i| {
                    let eq = i.chain == chain;

                    chain = i.chain.clone();
                    eq
                }))
                    .into_iter()
                    .flat_map(|(_, g)| g)
                    .collect::<Vec<_>>();

                instances.sort_by(|a, b| a.chain.cmp(&b.chain).then(a.chain_index.cmp(&b.chain_index)));

                instances
                    .into_iter()
                    .filter_map(|instance| match self.matches(&deps, instance.clone(), tys.clone()) {
                        | Matched::Apart => None,
                        | Matched::Unknown => None,
                        | Matched::Match(t) => Some((t, instance.clone())),
                    })
                    .collect()
            };

            match self.unique(opts, ctnt.clone(), matches)? {
                | EntailsResult::Solved(subst, instance) => {
                    self.ctnts.remove(i);

                    for tys in subst.values() {
                        self.pair_wise_m(tys)?;
                    }

                    let subst = subst
                        .into_iter()
                        .map(|(k, mut v)| (k, self.subst_type(v.swap_remove(0))))
                        .collect::<Matching<_>>();

                    for (t1, t2) in instance.tys.into_iter().zip(tys) {
                        let inferred = t1.replace_vars(subst.clone());

                        self.unify_types(inferred, t2)?;
                    }

                    self.bounds.insert(id, crate::BoundInfo {
                        source: crate::BoundSource::Instance(instance.id),
                    });

                    Ok(true)
                },
                | EntailsResult::Unsolved(_ctnt) => Ok(false),
                | EntailsResult::Deferred => Ok(false),
            }
        }
    }

    fn unique<T>(&self, opts: SolverOpts, ctnt: Ctnt, mut info: Vec<(T, Instance)>) -> Result<EntailsResult<T>> {
        if info.is_empty() {
            if opts.defer_errors {
                Ok(EntailsResult::Deferred)
            } else if opts.should_generialize && ctnt.tys.iter().any(can_be_generalized) {
                Ok(EntailsResult::Unsolved(ctnt))
            } else {
                Err(TypeError::NoImpl(ctnt))
            }
        } else if info.len() == 1 {
            let (t, i) = info.remove(0);

            Ok(EntailsResult::Solved(t, i))
        } else {
            unimplemented!();
        }
    }

    fn matches(&mut self, deps: &[ir::FunDep], instance: impl InstanceLike, tys: Vec<Ty>) -> Matched<Matching<Vec<Ty>>> {
        let matched = tys
            .clone()
            .into_iter()
            .zip(instance.tys())
            .map(|(a, b)| {
                let mut subst = Matching::new();
                let m = Self::type_heads_eq(a, b, &mut subst);

                (m, subst)
            })
            .collect::<Vec<_>>();

        if !self.covers(&deps, &matched) {
            if matched.iter().any(|(m, _)| matches!(m, Matched::Apart)) {
                Matched::Apart
            } else {
                Matched::Unknown
            }
        } else {
            let determined = deps.iter().flat_map(|dep| dep.determined.iter().copied()).collect::<HashSet<_>>();

            let solved = matched
                .into_iter()
                .enumerate()
                .map(|(i, (_, ts))| (i, ts))
                .filter(|(i, _)| !determined.contains(i))
                .map(|(_, ts)| ts)
                .collect::<Vec<_>>();

            let mut matched = HashMap::<_, Vec<Ty>>::new();

            for map in solved {
                for (k, mut v) in map {
                    matched.entry(k).or_default().append(&mut v);
                }
            }

            Self::verify_subst(matched)
        }
    }

    fn covers(&mut self, deps: &[ir::FunDep], matched: &[(Matched<()>, Matching<Vec<Ty>>)]) -> bool {
        let expected = (0..matched.len()).collect::<HashSet<_>>();
        let initial_set = matched
            .into_iter()
            .enumerate()
            .filter(|(_, (m, _))| matches!(m, Matched::Match(_)))
            .map(|(i, _)| i)
            .collect::<HashSet<_>>();

        fn until_fixed_point(deps: &[ir::FunDep], a: HashSet<usize>) -> HashSet<usize> {
            let b = apply_all(deps, a.clone());

            if b == a {
                b
            } else {
                until_fixed_point(deps, b)
            }
        }

        fn apply_all(deps: &[ir::FunDep], s: HashSet<usize>) -> HashSet<usize> {
            deps.iter().rfold(s, |s, dep| {
                if dep.determiners.iter().copied().collect::<HashSet<_>>().is_subset(&s) {
                    s.into_iter().chain(dep.determined.iter().copied()).collect()
                } else {
                    s
                }
            })
        }

        let final_set = until_fixed_point(deps, initial_set);

        final_set == expected
    }

    fn type_heads_eq(a: Ty, b: Ty, subst: &mut Matching<Vec<Ty>>) -> Matched<()> {
        match (&*a, &*b) {
            | (Type::Unknown(a), Type::Unknown(b)) if a == b => Matched::Match(()),
            | (Type::Skolem(_, _, s1, _), Type::Skolem(_, _, s2, _)) if s1 == s2 => Matched::Match(()),
            | (_, Type::Var(v)) => {
                subst.entry(*v).or_default().push(a);
                Matched::Match(())
            },
            | (Type::Var(v), _) => {
                subst.entry(*v).or_default().push(b);
                Matched::Match(())
            },
            | (Type::Ctor(a), Type::Ctor(b)) if a == b => Matched::Match(()),
            | (Type::Int(a), Type::Int(b)) if a == b => Matched::Match(()),
            | (Type::String(a), Type::String(b)) if a == b => Matched::Match(()),
            | (Type::App(a1, a2), Type::App(b1, b2)) => {
                let base = Self::type_heads_eq(a1.clone(), b1.clone(), subst);

                base.then(Self::type_heads_eq(a2.clone(), b2.clone(), subst))
            },
            | (Type::KindApp(a1, a2), Type::KindApp(b1, b2)) => {
                let base = Self::type_heads_eq(a1.clone(), b1.clone(), subst);

                base.then(Self::type_heads_eq(a2.clone(), b2.clone(), subst))
            },
            | (Type::Tuple(a), Type::Tuple(b)) if a.len() == b.len() => {
                if a.is_empty() {
                    Matched::Match(())
                } else {
                    let mut a = a.into_iter();
                    let mut b = b.into_iter();
                    let base = Self::types_eq(a.next().unwrap(), b.next().unwrap());

                    a.zip(b).fold(base, |m, (a, b)| m.then(Self::types_eq(a, b)))
                }
            },
            | (Type::Row(_, _), Type::Row(_, _)) => unimplemented!(),
            | (Type::Unknown(_), _) => Matched::Unknown,
            | (_, _) => Matched::Apart,
        }
    }

    fn verify_subst(matched: Matching<Vec<Ty>>) -> Matched<Matching<Vec<Ty>>> {
        let mut m = Matched::Match(());

        for tys in matched.values() {
            m = m.then(Self::pair_wise_all(tys));
        }

        match m {
            | Matched::Match(_) => Matched::Match(matched),
            | Matched::Unknown => Matched::Unknown,
            | Matched::Apart => Matched::Apart,
        }
    }

    fn types_eq(a: Ty, b: Ty) -> Matched<()> {
        match (&*a, &*b) {
            | (Type::Unknown(a), Type::Unknown(b)) if a == b => Matched::Match(()),
            | (Type::Skolem(_, _, a, _), Type::Skolem(_, _, b, _)) if a == b => Matched::Match(()),
            | (Type::Skolem(..), _) => Matched::Unknown,
            | (_, Type::Skolem(..)) => Matched::Unknown,
            | (Type::Var(a), Type::Var(b)) if a == b => Matched::Match(()),
            | (Type::Ctor(a), Type::Ctor(b)) if a == b => Matched::Match(()),
            | (Type::Int(a), Type::Int(b)) if a == b => Matched::Match(()),
            | (Type::String(a), Type::String(b)) if a == b => Matched::Match(()),
            | (Type::App(a1, a2), Type::App(b1, b2)) => {
                let base = Self::types_eq(a1.clone(), b1.clone());

                base.then(Self::types_eq(a2.clone(), b2.clone()))
            },
            | (Type::KindApp(a1, a2), Type::KindApp(b1, b2)) => {
                let base = Self::types_eq(a1.clone(), b1.clone());

                base.then(Self::types_eq(a2.clone(), b2.clone()))
            },
            | (Type::Tuple(a), Type::Tuple(b)) if a.len() == b.len() => {
                if a.is_empty() {
                    Matched::Match(())
                } else {
                    let mut a = a.into_iter();
                    let mut b = b.into_iter();
                    let base = Self::types_eq(a.next().unwrap(), b.next().unwrap());

                    a.zip(b).fold(base, |m, (a, b)| m.then(Self::types_eq(a, b)))
                }
            },
            | (_, _) => Matched::Apart,
        }
    }

    fn pair_wise_all(ms: &[Ty]) -> Matched<()> {
        if ms.len() >= 2 {
            let p = &ms[0];
            let mut m = Matched::Match(());

            for ty in ms.iter() {
                m = m.then(Self::types_eq(p.clone(), ty.clone()));
            }

            m.then(Self::pair_wise_all(&ms[1..]))
        } else {
            Matched::Match(())
        }
    }

    fn pair_wise_m(&mut self, tys: &[Ty]) -> Result<()> {
        if tys.len() >= 2 {
            let p = &tys[0];

            for ty in tys.iter() {
                self.unify_types(p.clone(), ty.clone())?;
            }

            self.pair_wise_m(&tys[1..])
        } else {
            Ok(())
        }
    }
}

fn can_be_generalized(ty: &Ty) -> bool {
    if let Type::Unknown(_) = &**ty {
        true
    } else {
        false
    }
}
