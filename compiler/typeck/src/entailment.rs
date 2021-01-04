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
        for (i, (id, ctnt, ctx)) in self.ctnts.clone().into_iter().enumerate().rev() {
            self.entails(SolverOpts::new(should_generalize, false), i, id, ctnt, ctx)?;
        }

        Ok(self.ctnts.drain(..).map(|(_, c, _)| c).collect())
    }
}

#[derive(Clone, Copy)]
struct SolverOpts {
    should_generialize: bool,
    defer_errors: bool,
}

enum EntailsResult {
    Solved(Impl),
    Unsolved(Ctnt),
    Deferred,
}

enum Matched<T> {
    Match(T),
    Apart,
    Unknown,
}

type Matching<T> = HashMap<ir::HirId, T>;

impl SolverOpts {
    fn new(should_generialize: bool, defer_errors: bool) -> Self {
        SolverOpts {
            should_generialize,
            defer_errors,
        }
    }
}

impl<'db> Ctx<'db> {
    fn entails(
        &mut self,
        opts: SolverOpts,
        i: usize,
        id: ir::HirId,
        ctnt: Ctnt,
        ctx: Vec<Ctnt>,
    ) -> Result<bool> {
        if let Some(_) = self.find_match_ctx(ctx, &ctnt) {
            self.ctnts.remove(i);
            Ok(true)
        } else if ctnt.tys.is_empty() {
            Err(TypeError::NoImpl(ctnt))
        } else {
            let ctnt = self.subst_ctnt(ctnt);
            let impls = self.db.impls(ctnt.trait_);
            let matches = self.find_matches(impls, &ctnt);

            match self.unique(opts, ctnt, matches)? {
                EntailsResult::Solved(_imp) => {
                    self.ctnts.remove(i);
                    Ok(true)
                }
                EntailsResult::Unsolved(ctnt) => Err(TypeError::NoImpl(ctnt)),
                EntailsResult::Deferred => Ok(false),
            }
        }
    }

    fn unique(&self, opts: SolverOpts, ctnt: Ctnt, mut info: Vec<Impl>) -> Result<EntailsResult> {
        if info.is_empty() {
            if opts.defer_errors {
                Ok(EntailsResult::Deferred)
            } else if opts.should_generialize && ctnt.tys.iter().any(can_be_generalized) {
                Ok(EntailsResult::Unsolved(ctnt))
            } else {
                Err(TypeError::NoImpl(ctnt))
            }
        } else if info.len() == 1 {
            Ok(EntailsResult::Solved(info.remove(0)))
        } else {
            unimplemented!();
        }
    }

    fn find_matches(&mut self, impls: List<Impl>, ctnt: &Ctnt) -> Vec<Impl> {
        impls
            .into_iter()
            .filter(|imp| {
                (&imp.tys)
                    .into_iter()
                    .zip(&ctnt.tys)
                    .all(|(exp, ty)| self.unify_types(exp, ty).is_ok())
            })
            .collect()
    }

    fn find_match_ctx(&mut self, ctx: Vec<Ctnt>, ctnt: &Ctnt) -> Option<Ctnt> {
        ctx.into_iter()
            .filter(|c| c.trait_ == ctnt.trait_)
            .find(|c| {
                (&c.tys)
                    .into_iter()
                    .zip(&ctnt.tys)
                    .all(|(exp, ty)| self.unify_types(exp, ty).is_ok())
            })
    }

    fn matches(
        &mut self,
        deps: Vec<ir::FunDep>,
        imp: Impl,
        tys: Vec<Ty>,
    ) -> Matched<Matching<Vec<Ty>>> {
        let matched = tys
            .into_iter()
            .zip(&imp.tys)
            .map(|(a, b)| self.type_heads_eq(a, b))
            .collect::<Vec<_>>();

        if !self.covers(&matched) {
            if matched.iter().any(|(m, _)| matches!(m, Matched::Apart)) {
                Matched::Apart
            } else {
                Matched::Unknown
            }
        } else {
            let determined = deps
                .iter()
                .flat_map(|dep| dep.determined.iter().copied())
                .collect::<HashSet<_>>();

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

            self.verify_subst(matched)
        }
    }

    fn covers(&mut self, matched: &[(Matched<()>, Matching<Vec<Ty>>)]) -> bool {
        unimplemented!();
    }

    fn type_heads_eq(&mut self, a: Ty, b: Ty) -> (Matched<()>, Matching<Vec<Ty>>) {
        unimplemented!();
    }

    fn verify_subst(&mut self, matched: Matching<Vec<Ty>>) -> Matched<Matching<Vec<Ty>>> {
        unimplemented!();
    }
}

fn can_be_generalized(ty: &Ty) -> bool {
    if let Type::Unknown(_) = &**ty {
        true
    } else {
        false
    }
}
