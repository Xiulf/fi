use hir_def::id::HasModule;

use super::diagnostics::{CtntExpected, CtntFound};
use super::{ExprOrPatId, InferenceContext, InferenceDiagnostic, MethodSource};
use crate::class::{ClassEnvPath, ClassEnvScope, MatchConstraint, Members};
use crate::info::{CtntInfo, TypeOrigin};

impl InferenceContext<'_> {
    pub fn solve_constraints(&mut self) {
        let n_constraints = self.constraints.len();
        let mut unsolved = std::mem::replace(&mut self.constraints, Vec::with_capacity(n_constraints));
        let mut n_solved = 1;

        while !unsolved.is_empty() && n_solved > 0 {
            n_solved = 0;

            for (ctnt, expected, found, scope) in unsolved {
                if self.solve_constraint(ctnt, expected, found, scope) {
                    n_solved += 1;
                }
            }

            unsolved = std::mem::replace(&mut self.constraints, Vec::with_capacity(n_constraints));
        }

        unsolved.sort_by_key(|c| c.0.class);

        let mut prev = None;

        for (ctnt, expected, found, scope) in unsolved {
            if Some(&ctnt) == prev.as_ref() {
                if self.can_generalize && ctnt.can_be_generalized(&self.types) {
                    if !self.db.class_data(ctnt.class).items.is_empty() {
                        self.record_solve(
                            found,
                            MethodSource::Record(self.member_records - 1, ClassEnvPath::default()),
                        );
                    }
                }

                continue;
            }

            prev = Some(ctnt.clone());

            if self.can_generalize && ctnt.can_be_generalized(&self.types) {
                let class = self.db.class_data(ctnt.class);

                self.constraints.push((ctnt, expected, found, scope));

                if !class.items.is_empty() {
                    if self.record_solve(
                        found,
                        MethodSource::Record(self.member_records, ClassEnvPath::default()),
                    ) {
                        self.member_records += 1;
                    }
                }
            } else {
                self.report(InferenceDiagnostic::UnsolvedConstraint { expected, found, ctnt });
            }
        }
    }

    fn solve_constraint(
        &mut self,
        ctnt: CtntInfo,
        expected: CtntExpected,
        found: CtntFound,
        scope: Option<ClassEnvScope>,
    ) -> bool {
        let ctnt = self.subst_ctnt(&ctnt);
        let src = match found {
            | CtntFound::ExprOrPat(id) => self.source(id),
            | CtntFound::Member(id) => self.source(TypeOrigin::Def(id.into())),
        };

        let lib = self.owner.module(self.db.upcast()).lib;

        if let Some(res) = self
            .class_env
            .solve(self.db, &mut self.types, &mut self.type_vars, &ctnt, scope, src)
        {
            for (&u, &ty) in res.subst.iter() {
                self.solve_type(u, ty);
            }

            let entry = &self.class_env[res.scope];
            let class = self.db.class_data(entry.ctnt().class);

            if !class.items.is_empty() {
                let idx = self.class_env.index(res.scope);

                if self.record_solve(found, MethodSource::Record(idx, res.path)) {
                    // self.member_records += 1;
                }
            }

            true
        } else if let Some(res) = Members::solve_constraint(
            self.db,
            &self.class_env,
            &mut self.types,
            &mut self.type_vars,
            lib,
            &ctnt,
            src,
        ) {
            res.apply(ctnt, self);
            self.record_solve(found, MethodSource::Member(res.member));

            for ctnt in res.constraints.iter() {
                let source = match ctnt {
                    | MatchConstraint::Member(m) => MethodSource::Member(*m),
                    | MatchConstraint::Env(e) => {
                        let idx = self.class_env.index(*e);
                        // self.member_records = self.member_records.max(idx) + 1;
                        MethodSource::Record(idx, ClassEnvPath::default())
                    },
                };

                self.record_solve(found, source);
            }

            true
        } else {
            self.constraints.push((ctnt, expected, found, scope));
            false
        }
    }

    fn record_solve(&mut self, found: CtntFound, method: MethodSource) -> bool {
        match found {
            | CtntFound::ExprOrPat(ExprOrPatId::ExprId(expr)) => {
                self.result.methods.entry((expr, 0)).or_default().push(method);
                true
            },
            | CtntFound::ExprOrPat(ExprOrPatId::ExprIdInfix(expr, idx)) => {
                self.result.methods.entry((expr, idx)).or_default().push(method);
                true
            },
            | _ => {
                self.result.constraints.push(method);
                false
            },
        }
    }
}
