use super::{ExprOrPatId, InferenceContext, InferenceDiagnostic, MethodSource};
use crate::class::ClassEnvScope;
use crate::ty::*;

impl InferenceContext<'_> {
    pub fn solve_constraints(&mut self) {
        let n_constraints = self.constraints.len();
        let mut unsolved = std::mem::replace(&mut self.constraints, Vec::with_capacity(n_constraints));
        let mut n_solved = 1;

        while !unsolved.is_empty() && n_solved > 0 {
            n_solved = 0;

            for (ctnt, id, scope) in unsolved {
                if self.solve_constraint(ctnt, id, scope) {
                    n_solved += 1;
                }
            }

            unsolved = std::mem::replace(&mut self.constraints, Vec::with_capacity(n_constraints));
        }

        for (ctnt, id, scope) in unsolved {
            if ctnt.can_be_generalized(self.db) {
                self.constraints.push((ctnt, id, scope));
            } else {
                self.report(InferenceDiagnostic::UnsolvedConstraint { id, ctnt });
            }
        }
    }

    fn solve_constraint(&mut self, ctnt: Constraint, id: ExprOrPatId, scope: Option<ClassEnvScope>) -> bool {
        let ctnt = self.subst_ctnt(&ctnt);

        if let Some(res) = self.class_env.solve(self.db, ctnt.clone(), scope) {
            for (&u, &ty) in res.subst.iter() {
                self.solve_type(u, ty);
            }

            true
        } else if let Some(res) = self.db.solve_constraint(ctnt.clone()) {
            res.apply(self);

            if let ExprOrPatId::ExprId(expr) = id {
                self.result.methods.insert(expr, MethodSource::Instance(res.instance));
            }

            true
        } else {
            self.constraints.push((ctnt, id, scope));
            false
        }
    }
}
