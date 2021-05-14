use super::{ExprOrPatId, InferenceContext, InferenceDiagnostic};
use crate::ty::*;

impl InferenceContext<'_> {
    pub fn solve_constraints(&mut self) {
        let mut unsolved = std::mem::replace(&mut self.constraints, Vec::new());
        let mut n_solved = 1;

        while !unsolved.is_empty() && n_solved > 0 {
            n_solved = 0;

            for (ctnt, id) in unsolved {
                if self.solve_constraint(ctnt, id) {
                    n_solved += 1;
                }
            }

            unsolved = std::mem::replace(&mut self.constraints, Vec::new());
        }

        for (ctnt, id) in unsolved {
            self.report(InferenceDiagnostic::UnsolvedConstraint { id, ctnt });
        }
    }

    fn solve_constraint(&mut self, ctnt: Constraint, id: ExprOrPatId) -> bool {
        // @TODO: implement local constraints

        if let Some(res) = self.db.solve_constraint(self.subst_ctnt(&ctnt)) {
            for (&u, &ty) in res.subst.iter() {
                self.solve_type(u, ty);
            }

            true
        } else {
            self.constraints.push((ctnt, id));
            false
        }
    }
}
