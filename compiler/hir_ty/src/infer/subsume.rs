use super::{ExprOrPatId, InferenceContext};
use crate::display::HirDisplay;
use crate::ty::*;

impl InferenceContext<'_> {
    pub fn subsume_types(&mut self, t1: Ty, t2: Ty, origin: ExprOrPatId) -> bool {
        let t1 = self.subst_type(t1);
        let t2 = self.subst_type(t2);
        let never = self.lang_type("never-type");

        if t1 == never {
            return true;
        }

        match (t1.lookup(self.db), t2.lookup(self.db)) {
            | (TyKind::ForAll(kind, inner), _) => {
                let var = self.fresh_type_with_kind(kind);
                let repl = self.replace_var(inner, var);

                self.subsume_types_no_ctnt(repl, t2)
            },
            | (_, TyKind::ForAll(kind, inner)) => {
                let skolem = self.enter_universe();
                let sk = self.skolemize(skolem, kind, inner);
                let res = self.subsume_types_no_ctnt(t1, sk);

                self.exit_universe();
                res
            },
            | (TyKind::Ctnt(ctnt, inner), _) => {
                self.constrain(origin, ctnt);
                self.subsume_types(inner, t2, origin)
            },
            | (_, _) => self.unify_types(t1, t2),
        }
    }

    fn subsume_types_no_ctnt(&mut self, t1: Ty, t2: Ty) -> bool {
        let t1 = self.subst_type(t1);
        let t2 = self.subst_type(t2);

        match (t1.lookup(self.db), t2.lookup(self.db)) {
            | (TyKind::ForAll(kind, inner), _) => {
                let var = self.fresh_type_with_kind(kind);
                let repl = self.replace_var(inner, var);

                self.subsume_types_no_ctnt(repl, t2)
            },
            | (_, TyKind::ForAll(kind, inner)) => {
                let skolem = self.enter_universe();
                let sk = self.skolemize(skolem, kind, inner);
                let res = self.subsume_types_no_ctnt(t1, sk);

                self.exit_universe();
                res
            },
            | (_, _) => self.unify_types(t1, t2),
        }
    }
}
