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
                let repl = inner.replace_var(self.db, var);

                self.subsume_types(repl, t2, origin)
            },
            | (_, TyKind::ForAll(kind, inner)) => {
                let skolem = self.enter_universe();
                let sk = self.skolemize(skolem, kind, inner);
                let res = self.subsume_types(t1, sk, origin);

                self.exit_universe();
                res
            },
            | (TyKind::Ctnt(ctnt, inner), _) => {
                self.constrain(origin, ctnt);
                self.subsume_types(inner, t2, origin)
            },
            | (_, _) => {
                use hir_def::id::HasModule;
                let module = self.owner.module(self.db.upcast());
                let func_id = self.db.lang_item(module.lib, "fn-type".into()).unwrap();
                let func_id = func_id.as_type_ctor().unwrap();

                match (t1.match_ctor(self.db, func_id), t2.match_ctor(self.db, func_id)) {
                    | (Some([a1, r1]), Some([a2, r2])) => {
                        return self.subsume_types_no_ctnt(a2, a1) && self.subsume_types_no_ctnt(r1, r2);
                    },
                    | (_, _) => {},
                }

                self.unify_types(t1, t2)
            },
        }
    }

    fn subsume_types_no_ctnt(&mut self, t1: Ty, t2: Ty) -> bool {
        let t1 = self.subst_type(t1);
        let t2 = self.subst_type(t2);

        match (t1.lookup(self.db), t2.lookup(self.db)) {
            | (TyKind::ForAll(kind, inner), _) => {
                let var = self.fresh_type_with_kind(kind);
                let repl = inner.replace_var(self.db, var);

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
