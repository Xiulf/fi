use super::{ExprOrPatId, InferenceContext};
use crate::ty::*;

#[derive(PartialEq)]
enum SubsumeMode {
    Ctnt,
    NoCtnt,
}

impl InferenceContext<'_> {
    pub fn subsume_types(&mut self, t1: Ty, t2: Ty, origin: ExprOrPatId) -> bool {
        let t1 = self.subst_type(t1);
        let t2 = self.subst_type(t2);
        let never = self.lang_type("never-type");

        if t1 == never {
            return true;
        }

        self.subsume_types_impl(t1, t2, origin, SubsumeMode::Ctnt)
    }

    fn subsume_types_impl(&mut self, t1: Ty, t2: Ty, origin: ExprOrPatId, mode: SubsumeMode) -> bool {
        match (t1.lookup(self.db), t2.lookup(self.db)) {
            | (TyKind::ForAll(kinds, inner), _) => {
                let vars = kinds.iter().map(|&k| self.fresh_type_with_kind(k)).collect::<Vec<_>>();
                let repl = inner.replace_vars(self.db, &vars);

                self.subsume_types_impl(repl, t2, origin, mode)
            },
            | (_, TyKind::ForAll(kinds, inner)) => {
                let sk = self.skolemize(&kinds, inner);

                self.subsume_types_impl(t1, sk, origin, mode)
            },
            | (TyKind::Ctnt(ctnt, inner), _) if mode == SubsumeMode::Ctnt => {
                self.constrain(origin, ctnt);
                self.subsume_types_impl(inner, t2, origin, mode)
            },
            | (TyKind::Func(a1, r1), TyKind::Func(a2, r2)) => {
                a2.iter()
                    .zip(a1.iter())
                    .all(|(&a2, &a1)| self.subsume_types_impl(a2, a1, origin, SubsumeMode::NoCtnt))
                    && self.subsume_types_impl(r1, r2, origin, SubsumeMode::NoCtnt)
            },
            | (_, _) => {
                use hir_def::id::HasModule;
                let module = self.owner.module(self.db.upcast());
                let record_id = self.db.lang_item(module.lib, "record-type".into()).unwrap();
                let record_id = record_id.as_type_ctor().unwrap();

                match (
                    t1.match_ctor(self.db, record_id).as_deref(),
                    t2.match_ctor(self.db, record_id).as_deref(),
                ) {
                    | (Some([r1]), Some([r2])) => {
                        let (common, ((ts1, r1), (ts2, r2))) = Ty::align_rows_with(
                            self.db,
                            |a, b| self.subsume_types_impl(a, b, origin, SubsumeMode::NoCtnt),
                            *r1,
                            *r2,
                        );

                        if let None = r1 {
                            for (f1, f2) in ts2.iter().zip(ts1.iter()) {
                                if f1.name != f2.name {
                                    return false;
                                }
                            }
                        }

                        if let None = r2 {
                            for (f1, f2) in ts1.iter().zip(ts2.iter()) {
                                if f1.name != f2.name {
                                    return false;
                                }
                            }
                        }

                        if !common.into_iter().all(std::convert::identity) {
                            return false;
                        }

                        let r1 = TyKind::Row(ts1, r1).intern(self.db);
                        let r2 = TyKind::Row(ts2, r2).intern(self.db);

                        return self.unify_types(r1, r2);
                    },
                    | (_, _) => {},
                }

                self.unify_types(t1, t2)
            },
        }
    }
}
