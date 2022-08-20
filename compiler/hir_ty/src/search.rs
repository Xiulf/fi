use std::sync::Arc;

use hir_def::expr::ExprId;
use hir_def::id::TypeVarOwner;
use hir_def::resolver::{Resolver, ValueNs};
use rustc_hash::FxHashSet;

use crate::infer::InferenceContext;
use crate::info::{ToInfo, TyId, TyInfo};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeSearchResult<Ns> {
    pub results: Vec<Ns>,
}

impl<Ns> Default for TypeSearchResult<Ns> {
    fn default() -> Self {
        Self { results: Vec::new() }
    }
}

impl InferenceContext<'_> {
    pub fn search_value(&mut self, ty: TyId, expr: ExprId) -> Arc<TypeSearchResult<ValueNs>> {
        let resolver = Resolver::for_expr(self.db.upcast(), self.resolver.body_owner().unwrap(), expr);
        let ctnts = self.constraints.drain(..).collect();
        let subst = self.subst.clone();
        let src = self.source(expr);
        let mut results = Vec::new();
        let mut seen = FxHashSet::default();

        for (name, ns) in resolver.iter_values() {
            if seen.contains(name) {
                continue;
            }

            seen.insert(name);

            let t = match ns {
                | ValueNs::Local(id) => {
                    let t = self.result.type_of_pat[id];
                    self.subst_type(t)
                },
                | ValueNs::Fixity(_) => self.error(src),
                | ValueNs::Func(id) => {
                    if self.owner == TypeVarOwner::DefWithBodyId(id.into()) {
                        let t = self.result.self_type.ty;
                        self.subst_type(t)
                    } else {
                        self.db
                            .value_ty(id.into())
                            .ty
                            .to_info(self.db, &mut self.types, &mut self.type_vars, src)
                    }
                },
                | ValueNs::Static(id) => {
                    self.db
                        .value_ty(id.into())
                        .ty
                        .to_info(self.db, &mut self.types, &mut self.type_vars, src)
                },
                | ValueNs::Const(id) => {
                    self.db
                        .value_ty(id.into())
                        .ty
                        .to_info(self.db, &mut self.types, &mut self.type_vars, src)
                },
                | ValueNs::Ctor(id) => {
                    self.db
                        .value_ty(id.into())
                        .ty
                        .to_info(self.db, &mut self.types, &mut self.type_vars, src)
                },
            };

            if self.types[t] == TyInfo::Error {
                continue;
            }

            let res = self.subsume_types(t, ty, expr.into());
            let _ = self.solve_constraints();

            if res && self.result.diagnostics.is_empty() {
                results.push(ns);
            }

            self.result.diagnostics.clear();
        }

        self.subst = subst;
        self.constraints = ctnts;

        Arc::new(TypeSearchResult { results })
    }
}
