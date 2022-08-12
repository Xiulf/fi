use std::sync::Arc;

use hir_def::expr::ExprId;
use hir_def::id::TypeVarOwner;
use hir_def::resolver::{TypeNs, ValueNs};

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
        let mut icx = InferenceContext::new(self.db, self.resolver.clone(), self.owner, false);
        let safe = unsafe { &mut *(&mut icx as *mut InferenceContext) };
        let safe2 = unsafe { &mut *(&mut icx as *mut InferenceContext) };
        let src = self.source(expr);

        icx.types = self.types.clone();
        icx.subst = self.subst.clone();

        let results = self
            .resolver
            .iter_values()
            .map(|ns| {
                let ty = match ns {
                    | ValueNs::Local(id) => self.result.type_of_pat[id],
                    | ValueNs::Fixity(_) => safe.unit(src),
                    | ValueNs::Func(id) => {
                        if self.owner == TypeVarOwner::DefWithBodyId(id.into()) {
                            self.result.self_type.ty
                        } else {
                            self.db
                                .value_ty(id.into())
                                .ty
                                .to_info(self.db, &mut safe.types, &mut safe.type_vars, src)
                        }
                    },
                    | ValueNs::Static(id) => {
                        self.db
                            .value_ty(id.into())
                            .ty
                            .to_info(self.db, &mut safe.types, &mut safe.type_vars, src)
                    },
                    | ValueNs::Const(id) => {
                        self.db
                            .value_ty(id.into())
                            .ty
                            .to_info(self.db, &mut safe.types, &mut safe.type_vars, src)
                    },
                    | ValueNs::Ctor(id) => {
                        self.db
                            .value_ty(id.into())
                            .ty
                            .to_info(self.db, &mut safe.types, &mut safe.type_vars, src)
                    },
                };

                (ns, ty)
            })
            .filter(|(_, t)| safe2.types[*t] != TyInfo::Error)
            .filter(|(_, t)| {
                let res = icx.subsume_types(*t, ty, expr.into());
                icx.solve_constraints();
                let res = res && icx.result.diagnostics.is_empty();
                icx.result.diagnostics.clear();
                res
            })
            .map(|(ns, _)| ns)
            .collect();

        Arc::new(TypeSearchResult { results })
    }

    pub fn search_type(&self, ty: TyId) -> Arc<TypeSearchResult<TypeNs>> {
        Arc::new(TypeSearchResult { results: Vec::new() })
    }
}
