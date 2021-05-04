use crate::db::HirDatabase;
use crate::ty::{Ty, TyKind};
use hir_def::arena::ArenaMap;
use hir_def::expr::ExprId;
use hir_def::id::{DefWithBodyId, FuncId};
use hir_def::pat::PatId;
use std::sync::Arc;

pub(crate) fn infer_query(db: &dyn HirDatabase, def: DefWithBodyId) -> Arc<InferenceResult> {
    unimplemented!();
}

#[derive(Debug, PartialEq, Eq)]
pub struct InferenceResult {
    pub type_of_expr: ArenaMap<ExprId, Ty>,
    pub type_of_pat: ArenaMap<PatId, Ty>,
}
