use crate::infer::InferenceResult;
use crate::ty::{Ty, TyKind};
use base_db::Upcast;
use hir_def::db::DefDatabase;
use hir_def::id::DefWithBodyId;
use std::sync::Arc;

#[salsa::query_group(HirDatabaseStorage)]
pub trait HirDatabase: DefDatabase + Upcast<dyn DefDatabase> {
    #[salsa::invoke(crate::infer::infer_query)]
    fn infer(&self, id: DefWithBodyId) -> Arc<InferenceResult>;

    #[salsa::interned]
    fn intern_ty(&self, ty: TyKind) -> Ty;
}
