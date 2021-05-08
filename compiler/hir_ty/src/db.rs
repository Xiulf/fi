use crate::class::Class;
use crate::infer::InferenceResult;
use crate::lower::LowerResult;
use crate::ty::{Ty, TyKind};
use base_db::Upcast;
use hir_def::db::DefDatabase;
use hir_def::id::{ClassId, DefWithBodyId, TypeAliasId, TypeCtorId};
use std::sync::Arc;

#[salsa::query_group(HirDatabaseStorage)]
pub trait HirDatabase: DefDatabase + Upcast<dyn DefDatabase> {
    #[salsa::invoke(crate::infer::infer_query)]
    fn infer(&self, id: DefWithBodyId) -> Arc<InferenceResult>;

    #[salsa::interned]
    fn intern_ty(&self, ty: TyKind) -> Ty;

    #[salsa::invoke(crate::lower::type_for_alias)]
    #[salsa::cycle(crate::lower::type_for_alias_recover)]
    fn type_for_alias(&self, id: TypeAliasId) -> Arc<LowerResult>;

    #[salsa::invoke(crate::lower::type_for_ctor)]
    #[salsa::cycle(crate::lower::type_for_ctor_recover)]
    fn type_for_ctor(&self, id: TypeCtorId) -> Arc<LowerResult>;

    #[salsa::invoke(crate::lower::kind_for_ctor)]
    fn kind_for_ctor(&self, id: TypeCtorId) -> Ty;

    #[salsa::invoke(crate::class::lower_class_query)]
    fn lower_class(&self, id: ClassId) -> Arc<Class>;
}
