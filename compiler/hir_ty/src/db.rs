use crate::class::{InstanceMatchResult, Instances};
use crate::infer::InferenceResult;
use crate::lower::{ClassLowerResult, InstanceLowerResult, LowerResult};
use crate::ty::{Constraint, Ty, TyKind};
use base_db::Upcast;
use hir_def::db::DefDatabase;
use hir_def::id::{ClassId, DefWithBodyId, InstanceId, TypeAliasId, TypeCtorId, ValueTyDefId};
use std::sync::Arc;

#[salsa::query_group(HirDatabaseStorage)]
pub trait HirDatabase: DefDatabase + Upcast<dyn DefDatabase> {
    #[salsa::invoke(crate::infer::infer_query)]
    fn infer(&self, id: DefWithBodyId) -> Arc<InferenceResult>;

    #[salsa::invoke(crate::lower::value_ty)]
    fn value_ty(&self, id: ValueTyDefId) -> Ty;

    #[salsa::interned]
    fn intern_ty(&self, ty: TyKind) -> Ty;

    #[salsa::invoke(crate::lower::type_for_alias)]
    #[salsa::cycle(crate::lower::type_for_alias_recover)]
    fn type_for_alias(&self, id: TypeAliasId) -> Arc<LowerResult>;

    #[salsa::invoke(crate::lower::type_for_ctor)]
    #[salsa::cycle(crate::lower::type_for_ctor_recover)]
    fn type_for_ctor(&self, id: TypeCtorId) -> Arc<LowerResult>;

    #[salsa::invoke(crate::lower::kind_for_ctor)]
    #[salsa::cycle(crate::lower::kind_for_ctor_recover)]
    fn kind_for_ctor(&self, id: TypeCtorId) -> Ty;

    #[salsa::invoke(crate::lower::lower_class_query)]
    fn lower_class(&self, id: ClassId) -> Arc<ClassLowerResult>;

    #[salsa::invoke(crate::lower::lower_instance_query)]
    fn lower_instance(&self, id: InstanceId) -> Arc<InstanceLowerResult>;

    #[salsa::invoke(Instances::instances_query)]
    fn instances(&self, id: ClassId) -> Arc<Instances>;

    #[salsa::invoke(Instances::solve_constraint_query)]
    fn solve_constraint(&self, ctnt: Constraint) -> Option<Arc<InstanceMatchResult>>;
}
