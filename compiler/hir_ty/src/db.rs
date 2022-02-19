use crate::class::{MemberMatchResult, Members};
use crate::infer::InferenceResult;
use crate::lower::{ClassLowerResult, LowerResult, MemberLowerResult};
use crate::ty::{Constraint, Ty, TyKind};
use base_db::Upcast;
use hir_def::db::DefDatabase;
use hir_def::id::{ClassId, CtorId, DefWithBodyId, MemberId, TypeAliasId, TypeCtorId, ValueTyDefId};
use std::sync::Arc;

#[salsa::query_group(HirDatabaseStorage)]
pub trait HirDatabase: DefDatabase + Upcast<dyn DefDatabase> {
    #[salsa::invoke(crate::infer::infer_query)]
    fn infer(&self, id: DefWithBodyId) -> Arc<InferenceResult<Ty>>;

    #[salsa::invoke(crate::lower::value_ty)]
    fn value_ty(&self, id: ValueTyDefId) -> Ty;

    #[salsa::invoke(crate::lower::ctor_ty)]
    fn ctor_ty(&self, id: CtorId) -> Arc<LowerResult>;

    #[salsa::interned]
    fn intern_ty(&self, ty: TyKind) -> Ty;

    #[salsa::invoke(crate::lower::type_for_alias)]
    #[salsa::cycle(crate::lower::type_for_alias_recover)]
    fn type_for_alias(&self, id: TypeAliasId) -> Arc<LowerResult>;

    #[salsa::invoke(crate::lower::kind_for_ctor)]
    #[salsa::cycle(crate::lower::kind_for_ctor_recover)]
    fn kind_for_ctor(&self, id: TypeCtorId) -> Arc<LowerResult>;

    #[salsa::invoke(crate::lower::lower_class_query)]
    fn lower_class(&self, id: ClassId) -> Arc<ClassLowerResult>;

    #[salsa::invoke(crate::lower::lower_member_query)]
    fn lower_member(&self, id: MemberId) -> Arc<MemberLowerResult>;

    #[salsa::invoke(Members::members_query)]
    fn members(&self, id: ClassId) -> Arc<Members>;

    #[salsa::invoke(Members::solve_constraint_query)]
    fn solve_constraint(&self, ctnt: Constraint) -> Option<Arc<MemberMatchResult>>;
}
