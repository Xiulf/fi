use std::sync::Arc;

use base_db::Upcast;
use hir_def::db::DefDatabase;
use hir_def::id::{ClassId, CtorId, DefWithBodyId, MemberId, TypeAliasId, TypeCtorId, ValueTyDefId};

use crate::class::Members;
use crate::infer::InferenceResult;
use crate::lower::{ClassLowerResult, LowerResult, MemberLowerResult};
use crate::ty::{Constraint, Ty, TyAndSrc, TyKind};

#[salsa::query_group(HirDatabaseStorage)]
pub trait HirDatabase: DefDatabase + Upcast<dyn DefDatabase> {
    #[salsa::invoke(crate::infer::infer_query)]
    #[salsa::cycle(crate::infer::infer_recover)]
    fn infer(&self, id: DefWithBodyId) -> Arc<InferenceResult<Ty, Constraint>>;

    #[salsa::invoke(crate::lower::value_ty)]
    #[salsa::cycle(crate::lower::value_ty_recover)]
    fn value_ty(&self, id: ValueTyDefId) -> TyAndSrc<Ty>;

    #[salsa::invoke(crate::lower::ctor_ty)]
    fn ctor_ty(&self, id: CtorId) -> Arc<LowerResult<Ty>>;

    #[salsa::interned]
    fn intern_ty(&self, ty: TyKind) -> Ty;

    #[salsa::invoke(crate::lower::type_for_alias)]
    #[salsa::cycle(crate::lower::type_for_alias_recover)]
    fn type_for_alias(&self, id: TypeAliasId) -> Arc<LowerResult<Ty>>;

    #[salsa::invoke(crate::lower::kind_for_ctor)]
    #[salsa::cycle(crate::lower::kind_for_ctor_recover)]
    fn kind_for_ctor(&self, id: TypeCtorId) -> Arc<LowerResult<Ty>>;

    #[salsa::invoke(crate::lower::lower_class_query)]
    fn lower_class(&self, id: ClassId) -> Arc<ClassLowerResult<Ty, Constraint>>;

    #[salsa::invoke(crate::lower::lower_member_query)]
    fn lower_member(&self, id: MemberId) -> Arc<MemberLowerResult<Ty, Constraint>>;

    #[salsa::invoke(Members::members_query)]
    fn members(&self, id: ClassId) -> Arc<Members>;
}
