use crate::eval::EvalResult;
use crate::ir::Bodies;
use crate::layout::Layout;
use crate::ty::Type;
use base_db::Upcast;
use hir::db::HirDatabase;
use hir::ty::Ty;
use std::sync::Arc;

#[salsa::query_group(MirDatabaseStorage)]
pub trait MirDatabase: HirDatabase + Upcast<dyn HirDatabase> {
    #[salsa::input]
    fn target_triple(&self) -> Arc<target_lexicon::Triple>;

    #[salsa::invoke(crate::layout::layout_of_query)]
    fn layout_of(&self, ty: Arc<Type>) -> Arc<Layout>;

    #[salsa::invoke(Type::mir_type_query)]
    fn mir_type(&self, ty: Ty) -> Arc<Type>;

    #[salsa::invoke(Bodies::body_mir_query)]
    fn body_mir(&self, def: hir::id::DefWithBodyId) -> Arc<Bodies>;

    #[salsa::invoke(crate::eval::eval_query)]
    fn eval(&self, def: hir::id::DefWithBodyId) -> EvalResult;
}
