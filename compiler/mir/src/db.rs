use crate::ir::Body;
use crate::layout::Layout;
use base_db::Upcast;
use hir::db::HirDatabase;
use hir::ty::Ty;
use std::sync::Arc;

#[salsa::query_group(MirDatabaseStorage)]
pub trait MirDatabase: HirDatabase + Upcast<dyn HirDatabase> {
    #[salsa::input]
    fn target_triple(&self) -> Arc<target_lexicon::Triple>;

    #[salsa::invoke(crate::layout::layout_of_query)]
    fn layout_of(&self, ty: Ty) -> Arc<Layout>;

    #[salsa::invoke(Body::body_mir_query)]
    fn body_mir(&self, def: hir::id::DefWithBodyId) -> Arc<Body>;
}
