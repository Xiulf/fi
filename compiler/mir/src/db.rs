use std::sync::Arc;

use base_db::Upcast;
use hir::db::HirDatabase;
use hir::id::DefWithBodyId;

use crate::syntax::{Body, BodyData};

#[salsa::query_group(MirDatabaseStorage)]
pub trait MirDatabase: HirDatabase + Upcast<dyn HirDatabase> {
    #[salsa::interned]
    fn intern_body(&self, body: Arc<BodyData>) -> Body;

    #[salsa::invoke(crate::lower::lower_body)]
    fn body_mir(&self, def: DefWithBodyId) -> Body;
}
