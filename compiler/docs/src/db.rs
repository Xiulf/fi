use crate::elems::{Page, PageId};
use base_db::Upcast;
use hir::db::HirDatabase;
use std::sync::Arc;

#[salsa::query_group(DocDatabaseStorage)]
pub trait DocDatabase: HirDatabase + Upcast<dyn HirDatabase> {
    #[salsa::interned]
    fn intern_page(&self, page: Arc<Page>) -> PageId;
}
