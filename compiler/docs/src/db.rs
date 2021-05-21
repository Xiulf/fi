use crate::elems::{Page, PageId};
use hir::db::HirDatabase;

#[salsa::query_group(DocDatabaseStorage)]
pub trait DocDatabase: HirDatabase {
    #[salsa::interned]
    fn inter_page(&self, page: Page) -> PageId;
}
