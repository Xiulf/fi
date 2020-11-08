pub use codespan::{FileId, Files};
use std::sync::Arc;

#[salsa::query_group(SourceDatabaseStorage)]
pub trait SourceDatabase: salsa::Database {
    #[salsa::input]
    fn files(&self) -> Arc<Files<Arc<str>>>;

    fn file_content(&self, id: FileId) -> Arc<str>;
}

fn file_content(db: &dyn SourceDatabase, id: FileId) -> Arc<str> {
    db.files().source(id).clone()
}
