pub mod opts;

pub use codespan::FileId;
use std::sync::Arc;

pub type Files = codespan::Files<Arc<str>>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LibId(pub u32);

#[salsa::query_group(SourceDatabaseStorage)]
pub trait SourceDatabase: salsa::Database {
    #[salsa::input]
    fn libs(&self) -> Vec<LibId>;

    #[salsa::input]
    fn files(&self) -> Arc<Files>;

    #[salsa::input]
    fn lib_files(&self, lib: LibId) -> Arc<Vec<FileId>>;

    #[salsa::input]
    fn file_lib(&self, id: FileId) -> LibId;

    #[salsa::input]
    fn manifest(&self, lib: LibId) -> Arc<opts::Manifest>;

    fn file_content(&self, id: FileId) -> Arc<str>;
}

fn file_content(db: &dyn SourceDatabase, id: FileId) -> Arc<str> {
    db.files().source(id).clone()
}
