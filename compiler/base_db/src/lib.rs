#![feature(crate_visibility_modifier)]

pub mod input;
pub mod libs;
pub mod metadata;

use input::{FileId, FileTree, LineIndex, SourceRoot, SourceRootId};
use std::fmt;
use std::panic;
use std::path::PathBuf;
use std::sync::Arc;
use syntax::{ast, Parsed};

pub trait Upcast<T: ?Sized> {
    fn upcast(&self) -> &T;
}

#[salsa::query_group(SourceDatabaseStorage)]
pub trait SourceDatabase: CheckCanceled + FileLoader {
    fn parse(&self, file_id: FileId) -> Parsed<ast::SourceFile>;

    fn parse_path(&self, file_id: FileId) -> Parsed<ast::Path>;
    fn parse_type(&self, file_id: FileId) -> Parsed<ast::Type>;

    #[salsa::input]
    fn libs(&self) -> Arc<libs::LibSet>;
}

#[salsa::query_group(SourceDatabaseExtStorage)]
pub trait SourceDatabaseExt: SourceDatabase {
    #[salsa::input]
    fn file_text(&self, file_id: FileId) -> Arc<String>;

    #[salsa::input]
    fn file_source_root(&self, file_id: FileId) -> SourceRootId;

    #[salsa::input]
    fn file_lib(&self, file_id: FileId) -> libs::LibId;

    #[salsa::input]
    fn lib_source_root(&self, lib: libs::LibId) -> SourceRootId;

    #[salsa::input]
    fn source_root(&self, id: SourceRootId) -> Arc<SourceRoot>;

    #[salsa::input]
    fn target_dir(&self, lib: libs::LibId) -> PathBuf;

    #[salsa::invoke(FileTree::file_tree_query)]
    fn file_tree(&self, lib: libs::LibId) -> Arc<FileTree>;

    fn line_index(&self, file_id: FileId) -> Arc<LineIndex>;

    #[salsa::invoke(metadata::read_metadata)]
    fn metadata(&self, lib: libs::LibId) -> Arc<metadata::Metadata>;
}

fn parse(db: &dyn SourceDatabase, file_id: FileId) -> Parsed<ast::SourceFile> {
    let text = db.file_text(file_id);

    ast::SourceFile::parse(&*text)
}

fn parse_path(db: &dyn SourceDatabase, file_id: FileId) -> Parsed<ast::Path> {
    let text = db.file_text(file_id);

    Parsed::<ast::Path>::parse(&*text)
}

fn parse_type(db: &dyn SourceDatabase, file_id: FileId) -> Parsed<ast::Type> {
    let text = db.file_text(file_id);

    Parsed::<ast::Type>::parse(&*text)
}

fn line_index(db: &dyn SourceDatabaseExt, file_id: FileId) -> Arc<LineIndex> {
    let text = SourceDatabaseExt::file_text(db, file_id);

    Arc::new(LineIndex::new(&text))
}

pub trait FileLoader {
    fn file_text(&self, file_id: FileId) -> Arc<String>;
}

pub struct FileLoaderDelegate<T>(pub T);

impl<T: SourceDatabaseExt> FileLoader for FileLoaderDelegate<&'_ T> {
    fn file_text(&self, file_id: FileId) -> Arc<String> {
        SourceDatabaseExt::file_text(self.0, file_id)
    }
}

pub trait CheckCanceled {
    fn check_canceled(&self);

    fn catch_canceled<F, T>(&self, f: F) -> Result<T, Canceled>
    where
        Self: Sized + panic::RefUnwindSafe,
        F: FnOnce(&Self) -> T + panic::UnwindSafe,
    {
        panic::catch_unwind(|| f(self)).map_err(|err| match err.downcast::<Canceled>() {
            | Ok(canceled) => *canceled,
            | Err(payload) => panic::resume_unwind(payload),
        })
    }
}

impl<T: salsa::Database> CheckCanceled for T {
    fn check_canceled(&self) {
        if self.salsa_runtime().is_current_revision_canceled() {
            Canceled::throw()
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Canceled;

impl Canceled {
    pub fn throw() -> ! {
        panic::resume_unwind(Box::new(Canceled))
    }
}

impl fmt::Display for Canceled {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("canceled")
    }
}

impl std::error::Error for Canceled {
}
