pub mod input;
pub mod libs;
pub mod target;

use std::sync::Arc;
use std::{fmt, panic};

use input::{FileId, LineIndex, SourceRoot, SourceRootId};
use libs::LibId;
use rustc_hash::FxHashSet;
pub use salsa::Cancelled;
use syntax::{ast, Parsed};
use target::CompilerTarget;

pub trait Upcast<T: ?Sized> {
    fn upcast(&self) -> &T;
}

#[salsa::query_group(SourceDatabaseStorage)]
pub trait SourceDatabase: FileLoader {
    fn parse(&self, file_id: FileId) -> Parsed<ast::SourceFile>;

    #[salsa::input]
    fn target(&self) -> CompilerTarget;

    #[salsa::input]
    fn libs(&self) -> Arc<libs::LibSet>;

    #[salsa::input]
    fn file_source_root(&self, file_id: FileId) -> SourceRootId;

    #[salsa::input]
    fn source_root(&self, id: SourceRootId) -> Arc<SourceRoot>;
}

#[salsa::query_group(SourceDatabaseExtStorage)]
pub trait SourceDatabaseExt: SourceDatabase {
    #[salsa::input]
    fn file_text(&self, file_id: FileId) -> Arc<str>;

    fn source_root_libs(&self, id: SourceRootId) -> Arc<FxHashSet<LibId>>;

    fn line_index(&self, file_id: FileId) -> Arc<LineIndex>;
}

fn parse(db: &dyn SourceDatabase, file_id: FileId) -> Parsed<ast::SourceFile> {
    let text = db.file_text(file_id);

    ast::SourceFile::parse(&*text)
}

fn source_root_libs(db: &dyn SourceDatabaseExt, id: SourceRootId) -> Arc<FxHashSet<LibId>> {
    let libs = db.libs();

    Arc::new(
        libs.iter()
            .filter(|&lib| db.file_source_root(libs[lib].root_file) == id)
            .collect(),
    )
}

fn line_index(db: &dyn SourceDatabaseExt, file_id: FileId) -> Arc<LineIndex> {
    let text = SourceDatabaseExt::file_text(db, file_id);

    Arc::new(LineIndex::new(&text))
}

pub trait FileLoader {
    fn file_text(&self, file_id: FileId) -> Arc<str>;
    fn relevant_libs(&self, file_id: FileId) -> Arc<FxHashSet<LibId>>;
}

pub struct FileLoaderDelegate<T>(pub T);

impl<T: SourceDatabaseExt> FileLoader for FileLoaderDelegate<&'_ T> {
    fn file_text(&self, file_id: FileId) -> Arc<str> {
        SourceDatabaseExt::file_text(self.0, file_id)
    }

    fn relevant_libs(&self, file_id: FileId) -> Arc<FxHashSet<LibId>> {
        let source_root = self.0.file_source_root(file_id);

        self.0.source_root_libs(source_root)
    }
}

#[derive(Debug)]
pub struct ICE(pub std::borrow::Cow<'static, str>);

impl ICE {
    pub fn throw(msg: impl Into<std::borrow::Cow<'static, str>>) -> ! {
        panic::panic_any(Self(msg.into()))
    }
}

impl fmt::Display for ICE {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "internal compiler error: '{}'", self.0)
    }
}

impl std::error::Error for ICE {
}

#[derive(Debug)]
pub struct Error(pub std::borrow::Cow<'static, str>);

impl Error {
    pub fn throw(msg: impl Into<std::borrow::Cow<'static, str>>) -> ! {
        panic::panic_any(Self(msg.into()))
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "error: '{}'", self.0)
    }
}

impl std::error::Error for Error {
}
