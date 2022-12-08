use std::mem::ManuallyDrop;
use std::sync::Arc;

use base_db::input::FileId;
use base_db::libs::LibId;
use base_db::{FileLoader, FileLoaderDelegate, Upcast};
use rustc_hash::FxHashSet;

#[salsa::database(
    base_db::SourceDatabaseStorage,
    base_db::SourceDatabaseExtStorage,
    hir::db::InternDatabaseStorage,
    hir::db::DefDatabaseStorage,
    hir::db::HirDatabaseStorage,
    mir::db::MirDatabaseStorage,
    codegen::db::CodegenDatabaseStorage
)]
#[derive(Default)]
pub struct RootDatabase {
    storage: ManuallyDrop<salsa::Storage<Self>>,
}

impl Drop for RootDatabase {
    fn drop(&mut self) {
        unsafe { ManuallyDrop::drop(&mut self.storage) }
    }
}

impl salsa::Database for RootDatabase {
}

impl salsa::ParallelDatabase for RootDatabase {
    fn snapshot(&self) -> salsa::Snapshot<Self> {
        salsa::Snapshot::new(RootDatabase {
            storage: ManuallyDrop::new(self.storage.snapshot()),
        })
    }
}

impl FileLoader for RootDatabase {
    fn file_text(&self, file_id: FileId) -> Arc<str> {
        FileLoaderDelegate(self).file_text(file_id)
    }

    fn relevant_libs(&self, file_id: FileId) -> Arc<FxHashSet<LibId>> {
        FileLoaderDelegate(self).relevant_libs(file_id)
    }
}

impl Upcast<dyn base_db::SourceDatabase + 'static> for RootDatabase {
    fn upcast(&self) -> &(dyn base_db::SourceDatabase + 'static) {
        self
    }
}

impl Upcast<dyn hir::db::DefDatabase + 'static> for RootDatabase {
    fn upcast(&self) -> &(dyn hir::db::DefDatabase + 'static) {
        self
    }
}

impl Upcast<dyn hir::db::HirDatabase + 'static> for RootDatabase {
    fn upcast(&self) -> &(dyn hir::db::HirDatabase + 'static) {
        self
    }
}

impl Upcast<dyn mir::db::MirDatabase + 'static> for RootDatabase {
    fn upcast(&self) -> &(dyn mir::db::MirDatabase + 'static) {
        self
    }
}

impl Upcast<dyn codegen::db::CodegenDatabase + 'static> for RootDatabase {
    fn upcast(&self) -> &(dyn codegen::db::CodegenDatabase + 'static) {
        self
    }
}
