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
    hir::db::HirDatabaseStorage
)]
#[derive(Default)]
pub struct LspDatabase {
    storage: ManuallyDrop<salsa::Storage<Self>>,
}

impl LspDatabase {
    pub fn request_cancellation(&mut self) {
        self.storage.salsa_runtime_mut().synthetic_write(salsa::Durability::LOW);
    }
}

impl Drop for LspDatabase {
    fn drop(&mut self) {
        unsafe { ManuallyDrop::drop(&mut self.storage) }
    }
}

impl salsa::Database for LspDatabase {
}

impl salsa::ParallelDatabase for LspDatabase {
    fn snapshot(&self) -> salsa::Snapshot<Self> {
        salsa::Snapshot::new(Self {
            storage: ManuallyDrop::new(self.storage.snapshot()),
        })
    }
}

impl FileLoader for LspDatabase {
    fn file_text(&self, file_id: FileId) -> Arc<str> {
        FileLoaderDelegate(self).file_text(file_id)
    }

    fn relevant_libs(&self, file_id: FileId) -> Arc<FxHashSet<LibId>> {
        FileLoaderDelegate(self).relevant_libs(file_id)
    }
}

impl Upcast<dyn base_db::SourceDatabase + 'static> for LspDatabase {
    fn upcast(&self) -> &(dyn base_db::SourceDatabase + 'static) {
        self
    }
}

impl Upcast<dyn hir::db::DefDatabase + 'static> for LspDatabase {
    fn upcast(&self) -> &(dyn hir::db::DefDatabase + 'static) {
        self
    }
}

impl Upcast<dyn hir::db::HirDatabase + 'static> for LspDatabase {
    fn upcast(&self) -> &(dyn hir::db::HirDatabase + 'static) {
        self
    }
}
