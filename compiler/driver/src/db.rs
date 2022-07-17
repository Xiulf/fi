use std::sync::Arc;

use base_db::input::FileId;
use base_db::{Canceled, CheckCanceled, FileLoader, FileLoaderDelegate, Upcast};

#[salsa::database(
    base_db::SourceDatabaseStorage,
    base_db::SourceDatabaseExtStorage,
    hir::db::InternDatabaseStorage,
    hir::db::DefDatabaseStorage,
    hir::db::HirDatabaseStorage,
    codegen::db::CodegenDatabaseStorage
)]
#[derive(Default)]
pub struct RootDatabase {
    storage: salsa::Storage<Self>,
}

impl salsa::Database for RootDatabase {
    fn on_propagated_panic(&self) -> ! {
        Canceled::throw()
    }

    fn salsa_event(&self, event: salsa::Event) {
        match event.kind {
            | salsa::EventKind::DidValidateMemoizedValue { .. } | salsa::EventKind::WillExecute { .. } => {
                self.check_canceled();
            },
            | _ => {},
        }
    }
}

impl salsa::ParallelDatabase for RootDatabase {
    fn snapshot(&self) -> salsa::Snapshot<Self> {
        salsa::Snapshot::new(RootDatabase {
            storage: self.storage.snapshot(),
        })
    }
}

impl FileLoader for RootDatabase {
    fn file_text(&self, file_id: FileId) -> Arc<String> {
        FileLoaderDelegate(self).file_text(file_id)
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

impl Upcast<dyn codegen::db::CodegenDatabase + 'static> for RootDatabase {
    fn upcast(&self) -> &(dyn codegen::db::CodegenDatabase + 'static) {
        self
    }
}
