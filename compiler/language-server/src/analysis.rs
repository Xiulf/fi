use std::panic::UnwindSafe;
use std::sync::Arc;

use base_db::input::{FileId, SourceRoot, SourceRootId};
use base_db::libs::LibSet;
use base_db::{Canceled, CheckCanceled, SourceDatabase, SourceDatabaseExt};
use salsa::ParallelDatabase;

use crate::db::LspDatabase;
use crate::diagnostics::{self, Diagnostic};

#[derive(Default)]
pub struct Analysis {
    pub db: LspDatabase,
}

pub struct AnalysisSnapshot {
    pub db: salsa::Snapshot<LspDatabase>,
}

#[derive(Default)]
pub struct AnalysisChange {
    libs: Option<LibSet>,
    roots: Option<Vec<SourceRoot>>,
    files_changed: Vec<(FileId, Option<Arc<str>>)>,
}

impl Analysis {
    pub fn snapshot(&self) -> AnalysisSnapshot {
        AnalysisSnapshot { db: self.db.snapshot() }
    }

    pub fn apply_change(&mut self, change: AnalysisChange) {
        self.db.apply_change(change);
    }
}

impl AnalysisSnapshot {
    pub fn diagnostics(&self, file_id: FileId) -> Result<Vec<Diagnostic>, Canceled> {
        self.with_db(|db| diagnostics::file_diagnostics(db, file_id))
    }

    fn with_db<T>(&self, f: impl FnOnce(&LspDatabase) -> T + UnwindSafe) -> Result<T, Canceled> {
        self.db.catch_canceled(f)
    }
}

impl AnalysisChange {
    pub fn set_libs(&mut self, libs: LibSet) {
        self.libs = Some(libs);
    }

    pub fn set_roots(&mut self, roots: Vec<SourceRoot>) {
        self.roots = Some(roots);
    }

    pub fn change_file(&mut self, file: FileId, text: Option<Arc<str>>) {
        self.files_changed.push((file, text));
    }
}

impl LspDatabase {
    fn apply_change(&mut self, change: AnalysisChange) {
        if let Some(libs) = change.libs {
            self.set_libs(Arc::new(libs));
        }

        if let Some(roots) = change.roots {
            for (idx, root) in roots.into_iter().enumerate() {
                let root_id = SourceRootId(idx as u32);

                for file_id in root.iter() {
                    self.set_file_source_root(file_id, root_id);
                }

                self.set_source_root(root_id, Arc::new(root));
            }
        }

        for (file_id, text) in change.files_changed {
            let text = text.unwrap_or_else(|| Arc::from("".to_string()));

            self.set_file_text(file_id, text);
        }
    }
}
