use base_db::input::FileId;
use base_db::{FileLoader, SourceDatabase, SourceDatabaseExt, Upcast};
use crossbeam_channel::Sender;
use diagnostics::{DiagnosticForWith, Level, SecondaryAnnotation};
use hir::db::DefDatabase;
use lsp_server::Notification;
use lsp_types::notification::{Notification as _, PublishDiagnostics};
use lsp_types::{DiagnosticRelatedInformation, DiagnosticSeverity, PublishDiagnosticsParams};
use syntax::TextRange;

use crate::db::LspDatabase;
use crate::state::{LspState, LspStateSnapshot, Task};
use crate::util;

pub struct Diagnostic {
    pub range: TextRange,
    pub message: String,
    pub severity: DiagnosticSeverity,
    pub secondary: Vec<SecondaryAnnotation>,
    pub notes: Vec<String>,
}

impl LspState {
    pub fn push_diagnostics(&self) {
        let snapshot = self.snapshot();
        let task_sender = self.task_sender.clone();

        self.thread_pool.execute(move || {
            let _ = snapshot.handle_diagnostics(task_sender);
        });
    }
}

impl LspStateSnapshot {
    fn handle_diagnostics(self, sender: Sender<Task>) -> anyhow::Result<()> {
        let libs = self.analysis.db.libs();

        for &lib in self.libs.iter() {
            let source_root = self.analysis.db.source_root(libs[lib].source_root);

            for file in source_root.files() {
                let line_index = self.analysis.db.line_index(file);
                let uri = util::uri(&self, file)?;
                let diagnostics = self.analysis.diagnostics(file)?;
                let mut lsp_diagnostics = Vec::with_capacity(diagnostics.len());

                for d in diagnostics {
                    lsp_diagnostics.push(lsp_types::Diagnostic {
                        range: util::lsp_range(&line_index, d.range),
                        severity: Some(d.severity),
                        message: d.message,
                        related_information: {
                            let mut annotations = Vec::with_capacity(d.secondary.len());

                            for a in d.secondary {
                                annotations.push(DiagnosticRelatedInformation {
                                    location: lsp_types::Location {
                                        uri: util::uri(&self, a.range.file_id)?,
                                        range: util::lsp_range(
                                            &self.analysis.db.line_index(a.range.file_id),
                                            a.range.value,
                                        ),
                                    },
                                    message: a.message,
                                });
                            }

                            if annotations.is_empty() {
                                None
                            } else {
                                Some(annotations)
                            }
                        },
                        ..Default::default()
                    });
                }

                sender.send(Task::Notify(Notification {
                    method: PublishDiagnostics::METHOD.into(),
                    params: serde_json::to_value(PublishDiagnosticsParams {
                        uri,
                        diagnostics: lsp_diagnostics,
                        version: None,
                    })?,
                }))?;
            }
        }

        Ok(())
    }
}

pub fn file_diagnostics(db: &LspDatabase, file_id: FileId) -> Vec<Diagnostic> {
    let parsed = db.parse(file_id);
    let mut result = Vec::new();

    result.extend(parsed.errors().iter().map(|d| Diagnostic {
        range: d.range(),
        severity: lsp_types::DiagnosticSeverity::ERROR,
        message: d.msg.clone(),
        secondary: Vec::new(),
        notes: Vec::new(),
    }));

    let mut sink = hir::diagnostic::DiagnosticSink::new(|d| {
        d.with_diagnostic(db, |d| {
            result.push(Diagnostic {
                range: d.range(),
                severity: match d.level() {
                    | Level::Error => lsp_types::DiagnosticSeverity::ERROR,
                    | Level::Warning => lsp_types::DiagnosticSeverity::WARNING,
                    | Level::Info => lsp_types::DiagnosticSeverity::INFORMATION,
                },
                message: d.title(),
                secondary: d.secondary_annotations(),
                notes: d.notes(),
            });
        })
    });

    for &lib_id in db.relevant_libs(file_id).iter() {
        let def_map = db.def_map(lib_id);

        for local_id in def_map.modules_for_file(file_id) {
            let module = hir::Module::from(def_map.module_id(local_id));

            module.diagnostics(db.upcast(), &mut sink);
        }
    }

    drop(sink);
    result
}
