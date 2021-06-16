use crate::db::RootDatabase;
use annotate_snippets::display_list::{DisplayList, FormatOptions};
use annotate_snippets::snippet::{Annotation, AnnotationType, Slice, Snippet, SourceAnnotation};
use base_db::input::{FileId, LineIndex};
use base_db::libs::LibId;
use base_db::{SourceDatabase, SourceDatabaseExt};
use diagnostics::DiagnosticForWith;
use hir::db::HirDatabase;
use hir::diagnostic::{Diagnostic, DiagnosticSink};
use relative_path::{RelativePath, RelativePathBuf};
use std::collections::HashMap;
use std::io;
use std::sync::Arc;
use syntax::{SyntaxError, TextRange, TextSize};

pub fn emit_diagnostics(db: &RootDatabase, lib: hir::Lib, writer: &mut dyn io::Write) -> io::Result<usize> {
    let mut errors = 0;

    for module in lib.modules(db) {
        let file_id = module.file_id(db);
        let parse = db.parse(file_id);
        let source_root = db.file_source_root(file_id);
        let source_root = db.source_root(source_root);
        let source_path = source_root.relative_path(file_id);
        let source_code = db.file_text(file_id);
        let line_index = db.line_index(file_id);

        for err in parse.errors().iter() {
            errors += 1;
            emit_syntax_error(err, source_path, &source_code, &line_index, writer)?;
        }

        let mut diagnostic_sink = DiagnosticSink::new(|d| {
            errors += 1;
            emit_hir_diagnostic(d, db, file_id, writer);
        });

        module.diagnostics(db, &mut diagnostic_sink);
    }

    Ok(errors)
}

const FMT_OPTS: FormatOptions = FormatOptions {
    color: true,
    anonymized_line_numbers: false,
    margin: None,
};

fn emit_syntax_error(
    err: &SyntaxError,
    source_path: &RelativePath,
    source_code: &str,
    line_index: &LineIndex,
    writer: &mut dyn io::Write,
) -> io::Result<()> {
    let message = err.to_string();
    let mut range = err.range();

    if range.start() == range.end() && range.start() != TextSize::default() {
        range = TextRange::new(range.start() - TextSize::of(" "), range.end());
    }

    let line = line_index.line_col(range.start()).line;
    let line_offset = line_index.line_offset(line);

    let snippet = Snippet {
        title: Some(Annotation {
            id: None,
            label: Some("syntax error"),
            annotation_type: AnnotationType::Error,
        }),
        slices: vec![Slice {
            source: &source_code[line_offset..],
            line_start: line as usize + 1,
            origin: Some(source_path.as_str()),
            annotations: vec![SourceAnnotation {
                range: (
                    Into::<usize>::into(range.start()) - line_offset,
                    Into::<usize>::into(range.end()) - line_offset,
                ),
                label: &message,
                annotation_type: AnnotationType::Error,
            }],
            fold: true,
        }],
        footer: vec![],
        opt: FMT_OPTS,
    };

    let dl = DisplayList::from(snippet);

    writeln!(writer, "{}\n", dl)
}

fn emit_hir_diagnostic(
    diag: &dyn Diagnostic,
    db: &impl HirDatabase,
    file_id: FileId,
    writer: &mut dyn io::Write,
) -> io::Result<()> {
    diag.with_diagnostic(db, |diag| emit_diagnostic(diag, db, file_id, writer))
}

fn emit_diagnostic(
    diag: &dyn diagnostics::Diagnostic,
    db: &impl HirDatabase,
    file_id: FileId,
    writer: &mut dyn io::Write,
) -> io::Result<()> {
    let title = diag.title();
    let range = diag.range();

    struct AnnoationFile {
        path: RelativePathBuf,
        source_code: Arc<String>,
        line_index: Arc<LineIndex>,
        annotations: Vec<diagnostics::SourceAnnotation>,
    }

    let annotations = {
        let mut annotations = Vec::new();
        let mut file_to_index = HashMap::new();
        let source_root = db.file_source_root(file_id);
        let source_root = db.source_root(source_root);

        annotations.push(AnnoationFile {
            path: source_root.relative_path(file_id).to_owned(),
            source_code: SourceDatabaseExt::file_text(db, file_id),
            line_index: db.line_index(file_id),
            annotations: vec![match diag.primary_annotation() {
                | None => diagnostics::SourceAnnotation {
                    range,
                    message: title.clone(),
                },
                | Some(ann) => ann,
            }],
        });

        file_to_index.insert(file_id, 0);

        for ann in diag.secondary_annotations() {
            let file_id = ann.range.file_id;
            let file_idx = match file_to_index.get(&file_id) {
                | None => {
                    let source_root = db.file_source_root(file_id);
                    let source_root = db.source_root(source_root);

                    annotations.push(AnnoationFile {
                        path: source_root.relative_path(file_id).to_owned(),
                        source_code: SourceDatabaseExt::file_text(db, file_id),
                        line_index: db.line_index(file_id),
                        annotations: Vec::new(),
                    });

                    let idx = annotations.len() - 1;

                    file_to_index.insert(file_id, idx);
                    idx
                },
                | Some(idx) => *idx,
            };

            annotations[file_idx].annotations.push(ann.into());
        }

        annotations
    };

    let notes = diag.notes();
    let snippet = Snippet {
        title: Some(Annotation {
            id: None,
            label: Some(&title),
            annotation_type: AnnotationType::Error,
        }),
        slices: annotations
            .iter()
            .filter_map(|file| {
                let first_offset = {
                    let mut iter = file.annotations.iter();

                    match iter.next() {
                        | Some(first) => {
                            let first = first.range.start();

                            iter.fold(first, |init, value| init.min(value.range.start()))
                        },
                        | None => return None,
                    }
                };

                let first_offset_line = file.line_index.line_col(first_offset);
                let line_offset = file.line_index.line_offset(first_offset_line.line);

                Some(Slice {
                    source: &file.source_code[line_offset..],
                    line_start: first_offset_line.line as usize + 1,
                    origin: Some(file.path.as_ref()),
                    annotations: file
                        .annotations
                        .iter()
                        .map(|ann| SourceAnnotation {
                            range: (
                                usize::from(ann.range.start()) - line_offset,
                                usize::from(ann.range.end()) - line_offset,
                            ),
                            label: ann.message.as_str(),
                            annotation_type: AnnotationType::Error,
                        })
                        .collect(),
                    fold: true,
                })
            })
            .collect(),
        footer: notes
            .iter()
            .map(|note| Annotation {
                id: None,
                label: Some(note.as_str()),
                annotation_type: AnnotationType::Note,
            })
            .collect(),
        opt: FMT_OPTS,
    };

    let dl = DisplayList::from(snippet);

    write!(writer, "{}\n", dl)
}
