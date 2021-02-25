use crate::db::RootDatabase;
use annotate_snippets::display_list::{DisplayList, FormatOptions};
use annotate_snippets::snippet::{Annotation, AnnotationType, Slice, Snippet, SourceAnnotation};
use base_db::input::LineIndex;
use base_db::libs::LibId;
use base_db::{SourceDatabase, SourceDatabaseExt};
use hir::db::DefDatabase;
use hir::diagnostic::{Diagnostic, DiagnosticSink};
use relative_path::RelativePath;
use std::io;
use syntax::SyntaxError;

pub fn emit_diagnostics(db: &RootDatabase, lib: LibId, writer: &mut dyn io::Write) -> io::Result<bool> {
    let mut has_error = false;
    let def_map = db.def_map(lib);

    for (module_id, module) in def_map.modules() {
        let file_id = module.origin.file_id;
        let parse = db.parse(file_id);
        let source_root = db.file_source_root(file_id);
        let source_root = db.source_root(source_root);
        let source_path = source_root.relative_path(file_id);
        let source_code = db.file_text(file_id);
        let line_index = db.line_index(file_id);

        for err in parse.errors().iter() {
            emit_syntax_error(err, source_path, &source_code, &line_index, writer)?;
            has_error = true;
        }

        let mut diagnostic_sink = DiagnosticSink::new(|d| {
            has_error = true;
            emit_hir_diagnostic(d, source_path, &source_code, &line_index, writer);
        });

        for diag in def_map.diagnostics() {
            diag.add_to(db, module_id, &mut diagnostic_sink);
        }
    }

    Ok(has_error)
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
    let range = err.range();
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
                    Into::<usize>::into(range.end()) - line_offset + 1,
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
    source_path: &RelativePath,
    source_code: &str,
    line_index: &LineIndex,
    writer: &mut dyn io::Write,
) -> io::Result<()> {
    let message = diag.message();
    let range = diag.display_source().value.range();
    let line = line_index.line_col(range.start()).line;
    let line_offset = line_index.line_offset(line);

    let snippet = Snippet {
        title: Some(Annotation {
            id: None,
            label: Some(&message),
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
                label: "",
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
