use crate::db::RootDatabase;
use annotate_snippets::display_list::{DisplayList, FormatOptions};
use annotate_snippets::snippet::{Annotation, AnnotationType, Slice, Snippet, SourceAnnotation};
use base_db::input::LineIndex;
use base_db::{SourceDatabase, SourceDatabaseExt};
use hir::db::DefDatabase;
use std::io;
use syntax::SyntaxError;

pub fn emit_diagnostics(db: &RootDatabase, writer: &mut impl io::Write) -> io::Result<bool> {
    let mut has_error = false;

    for lib in db.libs().iter() {
        let def_map = db.def_map(lib);

        for (_, module) in def_map.modules() {
            let file_id = module.origin.file_id;
            let parse = db.parse(file_id);
            let source_code = db.file_text(file_id);
            let line_index = db.line_index(file_id);

            for err in parse.errors().iter() {
                emit_syntax_error(err, &source_code, &line_index, writer)?;
                has_error = true;
            }
        }
    }

    Ok(has_error)
}

fn emit_syntax_error(err: &SyntaxError, source_code: &str, line_index: &LineIndex, writer: &mut impl io::Write) -> io::Result<()> {
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
            origin: None,
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
        opt: FormatOptions {
            color: true,
            anonymized_line_numbers: false,
            margin: None,
        },
    };

    let dl = DisplayList::from(snippet);

    writeln!(writer, "{}\n", dl)
}
