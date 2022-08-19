use std::io;

use ariadne::{Cache, Color, ColorGenerator, Label, Report, ReportKind, Source};
use base_db::input::FileId;
use base_db::{SourceDatabase, SourceDatabaseExt};
use diagnostics::{DiagnosticForWith, Level};
use hir::db::HirDatabase;
use hir::diagnostic::{Diagnostic, DiagnosticSink};
use rustc_hash::FxHashMap;
use syntax::SyntaxError;

use crate::db::RootDatabase;

pub fn emit_diagnostics(db: &RootDatabase, lib: hir::Lib, mut writer: impl io::Write) -> io::Result<usize> {
    let mut errors = 0;
    let mut cache = DbCache {
        db,
        files: FxHashMap::default(),
    };

    let config = ariadne::Config::default();

    for module in lib.modules(db) {
        let file_id = module.file_id(db);
        let parse = db.parse(file_id);

        for err in parse.errors().iter() {
            errors += 1;
            emit_syntax_error(err, file_id, config, &mut cache, &mut writer)?;
        }

        let mut error = None;
        let mut diagnostic_sink = DiagnosticSink::new(|d| {
            errors += 1;

            if let Err(e) = emit_hir_diagnostic(d, db, file_id, config, &mut cache, &mut writer) {
                error = Some(e);
            }
        });

        module.diagnostics(db, &mut diagnostic_sink);
        drop(diagnostic_sink);

        if let Some(e) = error {
            return Err(e);
        }
    }

    Ok(errors)
}

fn emit_syntax_error(
    err: &SyntaxError,
    file_id: FileId,
    config: ariadne::Config,
    cache: impl Cache<FileId>,
    mut writer: impl io::Write,
) -> io::Result<()> {
    let span = (file_id, usize::from(err.range.start())..usize::from(err.range.end()));

    Report::build(ReportKind::Error, file_id, err.range.start().into())
        .with_message("syntax error")
        .with_label(Label::new(span).with_message(&err.msg).with_color(Color::Red))
        .with_config(config)
        .finish()
        .write(cache, &mut writer)?;

    writeln!(writer)
}

fn emit_hir_diagnostic(
    diag: &dyn Diagnostic,
    db: &impl HirDatabase,
    file_id: FileId,
    config: ariadne::Config,
    cache: impl Cache<FileId>,
    writer: impl io::Write,
) -> io::Result<()> {
    diag.with_diagnostic(db, |diag| emit_diagnostic(diag, file_id, config, cache, writer))
}

fn emit_diagnostic(
    d: &dyn diagnostics::Diagnostic,
    file_id: FileId,
    config: ariadne::Config,
    cache: impl Cache<FileId>,
    mut writer: impl io::Write,
) -> io::Result<()> {
    let (report_kind, color) = match d.level() {
        | Level::Error => (ReportKind::Error, Color::Red),
        | Level::Warning => (ReportKind::Warning, Color::Yellow),
        | Level::Info => (ReportKind::Advice, Color::Blue),
    };

    let mut colors = ColorGenerator::new();
    let mut builder = Report::build(report_kind, file_id, d.range().start().into())
        .with_config(config)
        .with_message(d.title());

    if let Some(ann) = d.primary_annotation() {
        let span = (file_id, usize::from(ann.range.start())..usize::from(ann.range.end()));

        builder = builder.with_label(Label::new(span).with_message(ann.message).with_color(color));
    } else {
        let span = (file_id, usize::from(d.range().start())..usize::from(d.range().end()));

        builder = builder.with_label(Label::new(span).with_color(color));
    }

    for ann in d.secondary_annotations() {
        let color = colors.next();
        let range = ann.range.value;
        let span = (ann.range.file_id, usize::from(range.start())..usize::from(range.end()));

        builder = builder.with_label(Label::new(span).with_message(ann.message).with_color(color));
    }

    for note in d.notes() {
        builder = builder.with_note(note);
    }

    builder.finish().write(cache, &mut writer)?;
    writeln!(writer)
}

struct DbCache<'db> {
    db: &'db RootDatabase,
    files: FxHashMap<FileId, Source>,
}

impl<'db> Cache<FileId> for DbCache<'db> {
    fn fetch(&mut self, id: &FileId) -> Result<&Source, Box<dyn std::fmt::Debug + '_>> {
        if let Some(source) = self.files.get(id) {
            return Ok(source);
        }

        let source = Source::from(&*self.db.file_text(*id));

        unsafe {
            let files = &self.files as *const _ as *mut FxHashMap<FileId, Source>;
            (&mut *files).insert(*id, source);
        }

        Ok(&self.files[id])
    }

    fn display<'a>(&self, id: &'a FileId) -> Option<Box<dyn std::fmt::Display + 'a>> {
        let source_root = self.db.file_source_root(*id);
        let source_root = self.db.source_root(source_root);
        let path = source_root.relative_path(*id);

        Some(Box::new(path.to_relative_path_buf()))
    }
}
