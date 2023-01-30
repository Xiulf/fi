use std::collections::HashMap;

use ::diagnostics::{Diagnostic, Diagnostics, Level};
use ariadne::{Cache, Color, ColorGenerator, Label, Report, ReportKind, Source};
use paths::AbsPathBuf;
use vfs::{File, VfsPath};

use super::*;
use crate::db::Database;

impl Driver {
    pub fn report_diagnostics(&self, lib: LibId) -> io::Result<bool> {
        let mut cache = DbCache {
            db: &self.db,
            files: Default::default(),
        };

        let mut n_errors = 0;
        for file in lib.source_root(&self.db).iter(&self.db) {
            base_db::parse(&self.db, file);
            let diagnostics = base_db::parse::accumulated::<Diagnostics>(&self.db, file);
            n_errors += diagnostics.len();
            for diag in diagnostics {
                self.report_diagnostic(&mut cache, diag)?;
            }
        }

        if n_errors > 0 {
            return Ok(true);
        }

        hir::def_map::query(&self.db, lib);
        let diagnostics = hir::def_map::query::accumulated::<Diagnostics>(&self.db, lib);
        for diag in diagnostics {
            self.report_diagnostic(&mut cache, diag)?;
            n_errors += 1;
        }

        Ok(n_errors > 0)
    }

    fn report_diagnostic(&self, cache: &mut DbCache, diag: Diagnostic) -> io::Result<()> {
        let (report_kind, color) = match diag.level {
            | Level::Error => (ReportKind::Error, Color::Red),
            | Level::Warning => (ReportKind::Warning, Color::Yellow),
            | Level::Info => (ReportKind::Advice, Color::Black),
        };

        let mut colors = ColorGenerator::new();
        let mut report = Report::build(report_kind, diag.file, diag.range.start().into()).with_message(diag.title);

        if let Some(ann) = diag.primary_label {
            let span = (diag.file, usize::from(ann.range.start())..usize::from(ann.range.end()));
            report = report.with_label(Label::new(span).with_message(ann.message).with_color(color));
        } else {
            let span = (
                diag.file,
                usize::from(diag.range.start())..usize::from(diag.range.end()),
            );
            report = report.with_label(Label::new(span).with_color(color));
        }

        for ann in diag.secondary_labels {
            let color = colors.next();
            let range = ann.range.value;
            let span = (ann.range.file, usize::from(range.start())..usize::from(range.end()));
            report.add_label(Label::new(span).with_message(ann.message).with_color(color));
        }

        for note in diag.notes {
            report = report.with_note(note);
        }

        report.finish().eprint(cache)
    }
}

struct DbCache<'db> {
    db: &'db Database,
    files: HashMap<File, Source>,
}

impl<'db> Cache<File> for DbCache<'db> {
    fn fetch(&mut self, id: &File) -> Result<&Source, Box<dyn std::fmt::Debug + '_>> {
        if let Some(source) = self.files.get(id) {
            return Ok(source);
        }

        let text = id.text(self.db).as_deref().unwrap_or_default();
        let source = Source::from(text);

        unsafe {
            let files = &self.files as *const _ as *mut HashMap<File, Source>;
            (&mut *files).insert(*id, source);
        }

        Ok(&self.files[id])
    }

    fn display<'a>(&self, id: &'a File) -> Option<Box<dyn std::fmt::Display + 'a>> {
        let path = id.path(self.db);
        let path = match path {
            | VfsPath::PathBuf(p) => {
                let curr_dir = AbsPathBuf::assert(std::env::current_dir().ok()?.canonicalize().ok()?);

                p.normalize()
                    .strip_prefix(&curr_dir)?
                    .as_ref()
                    .to_string_lossy()
                    .into_owned()
            },
            | VfsPath::Virtual(p) => p.clone().into(),
        };

        Some(Box::new(path))
    }
}
