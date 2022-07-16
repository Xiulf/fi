use hir::diagnostic::Diagnostic as _;
use hir::display::HirDisplay;

use super::*;

pub struct MismatchedKind<'db, 'd, DB: hir::db::HirDatabase> {
    db: &'db DB,
    diag: &'d hir::diagnostic::MismatchedKind,
}

impl<'db, 'd, DB: hir::db::HirDatabase> Diagnostic for MismatchedKind<'db, 'd, DB> {
    fn title(&self) -> String {
        format!(
            "expected kind `{}`, found `{}`",
            self.diag.expected.display(self.db),
            self.diag.found.display(self.db)
        )
    }

    fn range(&self) -> TextRange {
        self.diag.display_source().value.range()
    }

    fn primary_annotation(&self) -> Option<SourceAnnotation> {
        Some(SourceAnnotation {
            range: self.diag.found_src.value.range(),
            message: format!("kind `{}` found here", self.diag.found.display(self.db)),
        })
    }

    fn secondary_annotations(&self) -> Vec<SecondaryAnnotation> {
        let mut annotations = Vec::new();

        if let Some(expected) = self.diag.expected_src {
            annotations.push(SecondaryAnnotation {
                range: expected.map(|s| s.range()),
                message: format!("expected kind `{}`", self.diag.expected.display(self.db)),
            });
        }

        annotations
    }
}

impl<'db, 'd, DB: hir::db::HirDatabase> MismatchedKind<'db, 'd, DB> {
    pub fn new(db: &'db DB, diag: &'d hir::diagnostic::MismatchedKind) -> Self {
        Self { db, diag }
    }
}
