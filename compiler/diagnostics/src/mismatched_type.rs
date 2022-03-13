use super::*;
use hir::diagnostic::Diagnostic as _;
use hir::display::HirDisplay;

pub struct MismatchedType<'db, 'd, DB: hir::db::HirDatabase> {
    db: &'db DB,
    diag: &'d hir::diagnostic::MismatchedType,
}

impl<'db, 'd, DB: hir::db::HirDatabase> Diagnostic for MismatchedType<'db, 'd, DB> {
    fn title(&self) -> String {
        if self.diag.found_src.is_some() {
            format!(
                "expected type `{}`, found `{}`",
                self.diag.expected.display(self.db),
                self.diag.found.display(self.db)
            )
        } else {
            "mismatched types".into()
        }
    }

    fn range(&self) -> TextRange {
        self.diag.display_source().value.range()
    }

    fn primary_annotation(&self) -> Option<SourceAnnotation> {
        if let Some(found_src) = self.diag.found_src {
            if found_src.file_id == self.diag.display_source().file_id {
                return Some(SourceAnnotation {
                    range: found_src.value.range(),
                    message: format!("type `{}` found here", self.diag.found.display(self.db)),
                });
            }
        }

        Some(SourceAnnotation {
            range: self.diag.display_source().value.range(),
            message: format!(
                "expected type `{}`, found `{}`",
                self.diag.expected.display(self.db),
                self.diag.found.display(self.db)
            ),
        })
    }

    fn secondary_annotations(&self) -> Vec<SecondaryAnnotation> {
        let mut annotations = Vec::new();

        if let Some(found) = self.diag.found_src {
            if found.file_id != self.diag.display_source().file_id {
                annotations.push(SecondaryAnnotation {
                    range: found.map(|s| s.range()),
                    message: format!("type `{}` found here", self.diag.found.display(self.db)),
                });
            }
        }

        if let Some(expected) = self.diag.expected_src {
            annotations.push(SecondaryAnnotation {
                range: expected.map(|s| s.range()),
                message: format!("type `{}` specified here", self.diag.expected.display(self.db)),
            });
        }

        annotations
    }
}

impl<'db, 'd, DB: hir::db::HirDatabase> MismatchedType<'db, 'd, DB> {
    pub fn new(db: &'db DB, diag: &'d hir::diagnostic::MismatchedType) -> Self {
        Self { db, diag }
    }
}
