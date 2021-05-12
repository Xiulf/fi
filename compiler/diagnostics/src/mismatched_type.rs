use super::*;
use hir::diagnostic::Diagnostic as _;
use hir::display::HirDisplay;

pub struct MismatchedType<'db, 'd, DB: hir::db::HirDatabase> {
    db: &'db DB,
    diag: &'d hir::diagnostic::MismatchedType,
}

impl<'db, 'd, DB: hir::db::HirDatabase> Diagnostic for MismatchedType<'db, 'd, DB> {
    fn title(&self) -> String {
        format!(
            "expected `{}`, found `{}`",
            self.diag.expected.display(self.db),
            self.diag.found.display(self.db)
        )
    }

    fn range(&self) -> TextRange {
        self.diag.display_source().value.range()
    }
}

impl<'db, 'd, DB: hir::db::HirDatabase> MismatchedType<'db, 'd, DB> {
    pub fn new(db: &'db DB, diag: &'d hir::diagnostic::MismatchedType) -> Self {
        Self { db, diag }
    }
}
