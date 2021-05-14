use super::*;
use hir::diagnostic::Diagnostic as _;
use hir::display::HirDisplay;

pub struct UnsolvedConstraint<'db, 'd, DB: hir::db::HirDatabase> {
    db: &'db DB,
    diag: &'d hir::diagnostic::UnsolvedConstraint,
}

impl<'db, 'd, DB: hir::db::HirDatabase> Diagnostic for UnsolvedConstraint<'db, 'd, DB> {
    fn title(&self) -> String {
        format!("unsolved constraint `{}`", self.diag.ctnt.display(self.db),)
    }

    fn range(&self) -> TextRange {
        self.diag.display_source().value.range()
    }
}

impl<'db, 'd, DB: hir::db::HirDatabase> UnsolvedConstraint<'db, 'd, DB> {
    pub fn new(db: &'db DB, diag: &'d hir::diagnostic::UnsolvedConstraint) -> Self {
        Self { db, diag }
    }
}
