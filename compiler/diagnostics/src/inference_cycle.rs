use super::*;

pub struct InferenceCycle<'db, 'd, DB: hir::db::HirDatabase> {
    db: &'db DB,
    diag: &'d hir::diagnostic::InferenceCycle,
}

impl<'db, 'd, DB: hir::db::HirDatabase> Diagnostic for InferenceCycle<'db, 'd, DB> {
    fn title(&self) -> String {
        "cycle when inferring type".into()
    }

    fn range(&self) -> TextRange {
        item_name(self.diag.src, &self.db.parse(self.diag.file))
    }

    fn notes(&self) -> Vec<String> {
        vec!["annotate the definition with a type".into()]
    }
}

impl<'db, 'd, DB: hir::db::HirDatabase> InferenceCycle<'db, 'd, DB> {
    pub fn new(db: &'db DB, diag: &'d hir::diagnostic::InferenceCycle) -> Self {
        Self { db, diag }
    }
}
