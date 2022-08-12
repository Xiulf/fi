use hir::diagnostic::Diagnostic as _;
use hir::{Const, Ctor, Fixity, Func, HirDisplay, Static, ValueNs};

use super::*;

pub struct ValueHole<'db, 'd, DB: hir::db::HirDatabase> {
    db: &'db DB,
    diag: &'d hir::diagnostic::ValueHole,
}

impl<'db, 'd, DB: hir::db::HirDatabase> Diagnostic for ValueHole<'db, 'd, DB> {
    fn title(&self) -> String {
        "missing value".into()
    }

    fn range(&self) -> TextRange {
        self.diag.display_source().value.range()
    }

    fn primary_annotation(&self) -> Option<SourceAnnotation> {
        Some(SourceAnnotation {
            range: self.diag.display_source().value.range(),
            message: format!("hole of type `{}`", self.diag.ty.display(self.db)),
        })
    }

    fn notes(&self) -> Vec<String> {
        let found = self
            .diag
            .search
            .results
            .iter()
            .map(|&ns| match ns {
                | ValueNs::Local(id) => String::new(),
                | ValueNs::Fixity(id) => Fixity::from(id).path(self.db).to_string(),
                | ValueNs::Func(id) => Func::from(id).path(self.db).to_string(),
                | ValueNs::Static(id) => Static::from(id).path(self.db).to_string(),
                | ValueNs::Const(id) => Const::from(id).path(self.db).to_string(),
                | ValueNs::Ctor(id) => Ctor::from(id).path(self.db).to_string(),
            })
            .collect::<Vec<_>>();

        vec![format!("available values: {}", found.join(", "))]
    }
}

impl<'db, 'd, DB: hir::db::HirDatabase> ValueHole<'db, 'd, DB> {
    pub fn new(db: &'db DB, diag: &'d hir::diagnostic::ValueHole) -> Self {
        Self { db, diag }
    }
}
