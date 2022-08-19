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
        if self.diag.search.results.is_empty() {
            return Vec::new();
        }

        let body = self.db.body(self.diag.owner);
        let found = self
            .diag
            .search
            .results
            .iter()
            .map(|&ns| match ns {
                | ValueNs::Local(id) => match body[id] {
                    | hir::Pat::Bind { ref name, .. } => name.clone(),
                    | _ => unreachable!(),
                },
                | ValueNs::Fixity(id) => Fixity::from(id).name(self.db),
                | ValueNs::Func(id) => Func::from(id).name(self.db),
                | ValueNs::Static(id) => Static::from(id).name(self.db),
                | ValueNs::Const(id) => Const::from(id).name(self.db),
                | ValueNs::Ctor(id) => Ctor::from(id).name(self.db),
            })
            .map(|n| n.to_string())
            .collect::<Vec<_>>();

        vec![format!("available values:\n  {}", found.join(", "))]
    }
}

impl<'db, 'd, DB: hir::db::HirDatabase> ValueHole<'db, 'd, DB> {
    pub fn new(db: &'db DB, diag: &'d hir::diagnostic::ValueHole) -> Self {
        Self { db, diag }
    }
}
