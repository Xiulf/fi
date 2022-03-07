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

    fn primary_annotation(&self) -> Option<SourceAnnotation> {
        match self.diag.ctnt.types.len() {
            | 0 => None,
            | 1 => Some(SourceAnnotation {
                range: self.diag.display_source().value.range(),
                message: format!(
                    "type `{}` is not a member of class `{}`",
                    self.diag.ctnt.types[0].display(self.db),
                    self.db.class_data(self.diag.ctnt.class).name
                ),
            }),
            | _ => Some(SourceAnnotation {
                range: self.diag.display_source().value.range(),
                message: format!(
                    "types `{}` are not a member of class `{}`",
                    self.diag
                        .ctnt
                        .types
                        .iter()
                        .map(|t| format!("{}", t.display(self.db)))
                        .collect::<Vec<_>>()
                        .join(" "),
                    self.db.class_data(self.diag.ctnt.class).name
                ),
            }),
        }
    }

    fn secondary_annotations(&self) -> Vec<SecondaryAnnotation> {
        vec![]
    }
}

impl<'db, 'd, DB: hir::db::HirDatabase> UnsolvedConstraint<'db, 'd, DB> {
    pub fn new(db: &'db DB, diag: &'d hir::diagnostic::UnsolvedConstraint) -> Self {
        Self { db, diag }
    }
}
