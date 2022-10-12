use super::*;

pub struct DuplicateDeclaration<'db, 'd, DB: hir::db::HirDatabase> {
    db: &'db DB,
    diag: &'d hir::diagnostic::DuplicateDeclaration,
}

impl<'db, 'd, DB: hir::db::HirDatabase> Diagnostic for DuplicateDeclaration<'db, 'd, DB> {
    fn title(&self) -> String {
        format!("`{}` is defined multiple times", self.diag.name)
    }

    fn range(&self) -> TextRange {
        item_name(self.diag.second.syntax_node_ptr(), &self.db.parse(self.diag.file))
    }

    fn primary_annotation(&self) -> Option<SourceAnnotation> {
        Some(SourceAnnotation {
            range: item_name(self.diag.second.syntax_node_ptr(), &self.db.parse(self.diag.file)),
            message: format!("`{}` redefined here", self.diag.name),
        })
    }

    fn secondary_annotations(&self) -> Vec<SecondaryAnnotation> {
        vec![SecondaryAnnotation {
            range: InFile::new(
                self.diag.file,
                item_name(self.diag.first.syntax_node_ptr(), &self.db.parse(self.diag.file)),
            ),
            message: format!("previous definition of `{}` here", self.diag.name),
        }]
    }

    fn notes(&self) -> Vec<String> {
        vec![format!("`{}` must be defined only once", self.diag.name)]
    }
}

impl<'db, 'd, DB: hir::db::HirDatabase> DuplicateDeclaration<'db, 'd, DB> {
    pub fn new(db: &'db DB, diag: &'d hir::diagnostic::DuplicateDeclaration) -> Self {
        Self { db, diag }
    }
}
