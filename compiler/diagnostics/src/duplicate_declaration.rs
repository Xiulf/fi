use super::*;
use syntax::{ast, AstNode, NameOwner, Parsed, SyntaxKind, SyntaxNodePtr};

pub struct DuplicateDeclaration<'db, 'd, DB: hir::db::HirDatabase> {
    db: &'db DB,
    diag: &'d hir::diagnostic::DuplicateDeclaration,
}

impl<'db, 'd, DB: hir::db::HirDatabase> Diagnostic for DuplicateDeclaration<'db, 'd, DB> {
    fn title(&self) -> String {
        format!("the name `{}` is defined multiple times", self.diag.name)
    }

    fn range(&self) -> TextRange {
        syntax_node_range(self.diag.second, &self.db.parse(self.diag.file))
    }

    fn primary_annotation(&self) -> Option<SourceAnnotation> {
        Some(SourceAnnotation {
            range: syntax_node_range(self.diag.second, &self.db.parse(self.diag.file)),
            message: format!("`{}` redefined here", self.diag.name),
        })
    }

    fn secondary_annotations(&self) -> Vec<SecondaryAnnotation> {
        vec![SecondaryAnnotation {
            range: InFile::new(
                self.diag.file,
                syntax_node_range(self.diag.first, &self.db.parse(self.diag.file)),
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

fn syntax_node_range(ptr: SyntaxNodePtr, parse: &Parsed<ast::Module>) -> TextRange {
    match ptr.kind() {
        | SyntaxKind::ITEM_FUN => ast::ItemFun::cast(ptr.to_node(parse.tree().syntax()))
            .and_then(|i| Some(i.name()?.syntax().text_range()))
            .unwrap_or_else(|| ptr.range()),
        | _ => ptr.range(),
    }
}
