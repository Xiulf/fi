use diagnostics::{Diagnostic, ToDiagnostic};
use syntax::ast::AstNode;
use syntax::ptr::AstPtr;
use vfs::File;

use crate::name::Name;
use crate::Db;

pub struct UnknownExport {
    pub file: File,
    pub ast: AstPtr<syntax::ast::NameRef>,
    pub name: Name,
}

impl ToDiagnostic for UnknownExport {
    type Db<'t> = dyn Db + 't;

    fn to_diagnostic(self, db: &Self::Db<'_>) -> Diagnostic {
        let root = base_db::parse(db, self.file);
        let node = self.ast.to_node(root.syntax());

        Diagnostic::new(
            format!("unknown export '{}'", self.name.display(db)),
            self.file,
            node.syntax().text_range(),
        )
    }
}
