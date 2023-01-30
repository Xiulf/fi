use diagnostics::{Diagnostic, ToDiagnostic};
use syntax::ast::AstNode;
use syntax::ptr::AstPtr;
use vfs::File;

use crate::name::Name;
use crate::per_ns::Namespace;
use crate::Db;

pub struct UnknownName {
    pub file: File,
    pub ast: AstPtr<syntax::ast::NameRef>,
    pub name: Name,
    pub ns: Namespace,
}

impl ToDiagnostic for UnknownName {
    type Db<'t> = dyn Db + 't;

    fn to_diagnostic(self, db: &Self::Db<'_>) -> Diagnostic {
        let ns = match self.ns {
            | Namespace::Types => "type",
            | Namespace::Values => "value",
            | Namespace::Modules => "module",
        };

        let root = base_db::parse(db, self.file);
        let node = self.ast.to_node(root.syntax());

        Diagnostic::new(
            format!("unknown {ns} '{}'", self.name.display(db)),
            self.file,
            node.syntax().text_range(),
        )
    }
}
