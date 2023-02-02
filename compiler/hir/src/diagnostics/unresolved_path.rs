use diagnostics::{Diagnostic, ToDiagnostic};
use syntax::ast::AstNode;
use syntax::ptr::AstPtr;
use vfs::File;

use crate::path::Path;
use crate::per_ns::Namespace;
use crate::Db;

pub struct UnresolvedPath {
    pub file: File,
    pub ast: AstPtr<syntax::ast::Path>,
    pub path: Path,
    pub ns: Namespace,
}

impl ToDiagnostic for UnresolvedPath {
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
            format!("unknown {ns} '{}'", self.path.display(db)),
            self.file,
            node.syntax().text_range(),
        )
    }
}
