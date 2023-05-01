use diagnostics::{Diagnostic, ToDiagnostic};
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
        let mut kind = match self.ns {
            | Namespace::Types => "type",
            | Namespace::Values => "value",
            | Namespace::Modules => "module",
        };

        if let Some(name) = self.path.as_name() {
            if name.is_symbol(db) {
                kind = "operator";
            }
        }

        Diagnostic::new(
            format!("unknown {kind} '{}'", self.path.display(db)),
            self.file,
            self.ast.syntax_node_ptr().range(),
        )
    }
}
