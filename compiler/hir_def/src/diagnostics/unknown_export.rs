use diagnostics::{Diagnostic, ToDiagnostic};
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
        Diagnostic::new(
            format!("unknown export '{}'", self.name.display(db)),
            self.file,
            self.ast.syntax_node_ptr().range(),
        )
    }
}
