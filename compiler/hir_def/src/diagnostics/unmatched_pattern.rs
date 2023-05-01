use diagnostics::{Diagnostic, ToDiagnostic};
use syntax::ptr::AstPtr;
use vfs::File;

use crate::Db;

pub struct UnmatchedPattern {
    pub file: File,
    pub ast: AstPtr<syntax::ast::ExprMatch>,
}

impl ToDiagnostic for UnmatchedPattern {
    type Db<'t> = dyn Db + 't;

    fn to_diagnostic(self, _: &Self::Db<'_>) -> Diagnostic {
        Diagnostic::new("not all cases matched", self.file, self.ast.syntax_node_ptr().range())
    }
}
