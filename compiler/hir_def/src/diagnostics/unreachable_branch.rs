use diagnostics::{Diagnostic, Level, ToDiagnostic};
use syntax::ptr::AstPtr;
use vfs::File;

use crate::Db;

pub struct UnreachableBranch {
    pub file: File,
    pub ast: AstPtr<syntax::ast::MatchArm>,
}

impl ToDiagnostic for UnreachableBranch {
    type Db<'t> = dyn Db + 't;

    fn to_diagnostic(self, _: &Self::Db<'_>) -> Diagnostic {
        Diagnostic::new("unreachable branch", self.file, self.ast.syntax_node_ptr().range()).with_level(Level::Warning)
    }
}
