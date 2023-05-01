use diagnostics::{Diagnostic, ToDiagnostic};
use syntax::ast::AstNode;
use syntax::ptr::SyntaxNodePtr;
use vfs::InFile;

use crate::ast_id::AstId;
use crate::Db;

pub struct UnresolvedImport {
    pub ast: AstId<syntax::ast::ItemImport>,
    pub index: usize,
}

impl ToDiagnostic for UnresolvedImport {
    type Db<'t> = dyn Db + 't;

    fn to_diagnostic(self, db: &Self::Db<'_>) -> Diagnostic {
        let item = self.ast.to_node(db);
        let mut curr = 0;
        let mut name = None;

        crate::item_tree::lower::expand_import(
            db,
            InFile::new(self.ast.0.file, item.clone()),
            |_, import, _, _, _, _| {
                if curr == self.index {
                    name = import;
                }

                curr += 1;
            },
        );

        let ptr = if let Some(name) = name {
            SyntaxNodePtr::new(name.syntax())
        } else {
            SyntaxNodePtr::new(item.module().unwrap().syntax())
        };

        Diagnostic::new("unresolved import", self.ast.0.file, ptr.range())
    }
}
