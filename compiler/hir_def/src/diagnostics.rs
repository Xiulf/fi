use crate::ast_id::AstId;
use crate::db::DefDatabase;
use crate::diagnostic::{Diagnostic, DiagnosticSink};
use crate::id::LocalModuleId;
use crate::in_file::InFile;
use base_db::input::FileId;
use std::any::Any;
use syntax::ast;
use syntax::ptr::{AstPtr, SyntaxNodePtr};

#[derive(Debug, PartialEq, Eq)]
pub struct DefDiagnostic {
    pub in_module: LocalModuleId,
    pub kind: DefDiagnosticKind,
}

#[derive(Debug, PartialEq, Eq)]
pub enum DefDiagnosticKind {
    UnresolvedImport { ast: AstId<ast::ItemImport>, index: usize },
}

impl DefDiagnostic {
    pub fn unresolved_import(container: LocalModuleId, ast: AstId<ast::ItemImport>, index: usize) -> Self {
        DefDiagnostic {
            in_module: container,
            kind: DefDiagnosticKind::UnresolvedImport { ast, index },
        }
    }

    pub fn add_to(&self, db: &dyn DefDatabase, module: LocalModuleId, sink: &mut DiagnosticSink) {
        if self.in_module != module {
            return;
        }

        match &self.kind {
            | DefDiagnosticKind::UnresolvedImport { ast, index } => {
                let item = ast.to_node(db);
                let mut curr = 0;
                let mut name = None;

                crate::path::ModPath::expand_import(InFile::new(ast.file_id, item.clone()), |_, import, _, _, _| {
                    if curr == *index {
                        name = import;
                    }

                    curr += 1;
                });

                if let Some(name) = name {
                    sink.push(UnresolvedImport {
                        file: ast.file_id,
                        node: AstPtr::new(&name),
                    });
                } else {
                    sink.push(UnresolvedImport {
                        file: ast.file_id,
                        node: AstPtr::new(&item.path().unwrap().segments().last().unwrap().name_ref().unwrap()),
                    });
                }
            },
        }
    }
}

#[derive(Debug)]
pub struct UnresolvedImport {
    pub file: FileId,
    pub node: AstPtr<ast::NameRef>,
}

impl Diagnostic for UnresolvedImport {
    fn message(&self) -> String {
        "unresolved import".to_string()
    }

    fn display_source(&self) -> InFile<SyntaxNodePtr> {
        InFile::new(self.file, self.node.clone().into())
    }

    fn as_any(&self) -> &(dyn Any + Send + 'static) {
        self
    }
}
