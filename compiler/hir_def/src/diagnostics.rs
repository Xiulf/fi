use std::any::Any;

use base_db::input::FileId;
use syntax::ast;
use syntax::ptr::{AstPtr, SyntaxNodePtr};

use crate::ast_id::AstId;
use crate::db::DefDatabase;
use crate::diagnostic::{Diagnostic, DiagnosticSink};
use crate::id::LocalModuleId;
use crate::in_file::InFile;
use crate::name::Name;

#[derive(Debug, PartialEq, Eq)]
pub struct DefDiagnostic {
    pub in_module: LocalModuleId,
    pub kind: DefDiagnosticKind,
}

#[derive(Debug, PartialEq, Eq)]
pub enum DefDiagnosticKind {
    DuplicateDeclaration {
        name: Name,
        first: AstId<ast::Item>,
        second: AstId<ast::Item>,
    },
    UnresolvedImport {
        ast: AstId<ast::ItemImport>,
        index: usize,
    },
    PrivateImport {
        ast: AstId<ast::ItemImport>,
        index: usize,
    },
}

impl DefDiagnostic {
    pub fn duplicate_declaration(
        container: LocalModuleId,
        name: Name,
        first: AstId<ast::Item>,
        second: AstId<ast::Item>,
    ) -> Self {
        DefDiagnostic {
            in_module: container,
            kind: DefDiagnosticKind::DuplicateDeclaration { name, first, second },
        }
    }

    pub fn unresolved_import(container: LocalModuleId, ast: AstId<ast::ItemImport>, index: usize) -> Self {
        DefDiagnostic {
            in_module: container,
            kind: DefDiagnosticKind::UnresolvedImport { ast, index },
        }
    }

    pub fn private_import(container: LocalModuleId, ast: AstId<ast::ItemImport>, index: usize) -> Self {
        DefDiagnostic {
            in_module: container,
            kind: DefDiagnosticKind::PrivateImport { ast, index },
        }
    }

    pub fn add_to(&self, db: &dyn DefDatabase, module: LocalModuleId, sink: &mut DiagnosticSink) {
        if self.in_module != module {
            return;
        }

        match &self.kind {
            | DefDiagnosticKind::DuplicateDeclaration { name, first, second } => sink.push(DuplicateDeclaration {
                file: first.file_id,
                name: name.to_string(),
                first: AstPtr::new(&first.to_node(db)),
                second: AstPtr::new(&second.to_node(db)),
            }),
            | DefDiagnosticKind::UnresolvedImport { ast, index } => {
                let item = ast.to_node(db);
                let mut curr = 0;
                let mut name = None;

                crate::path::Path::expand_import(InFile::new(ast.file_id, item.clone()), |_, import, _, _, _| {
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
                        node: AstPtr::new(&item.module().unwrap()),
                    });
                }
            },
            | DefDiagnosticKind::PrivateImport { ast, index } => {
                let item = ast.to_node(db);
                let mut curr = 0;
                let mut name = None;

                crate::path::Path::expand_import(InFile::new(ast.file_id, item.clone()), |_, import, _, _, _| {
                    if curr == *index {
                        name = import;
                    }

                    curr += 1;
                });

                if let Some(name) = name {
                    sink.push(PrivateImport {
                        file: ast.file_id,
                        node: AstPtr::new(&name),
                    });
                } else {
                    sink.push(PrivateImport {
                        file: ast.file_id,
                        node: AstPtr::new(&item.module().unwrap()),
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

#[derive(Debug)]
pub struct PrivateImport {
    pub file: FileId,
    pub node: AstPtr<ast::NameRef>,
}

impl Diagnostic for PrivateImport {
    fn message(&self) -> String {
        "private import".to_string()
    }

    fn display_source(&self) -> InFile<SyntaxNodePtr> {
        InFile::new(self.file, self.node.clone().into())
    }

    fn as_any(&self) -> &(dyn Any + Send + 'static) {
        self
    }
}

#[derive(Debug)]
pub struct DuplicateDeclaration {
    pub file: FileId,
    pub name: String,
    pub first: AstPtr<ast::Item>,
    pub second: AstPtr<ast::Item>,
}

impl Diagnostic for DuplicateDeclaration {
    fn message(&self) -> String {
        format!("the name `{}` is declared multiple times", self.name)
    }

    fn display_source(&self) -> InFile<SyntaxNodePtr> {
        InFile::new(self.file, self.second.syntax_node_ptr())
    }

    fn as_any(&self) -> &(dyn Any + Send + 'static) {
        self
    }
}
