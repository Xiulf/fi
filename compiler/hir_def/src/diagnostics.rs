use crate::ast_id::AstId;
use crate::db::DefDatabase;
use crate::diagnostic::{Diagnostic, DiagnosticSink};
use crate::id::LocalModuleId;
use crate::in_file::InFile;
use crate::item_tree::{Item, ItemTree};
use crate::name::Name;
use base_db::input::FileId;
use std::any::Any;
use syntax::ptr::{AstPtr, SyntaxNodePtr};
use syntax::{ast, AstNode};

#[derive(Debug, PartialEq, Eq)]
pub struct DefDiagnostic {
    pub in_module: LocalModuleId,
    pub kind: DefDiagnosticKind,
}

#[derive(Debug, PartialEq, Eq)]
pub enum DefDiagnosticKind {
    UnresolvedImport { ast: AstId<ast::ItemImport>, index: usize },
}

#[derive(Debug, PartialEq, Eq)]
pub enum ItemTreeDiagnostic {
    DuplicateDeclaration { name: Name, first: Item, second: Item },
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
                        node: AstPtr::new(&item.path().unwrap().segments().last().unwrap().name_ref().unwrap()),
                    });
                }
            },
        }
    }
}

impl ItemTreeDiagnostic {
    pub fn add_to(&self, db: &dyn DefDatabase, item_tree: &ItemTree, sink: &mut DiagnosticSink) {
        match self {
            | Self::DuplicateDeclaration { name, first, second } => sink.push(DuplicateDeclaration {
                file: item_tree.file,
                name: name.to_string(),
                first: ast_ptr_from_mod(db, item_tree, *first),
                second: ast_ptr_from_mod(db, item_tree, *second),
            }),
        }

        fn ast_ptr_from_mod(db: &dyn DefDatabase, item_tree: &ItemTree, item: Item) -> SyntaxNodePtr {
            match item {
                | Item::Import(it) => SyntaxNodePtr::new(item_tree.source(db, it).syntax()),
                | Item::Fixity(it) => SyntaxNodePtr::new(item_tree.source(db, it).syntax()),
                | Item::Func(it) => SyntaxNodePtr::new(item_tree.source(db, it).syntax()),
                | Item::Static(it) => SyntaxNodePtr::new(item_tree.source(db, it).syntax()),
                | Item::Const(it) => SyntaxNodePtr::new(item_tree.source(db, it).syntax()),
                | Item::TypeCtor(it) => SyntaxNodePtr::new(item_tree.source(db, it).syntax()),
                | Item::TypeAlias(it) => SyntaxNodePtr::new(item_tree.source(db, it).syntax()),
                | Item::Class(it) => SyntaxNodePtr::new(item_tree.source(db, it).syntax()),
                | Item::Instance(it) => SyntaxNodePtr::new(item_tree.source(db, it).syntax()),
            }
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
pub struct DuplicateDeclaration {
    pub file: FileId,
    pub name: String,
    pub first: SyntaxNodePtr,
    pub second: SyntaxNodePtr,
}

impl Diagnostic for DuplicateDeclaration {
    fn message(&self) -> String {
        format!("the name `{}` is declared multiple times", self.name)
    }

    fn display_source(&self) -> InFile<SyntaxNodePtr> {
        InFile::new(self.file, self.second)
    }

    fn as_any(&self) -> &(dyn Any + Send + 'static) {
        self
    }
}
