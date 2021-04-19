use crate::db::HirDatabase;
use crate::source_analyzer::SourceAnalyzer;
use crate::PathResolution;
use base_db::input::FileId;
use hir_def::in_file::InFile;
use rustc_hash::FxHashMap;
use std::cell::RefCell;
use std::fmt;
use syntax::ast;
use syntax::AstNode as _;
use syntax::SyntaxNode;

pub struct Semantics<'db, DB> {
    pub db: &'db DB,
    imp: SemanticsImpl<'db>,
}

pub struct SemanticsImpl<'db> {
    pub db: &'db dyn HirDatabase,
    cache: RefCell<FxHashMap<SyntaxNode, FileId>>,
}

impl<DB> fmt::Debug for Semantics<'_, DB> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Semantics {{ ... }}")
    }
}

impl<'db, DB: HirDatabase> Semantics<'db, DB> {
    pub fn new(db: &'db DB) -> Self {
        let imp = SemanticsImpl::new(db);

        Semantics { db, imp }
    }

    pub fn parse(&self, file_id: FileId) -> ast::Module {
        self.imp.parse(file_id)
    }

    pub fn resolve_path(&self, path: &ast::Path) -> Option<PathResolution> {
        self.imp.resolve_path(path)
    }
}

impl<'db> SemanticsImpl<'db> {
    fn new(db: &'db dyn HirDatabase) -> Self {
        SemanticsImpl {
            db,
            cache: RefCell::default(),
        }
    }

    fn parse(&self, file_id: FileId) -> ast::Module {
        self.db.parse(file_id).tree()
    }

    fn resolve_path(&self, path: &ast::Path) -> Option<PathResolution> {
        self.analyze(path.syntax()).resolve_path(self.db, path)
    }

    fn analyze(&self, node: &SyntaxNode) -> SourceAnalyzer {
        self.analyze_impl(node, None)
    }

    fn analyze_impl(&self, node: &SyntaxNode, offset: Option<syntax::TextSize>) -> SourceAnalyzer {
        let node = self.find_file(node.clone()).as_ref();

        unimplemented!();
    }

    fn cache(&self, root_node: SyntaxNode, file_id: FileId) {
        assert!(root_node.parent().is_none());
        let mut cache = self.cache.borrow_mut();
        let prev = cache.insert(root_node, file_id);
        assert!(prev == None || prev == Some(file_id));
    }

    fn lookup(&self, root_node: &SyntaxNode) -> Option<FileId> {
        self.cache.borrow().get(root_node).copied()
    }

    fn find_file(&self, node: SyntaxNode) -> InFile<SyntaxNode> {
        let root_node = find_root(&node);
        let file_id = self.lookup(&root_node).unwrap();

        InFile::new(file_id, node)
    }
}

fn find_root(node: &SyntaxNode) -> SyntaxNode {
    node.ancestors().last().unwrap()
}
