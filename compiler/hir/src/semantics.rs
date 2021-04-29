use crate::db::HirDatabase;
use crate::source_analyzer::SourceAnalyzer;
use crate::source_to_def::{ChildContainer, SourceToDefCache, SourceToDefCtx};
use crate::PathResolution;
use base_db::input::FileId;
use hir_def::in_file::InFile;
use hir_def::resolver::{HasResolver, Resolver};
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
    s2d_cache: RefCell<SourceToDefCache>,
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

    pub fn parse_path(&self, file_id: FileId) -> ast::Path {
        self.imp.parse_path(file_id)
    }

    pub fn resolve_path(&self, path: &ast::Path) -> Option<PathResolution> {
        self.imp.resolve_path(path)
    }

    pub fn resolve_path_in(&self, path: &ast::Path, file: FileId) -> Option<PathResolution> {
        self.imp.resolve_path_in(path, file)
    }
}

impl<'db> SemanticsImpl<'db> {
    fn new(db: &'db dyn HirDatabase) -> Self {
        SemanticsImpl {
            db,
            s2d_cache: Default::default(),
            cache: RefCell::default(),
        }
    }

    fn parse(&self, file_id: FileId) -> ast::Module {
        let tree = self.db.parse(file_id).tree();

        self.cache(tree.syntax().clone(), file_id);
        tree
    }

    fn parse_path(&self, file_id: FileId) -> ast::Path {
        let tree = self.db.parse_path(file_id).tree();

        self.cache(tree.syntax().clone(), file_id);
        tree
    }

    fn resolve_path(&self, path: &ast::Path) -> Option<PathResolution> {
        self.analyze(path.syntax()).resolve_path(self.db, path)
    }

    fn resolve_path_in(&self, path: &ast::Path, file: FileId) -> Option<PathResolution> {
        self.analyze_file(file).resolve_path(self.db, path)
    }

    fn with_ctx<T>(&self, f: impl FnOnce(&mut SourceToDefCtx) -> T) -> T {
        let mut cache = self.s2d_cache.borrow_mut();
        let mut ctx = SourceToDefCtx {
            db: self.db,
            cache: &mut *cache,
        };

        f(&mut ctx)
    }

    fn analyze(&self, node: &SyntaxNode) -> SourceAnalyzer {
        self.analyze_impl(node, None)
    }

    fn analyze_file(&self, file: FileId) -> SourceAnalyzer {
        let lib = self.db.file_lib(file);
        let def_map = self.db.def_map(lib);
        let module = match def_map.modules_for_file(file).next() {
            | Some(it) => def_map.module_id(it),
            | None => return SourceAnalyzer::new_for_resolver(Resolver::default(), file),
        };

        let resolver = module.resolver(self.db.upcast());

        SourceAnalyzer::new_for_resolver(resolver, file)
    }

    fn analyze_impl(&self, node: &SyntaxNode, offset: Option<syntax::TextSize>) -> SourceAnalyzer {
        let node = self.find_file(node.clone());
        let node = node.as_ref();
        let container = match self.with_ctx(|ctx| ctx.find_container(node)) {
            | Some(it) => it,
            | None => return SourceAnalyzer::new_for_resolver(Resolver::default(), node.file_id),
        };

        let resolver = match container {
            | ChildContainer::ModuleId(m) => m.resolver(self.db.upcast()),
            | ChildContainer::DefwithBodyId(def) => def.resolver(self.db.upcast()),
        };

        SourceAnalyzer::new_for_resolver(resolver, node.file_id)
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
