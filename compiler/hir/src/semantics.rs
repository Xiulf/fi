use std::cell::RefCell;
use std::fmt;

use base_db::input::FileId;
use hir_def::in_file::InFile;
use hir_def::resolver::{HasResolver, Resolver};
use hir_ty::ty::Ty;
use rustc_hash::FxHashMap;
use syntax::{ast, AstNode as _, SyntaxNode, TextSize};

use crate::db::HirDatabase;
use crate::source_analyzer::SourceAnalyzer;
use crate::source_to_def::{ChildContainer, SourceToDefCache, SourceToDefCtx, ToDef};
use crate::{ModuleDef, PathResolution};

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

    pub fn parse(&self, file_id: FileId) -> ast::SourceFile {
        self.imp.parse(file_id)
    }

    pub fn find_node_at_offset<N: ast::AstNode>(&self, node: &SyntaxNode, offset: TextSize) -> Option<N> {
        self.imp.find_node_at_offset(node, offset)
    }

    pub fn resolve_path(&self, path: &ast::Path) -> Option<PathResolution> {
        self.imp.resolve_path(path)
    }

    pub fn resolve_ident(&self, name_ref: &ast::NameRef) -> Option<PathResolution> {
        self.imp.resolve_ident(name_ref)
    }

    pub fn type_of_expr(&self, expr: &ast::Expr) -> Option<Ty> {
        self.imp.type_of_expr(expr)
    }

    pub fn type_of_pat(&self, pat: &ast::Pat) -> Option<Ty> {
        self.imp.type_of_pat(pat)
    }

    pub fn kind_of(&self, ty: &ast::Type) -> Option<Ty> {
        self.imp.kind_of(ty)
    }

    pub fn to_def<T: ToDef>(&self, src: &T) -> Option<T::Def> {
        let src = self.imp.find_file(src.syntax().clone()).with_value(src).cloned();
        T::to_def(&self.imp, src)
    }

    pub fn resolve_bind_pat_to_const(&self, pat: &ast::PatBind) -> Option<ModuleDef> {
        self.imp.resolve_bind_pat_to_const(pat)
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

    fn parse(&self, file_id: FileId) -> ast::SourceFile {
        let tree = self.db.parse(file_id).tree();

        self.cache(tree.syntax().clone(), file_id);
        tree
    }

    fn find_node_at_offset<N: ast::AstNode>(&self, node: &SyntaxNode, offset: TextSize) -> Option<N> {
        syntax::find_node_at_offset(node, offset)
    }

    fn resolve_path(&self, path: &ast::Path) -> Option<PathResolution> {
        self.analyze(path.syntax()).resolve_path(self.db, path)
    }

    fn resolve_ident(&self, name_ref: &ast::NameRef) -> Option<PathResolution> {
        self.analyze(name_ref.syntax()).resolve_ident(self.db, name_ref)
    }

    fn type_of_expr(&self, expr: &ast::Expr) -> Option<Ty> {
        self.analyze(expr.syntax()).type_of_expr(expr)
    }

    fn type_of_pat(&self, pat: &ast::Pat) -> Option<Ty> {
        self.analyze(pat.syntax()).type_of_pat(pat)
    }

    fn kind_of(&self, ty: &ast::Type) -> Option<Ty> {
        self.analyze(ty.syntax()).kind_of(self.db, ty)
    }

    fn resolve_bind_pat_to_const(&self, pat: &ast::PatBind) -> Option<ModuleDef> {
        self.analyze(pat.syntax()).resolve_bind_pat_to_const(self.db, pat)
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

    fn _analyze_with_offset(&self, node: &SyntaxNode, offset: TextSize) -> SourceAnalyzer {
        self.analyze_impl(node, Some(offset))
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
            | ChildContainer::DefwithBodyId(def) => return SourceAnalyzer::new_for_body(self.db, def, node, offset),
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

macro_rules! impl_to_def {
    ($(($def:path, $ast:path, $method:ident)),* ,) => {$(
        impl ToDef for $ast {
            type Def = $def;

            fn to_def(sema: &SemanticsImpl, src: InFile<Self>) -> Option<Self::Def> {
                sema.with_ctx(|ctx| ctx.$method(src)).map(<$def>::from)
            }
        }
    )*};
}

impl_to_def! {
    (crate::Module, ast::ItemModule, module_to_def),
    (crate::Func, ast::ItemFunc, func_to_def),
    (crate::Local, ast::PatBind, pat_bind_to_def),
}
