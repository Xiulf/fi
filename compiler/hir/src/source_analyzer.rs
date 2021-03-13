use crate::db::HirDatabase;
use crate::PathResolution;
use base_db::input::FileId;
use hir_def::path::ModPath;
use syntax::ast;
use syntax::AstNode;

type Resolver = ();

#[derive(Debug)]
pub(crate) struct SourceAnalyzer {
    pub(crate) file_id: FileId,
    pub(crate) resolver: Resolver,
}

impl SourceAnalyzer {
    pub fn resolve_path(&self, db: &dyn HirDatabase, path: &ast::Path) -> Option<PathResolution> {
        let parent = || path.syntax().parent();
        let hir_path = ModPath::lower(path.clone());

        resolve_hir_path(db, &self.resolver, &hir_path)
    }
}

fn resolve_hir_path(db: &dyn HirDatabase, resolver: &Resolver, path: &ModPath) -> Option<PathResolution> {
}
