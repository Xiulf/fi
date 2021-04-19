use crate::db::HirDatabase;
use crate::PathResolution;
use base_db::input::FileId;
use hir_def::path::Path;
use hir_def::resolver::Resolver;
use syntax::ast;
use syntax::AstNode;

#[derive(Debug)]
pub(crate) struct SourceAnalyzer {
    pub(crate) file_id: FileId,
    pub(crate) resolver: Resolver,
}

impl SourceAnalyzer {
    pub fn resolve_path(&self, db: &dyn HirDatabase, path: &ast::Path) -> Option<PathResolution> {
        let parent = || path.syntax().parent();
        let mut prefer_value_ns = false;

        if let Some(_) = parent().and_then(ast::ExprPath::cast) {
            prefer_value_ns = true;
        }

        let hir_path = Path::lower(path.clone());

        resolve_hir_path_(db, &self.resolver, &hir_path, true)
    }
}

#[inline]
pub(crate) fn resolve_hir_path(db: &dyn HirDatabase, resolver: &Resolver, path: &Path) -> Option<PathResolution> {
    resolve_hir_path_(db, resolver, path, false)
}

fn resolve_hir_path_(
    db: &dyn HirDatabase,
    resolver: &Resolver,
    path: &Path,
    prefer_value_ns: bool,
) -> Option<PathResolution> {
    let types = || -> Option<PathResolution> {
        unimplemented!();
    };

    let values = || -> Option<PathResolution> {
        unimplemented!();
    };

    let items = || -> Option<PathResolution> {
        resolver
            .resolve_path(db.upcast(), path)
            .types
            .map(|it| PathResolution::Def(it.into()))
    };

    if prefer_value_ns {
        values().or_else(types)
    } else {
        types().or_else(values)
    }
    .or_else(items)
}
