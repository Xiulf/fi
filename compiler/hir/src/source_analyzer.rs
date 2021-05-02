use crate::db::HirDatabase;
use crate::{Class, Const, Ctor, Fixity, Func, Local, PathResolution, Static, TypeAlias, TypeCtor};
use base_db::input::FileId;
use hir_def::body::{Body, BodySourceMap};
use hir_def::in_file::InFile;
use hir_def::path::Path;
use hir_def::resolver::{Resolver, TypeNs, ValueNs};
use hir_def::scope::ExprScopes;
use std::sync::Arc;
use syntax::ast;
use syntax::{AstNode, SyntaxNode};

#[derive(Debug)]
pub(crate) struct SourceAnalyzer {
    pub(crate) file_id: FileId,
    pub(crate) resolver: Resolver,
    body: Option<Arc<Body>>,
    body_source_map: Option<Arc<BodySourceMap>>,
    scopes: Option<Arc<ExprScopes>>,
}

impl SourceAnalyzer {
    pub(crate) fn new_for_resolver(resolver: Resolver, file_id: FileId) -> Self {
        SourceAnalyzer {
            resolver,
            file_id,
            body: None,
            body_source_map: None,
            scopes: None,
        }
    }

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
    let types = || {
        let (ty, remaining) = resolver.resolve_type(db.upcast(), path)?;
        let (ty, unresolved) = match remaining {
            | Some(remaining) if remaining > 1 => return None,
            | _ => (ty, path.segments().get(1)),
        };

        let res = match ty {
            | TypeNs::TypeAlias(id) => PathResolution::Def(TypeAlias::from(id).into()),
            | TypeNs::TypeCtor(id) => PathResolution::Def(TypeCtor::from(id).into()),
            | TypeNs::Class(id) => PathResolution::Def(Class::from(id).into()),
        };

        match unresolved {
            | Some(_) => None,
            | None => Some(res),
        }
    };

    let body_owner = resolver.body_owner();
    let values = || {
        resolver.resolve_value_fully(db.upcast(), path).and_then(|val| {
            Some(match val {
                | ValueNs::Local(pat_id) => {
                    let var = Local {
                        parent: body_owner?,
                        pat_id,
                    };

                    PathResolution::Local(var)
                },
                | ValueNs::Fixity(id) => PathResolution::Def(Fixity::from(id).into()),
                | ValueNs::Func(id) => PathResolution::Def(Func::from(id).into()),
                | ValueNs::Const(id) => PathResolution::Def(Const::from(id).into()),
                | ValueNs::Static(id) => PathResolution::Def(Static::from(id).into()),
                | ValueNs::Ctor(id) => PathResolution::Def(Ctor::from(id).into()),
            })
        })
    };

    let items = || {
        let per_ns = resolver.resolve_module_path(db.upcast(), path);

        per_ns.types.or(per_ns.modules).map(|it| PathResolution::Def(it.into()))
    };

    if prefer_value_ns {
        values().or_else(types)
    } else {
        types().or_else(values)
    }
    .or_else(items)
}
