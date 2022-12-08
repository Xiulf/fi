use std::sync::Arc;

use base_db::input::FileId;
use hir_def::body::{Body, BodySourceMap};
use hir_def::expr::ExprId;
use hir_def::id::DefWithBodyId;
use hir_def::in_file::InFile;
use hir_def::name::AsName;
use hir_def::pat::{Pat, PatId};
use hir_def::path::Path;
use hir_def::resolver::{Resolver, TypeNs, ValueNs};
use hir_def::scope::{ExprScopeId, ExprScopes};
use hir_def::type_ref::LocalTypeRefId;
use hir_ty::infer::InferenceResult;
use hir_ty::ty::{Constraint, Ty};
use syntax::{ast, AstNode, SyntaxNode, TextRange, TextSize};

use crate::db::HirDatabase;
use crate::{Class, Const, Ctor, Fixity, Func, Local, ModuleDef, PathResolution, Static, TypeAlias, TypeCtor};

#[allow(dead_code)]
#[derive(Debug)]
pub(crate) struct SourceAnalyzer {
    pub(crate) file_id: FileId,
    pub(crate) resolver: Resolver,
    body: Option<Arc<Body>>,
    body_source_map: Option<Arc<BodySourceMap>>,
    body_owner: Option<DefWithBodyId>,
    infer: Option<Arc<InferenceResult<Ty, Constraint>>>,
    scopes: Option<Arc<ExprScopes>>,
}

impl SourceAnalyzer {
    pub(crate) fn new_for_resolver(resolver: Resolver, file_id: FileId) -> Self {
        SourceAnalyzer {
            resolver,
            file_id,
            body: None,
            body_source_map: None,
            body_owner: None,
            infer: None,
            scopes: None,
        }
    }

    pub(crate) fn new_for_body(
        db: &dyn HirDatabase,
        def: DefWithBodyId,
        node: InFile<&SyntaxNode>,
        offset: Option<TextSize>,
    ) -> Self {
        let (body, source_map) = db.body_source_map(def);
        let scopes = db.expr_scopes(def);
        let scope = match offset {
            | None => scope_for(&scopes, &source_map, node),
            | Some(offset) => scope_for_offset(db, &scopes, &source_map, node.with_value(offset)),
        };

        let resolver = Resolver::for_expr_scope(db.upcast(), def, scope);

        Self {
            resolver,
            body: Some(body),
            body_source_map: Some(source_map),
            body_owner: Some(def),
            infer: Some(db.infer(def)),
            scopes: Some(scopes),
            file_id: node.file_id,
        }
    }

    pub fn resolve_path(&self, db: &dyn HirDatabase, path: &ast::Path) -> Option<PathResolution> {
        let parent = || path.syntax().parent();
        let mut prefer_value_ns = false;

        if let Some(_) = parent().and_then(ast::Expr::cast) {
            prefer_value_ns = true;
        }

        let hir_path = Path::lower(path.clone());

        resolve_hir_path_(db, &self.resolver, &hir_path, prefer_value_ns)
    }

    pub fn resolve_ident(&self, db: &dyn HirDatabase, name_ref: &ast::NameRef) -> Option<PathResolution> {
        let name = name_ref.as_name();
        let hir_path = Path::from(name);

        resolve_hir_path_(db, &self.resolver, &hir_path, true)
    }

    pub fn type_of_expr(&self, expr: &ast::Expr) -> Option<Ty> {
        let expr_id = self.expr_id(expr)?;
        let infer = self.infer.as_ref()?;
        let ty = infer.type_of_expr[expr_id];

        Some(ty)
    }

    pub fn type_of_pat(&self, pat: &ast::Pat) -> Option<Ty> {
        let pat_id = self.pat_id(pat)?;
        let infer = self.infer.as_ref()?;
        let ty = infer.type_of_pat[pat_id];

        Some(ty)
    }

    pub fn kind_of(&self, db: &dyn HirDatabase, ty: &ast::Type) -> Option<Ty> {
        let type_id = self.type_id(ty)?;
        let resolver = self.resolver.clone();
        let owner = resolver.body_owner()?.into();
        let body = self.body.as_ref()?;
        let mut infer_ctx = hir_ty::infer::InferenceContext::new(db, resolver, owner, false);
        let mut lower_ctx = hir_ty::lower::LowerCtx::new(body.type_map(), &mut infer_ctx);
        let ty = lower_ctx.lower_ty(type_id);
        let kind = infer_ctx.infer_kind(ty);
        let kind = infer_ctx.generalize(kind);
        let kind = infer_ctx.convert_ty(kind);

        Some(kind)
    }

    pub fn resolve_bind_pat_to_const(&self, db: &dyn HirDatabase, pat: &ast::PatBind) -> Option<ModuleDef> {
        let pat_id = self.pat_id(&pat.clone().into())?;
        let body = self.body.as_ref()?;
        let path = match &body[pat_id] {
            | Pat::Path { path } => path,
            | _ => return None,
        };

        let res = resolve_hir_path_(db, &self.resolver, path, true)?;

        match res {
            | PathResolution::Def(def) => Some(def),
            | _ => None,
        }
    }

    fn expr_id(&self, expr: &ast::Expr) -> Option<ExprId> {
        let src = InFile::new(self.file_id, expr);
        let sm = self.body_source_map.as_ref()?;

        sm.node_expr(src)
    }

    fn pat_id(&self, pat: &ast::Pat) -> Option<PatId> {
        let src = InFile::new(self.file_id, pat);
        let sm = self.body_source_map.as_ref()?;

        sm.node_pat(src)
    }

    fn type_id(&self, ty: &ast::Type) -> Option<LocalTypeRefId> {
        let ptr = syntax::AstPtr::new(ty);
        let sm = self.body_source_map.as_ref()?;

        sm.syntax_type_ref(ptr)
    }
}

#[inline]
pub(crate) fn _resolve_hir_path(db: &dyn HirDatabase, resolver: &Resolver, path: &Path) -> Option<PathResolution> {
    resolve_hir_path_(db, resolver, path, false)
}

fn scope_for(scopes: &ExprScopes, source_map: &BodySourceMap, node: InFile<&SyntaxNode>) -> Option<ExprScopeId> {
    node.value
        .ancestors()
        .filter_map(ast::Expr::cast)
        .filter_map(|it| source_map.node_expr(InFile::new(node.file_id, &it)))
        .find_map(|it| scopes.scope_for(it))
}

fn scope_for_offset(
    db: &dyn HirDatabase,
    scopes: &ExprScopes,
    source_map: &BodySourceMap,
    offset: InFile<TextSize>,
) -> Option<ExprScopeId> {
    scopes
        .scopes_by_expr()
        .iter()
        .filter_map(|(id, scope)| {
            let source = source_map.expr_syntax(*id).left()?;

            if source.file_id != offset.file_id {
                return None;
            }

            let root = source.file_syntax(db.upcast());
            let node = source.value.to_node(&root);

            Some((node.syntax().text_range(), scope))
        })
        .min_by_key(|(expr_range, _)| {
            (
                !(expr_range.start() <= offset.value && offset.value <= expr_range.end()),
                expr_range.len(),
            )
        })
        .map(|(expr_range, scope)| adjust(db, scopes, source_map, expr_range, offset).unwrap_or(*scope))
}

fn adjust(
    db: &dyn HirDatabase,
    scopes: &ExprScopes,
    source_map: &BodySourceMap,
    expr_range: TextRange,
    offset: InFile<TextSize>,
) -> Option<ExprScopeId> {
    scopes
        .scopes_by_expr()
        .iter()
        .filter_map(|(id, scope)| {
            let source = source_map.expr_syntax(*id).left()?;

            if source.file_id != offset.file_id {
                return None;
            }

            let root = source.file_syntax(db.upcast());
            let node = source.value.to_node(&root);

            Some((node.syntax().text_range(), scope))
        })
        .filter(|&(range, _)| range.start() <= offset.value && expr_range.contains_range(range) && range != expr_range)
        .max_by(|&(r1, _), &(r2, _)| {
            if r1.contains_range(r2) {
                std::cmp::Ordering::Greater
            } else if r2.contains_range(r1) {
                std::cmp::Ordering::Less
            } else {
                r1.start().cmp(&r2.start())
            }
        })
        .map(|(_, scope)| *scope)
}

fn resolve_hir_path_(
    db: &dyn HirDatabase,
    resolver: &Resolver,
    path: &Path,
    prefer_value_ns: bool,
) -> Option<PathResolution> {
    let types = || {
        let (ty, _vis, remaining) = resolver.resolve_type(db.upcast(), path)?;
        let (ty, unresolved) = match remaining {
            | Some(remaining) if remaining > 1 => return None,
            | _ => (ty, path.segments().get(1)),
        };

        let res = match ty {
            | TypeNs::Fixity(id) => PathResolution::Def(Fixity::from(id).into()),
            | TypeNs::TypeAlias(id) => PathResolution::Def(TypeAlias::from(id).into()),
            | TypeNs::TypeCtor(id) => PathResolution::Def(TypeCtor::from(id).into()),
            | TypeNs::Class(id) => PathResolution::Def(Class::from(id).into()),
            | TypeNs::TypeVar(id) => PathResolution::TypeVar(id.into()),
        };

        match unresolved {
            | Some(_) => None,
            | None => Some(res),
        }
    };

    let body_owner = resolver.body_owner();
    let values = || {
        resolver.resolve_value_fully(db.upcast(), path).and_then(|(val, _vis)| {
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

        per_ns
            .types
            .or(per_ns.modules)
            .map(|it| PathResolution::Def(it.0.into()))
    };

    if prefer_value_ns {
        values().or_else(types)
    } else {
        types().or_else(values)
    }
    .or_else(items)
}
