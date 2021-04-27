use crate::arena::Arena;
use crate::ast_id::{AstIdMap, FileAstId};
use crate::body::{Body, BodySourceMap, ExprPtr, ExprSource, PatPtr, PatSource, SyntheticSyntax};
use crate::db::DefDatabase;
use crate::def_map::DefMap;
use crate::expr::{dummy_expr_id, Expr, ExprId};
use crate::id::{LocalModuleId, ModuleDefId, ModuleId};
use crate::in_file::InFile;
use crate::name::{AsName, Name};
use crate::pat::{Pat, PatId};
use crate::path::Path;
use base_db::input::FileId;
use std::sync::Arc;
use syntax::{ast, AstPtr};

pub struct LowerCtx {
    file_id: FileId,
    source_ast_id_map: Arc<AstIdMap>,
}

impl LowerCtx {
    pub fn new(db: &dyn DefDatabase, file_id: FileId) -> Self {
        LowerCtx {
            file_id,
            source_ast_id_map: db.ast_id_map(file_id),
        }
    }

    pub(crate) fn file_id(&self) -> FileId {
        self.file_id
    }

    pub(crate) fn lower_path(&self, ast: ast::Path) -> Path {
        Path::lower(ast)
    }

    pub(crate) fn ast_id<N: ast::AstNode>(&self, item: &N) -> FileAstId<N> {
        self.source_ast_id_map.ast_id(item)
    }
}

pub(super) fn lower(
    db: &dyn DefDatabase,
    params: Option<ast::AstChildren<ast::Pat>>,
    body: Option<ast::Expr>,
    file_id: FileId,
    module: ModuleId,
) -> (Body, BodySourceMap) {
    ExprCollector {
        db,
        file_id,
        module: module.local_id,
        def_map: db.def_map(module.lib),
        source_map: BodySourceMap::default(),
        body: Body {
            exprs: Arena::default(),
            pats: Arena::default(),
            params: Vec::new(),
            body_expr: dummy_expr_id(),
        },
    }
    .collect(params, body)
}

struct ExprCollector<'a> {
    db: &'a dyn DefDatabase,
    body: Body,
    source_map: BodySourceMap,
    def_map: Arc<DefMap>,
    module: LocalModuleId,
    file_id: FileId,
}

impl<'a> ExprCollector<'a> {
    fn collect(mut self, params: Option<ast::AstChildren<ast::Pat>>, body: Option<ast::Expr>) -> (Body, BodySourceMap) {
        if let Some(params) = params {
            for param in params {
                let pat = self.collect_pat(param);

                self.body.params.push(pat);
            }
        }

        self.body.body_expr = self.collect_expr_opt(body);

        (self.body, self.source_map)
    }

    fn ctx(&self) -> LowerCtx {
        LowerCtx::new(self.db, self.file_id)
    }

    fn to_source<T>(&mut self, value: T) -> InFile<T> {
        InFile::new(self.file_id, value)
    }

    fn alloc_expr(&mut self, expr: Expr, ptr: ExprPtr) -> ExprId {
        let src = self.to_source(ptr);
        let id = self.make_expr(expr, Ok(src.clone()));

        self.source_map.expr_map.insert(src, id);
        id
    }

    fn alloc_expr_desugared(&mut self, expr: Expr) -> ExprId {
        self.make_expr(expr, Err(SyntheticSyntax))
    }

    fn missing_expr(&mut self) -> ExprId {
        self.alloc_expr_desugared(Expr::Missing)
    }

    fn make_expr(&mut self, expr: Expr, src: Result<ExprSource, SyntheticSyntax>) -> ExprId {
        let id = self.body.exprs.alloc(expr);

        self.source_map.expr_map_back.insert(id, src);
        id
    }

    fn alloc_pat(&mut self, pat: Pat, ptr: PatPtr) -> PatId {
        let src = self.to_source(ptr);
        let id = self.make_pat(pat, Ok(src.clone()));

        self.source_map.pat_map.insert(src, id);
        id
    }

    fn missing_pat(&mut self) -> PatId {
        self.make_pat(Pat::Missing, Err(SyntheticSyntax))
    }

    fn make_pat(&mut self, pat: Pat, src: Result<PatSource, SyntheticSyntax>) -> PatId {
        let id = self.body.pats.alloc(pat);

        self.source_map.pat_map_back.insert(id, src);
        id
    }

    fn collect_expr(&mut self, expr: ast::Expr) -> ExprId {
        self.maybe_collect_expr(expr).unwrap_or_else(|| self.missing_expr())
    }

    fn maybe_collect_expr(&mut self, expr: ast::Expr) -> Option<ExprId> {
        let syntax_ptr = AstPtr::new(&expr);

        Some(match expr {
            | ast::Expr::Path(e) => {
                let path = e
                    .path()
                    .map(Path::lower)
                    .map(|path| Expr::Path { path })
                    .unwrap_or(Expr::Missing);

                self.alloc_expr(path, syntax_ptr)
            },
            | ast::Expr::Do(e) => self.collect_block(e),
            | _ => unimplemented!("{:?}", expr),
        })
    }

    fn collect_expr_opt(&mut self, expr: Option<ast::Expr>) -> ExprId {
        if let Some(expr) = expr {
            self.collect_expr(expr)
        } else {
            self.missing_expr()
        }
    }

    fn collect_block(&mut self, block: ast::ExprDo) -> ExprId {
        unimplemented!();
    }

    fn collect_pat(&mut self, pat: ast::Pat) -> PatId {
        let ptr = AstPtr::new(&pat);
        let pattern = match pat {
            | ast::Pat::Bind(pat) => {
                let name = pat.name().map(|n| n.as_name()).unwrap_or_else(Name::missing);
                let subpat = pat.subpat().map(|sp| self.collect_pat(sp));

                if let None = subpat {
                    let (resolved, _) = self.def_map.resolve_path(self.db, self.module, &name.clone().into());

                    match resolved.values {
                        | Some(ModuleDefId::ConstId(_)) | Some(ModuleDefId::CtorId(_)) => {
                            Pat::Path { path: name.into() }
                        },
                        | _ => Pat::Bind { name, subpat },
                    }
                } else {
                    Pat::Bind { name, subpat }
                }
            },
            | _ => unimplemented!("{:?}", pat),
        };

        self.alloc_pat(pattern, ptr)
    }

    fn collect_pat_opt(&mut self, pat: Option<ast::Pat>) -> PatId {
        if let Some(pat) = pat {
            self.collect_pat(pat)
        } else {
            self.missing_pat()
        }
    }
}
