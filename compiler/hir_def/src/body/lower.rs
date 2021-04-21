use crate::arena::Arena;
use crate::ast_id::{AstIdMap, FileAstId};
use crate::body::{Body, BodySourceMap};
use crate::db::DefDatabase;
use crate::expr::{dummy_expr_id, ExprId};
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
) -> (Body, BodySourceMap) {
    ExprCollector {
        db,
        file_id,
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
    file_id: FileId,
}

impl<'a> ExprCollector<'a> {
    fn collect(
        mut self,
        param_list: Option<ast::AstChildren<ast::Pat>>,
        body: Option<ast::Expr>,
    ) -> (Body, BodySourceMap) {
        self.body.body_expr = self.collect_expr_opt(body);

        (self.body, self.source_map)
    }

    fn ctx(&self) -> LowerCtx {
        LowerCtx::new(self.db, self.file_id)
    }

    fn collect_expr(&mut self, expr: ast::Expr) -> ExprId {
        self.maybe_collect_expr(expr).unwrap_or_else(|| self.missing_expr())
    }

    fn maybe_collect_expr(&mut self, expr: ast::Expr) -> Option<ExprId> {
        let syntax_ptr = AstPtr::new(&expr);

        None
    }

    fn collect_expr_opt(&mut self, expr: Option<ast::Expr>) -> ExprId {
        if let Some(expr) = expr {
            self.collect_expr(expr)
        } else {
            self.missing_expr()
        }
    }
}
