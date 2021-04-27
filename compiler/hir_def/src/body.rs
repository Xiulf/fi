mod lower;

use crate::arena::{Arena, ArenaMap};
use crate::db::DefDatabase;
use crate::expr::{Expr, ExprId};
use crate::id::{DefWithBodyId, HasModule, HasSource, Lookup, ModuleId};
use crate::in_file::InFile;
use crate::pat::{Pat, PatId};
use base_db::input::FileId;
use rustc_hash::FxHashMap;
use std::sync::Arc;
use syntax::{ast, AstPtr};

#[derive(Debug, PartialEq, Eq)]
pub struct Body {
    pub exprs: Arena<Expr>,
    pub pats: Arena<Pat>,
    pub params: Vec<PatId>,
    pub body_expr: ExprId,
}

pub type ExprPtr = AstPtr<ast::Expr>;
pub type ExprSource = InFile<ExprPtr>;

pub type PatPtr = AstPtr<ast::Pat>;
pub type PatSource = InFile<PatPtr>;

#[derive(Default, Debug, PartialEq, Eq)]
pub struct BodySourceMap {
    expr_map: FxHashMap<ExprSource, ExprId>,
    expr_map_back: ArenaMap<ExprId, Result<ExprSource, SyntheticSyntax>>,

    pat_map: FxHashMap<PatSource, PatId>,
    pat_map_back: ArenaMap<PatId, Result<PatSource, SyntheticSyntax>>,
}

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
pub struct SyntheticSyntax;

impl Body {
    pub fn body_source_map_query(db: &dyn DefDatabase, def: DefWithBodyId) -> (Arc<Body>, Arc<BodySourceMap>) {
        let mut params = None;

        let (file_id, module, body) = match def {
            | DefWithBodyId::FuncId(f) => {
                let f = f.lookup(db);
                let src = f.source(db);

                params = Some(src.value.args());
                (src.file_id, f.module(db), src.value.body())
            },
            | DefWithBodyId::ConstId(c) => {
                let c = c.lookup(db);
                let src = c.source(db);

                (src.file_id, c.module(db), src.value.value())
            },
            | DefWithBodyId::StaticId(s) => {
                let s = s.lookup(db);
                let src = s.source(db);

                (src.file_id, s.module(db), src.value.value())
            },
        };

        let (body, source_map) = Body::new(db, params, body, file_id, module);

        (Arc::new(body), Arc::new(source_map))
    }

    pub fn body_query(db: &dyn DefDatabase, def: DefWithBodyId) -> Arc<Body> {
        db.body_source_map(def).0
    }

    fn new(
        db: &dyn DefDatabase,
        params: Option<ast::AstChildren<ast::Pat>>,
        body: Option<ast::Expr>,
        file_id: FileId,
        module: ModuleId,
    ) -> (Body, BodySourceMap) {
        lower::lower(db, params, body, file_id, module)
    }
}
