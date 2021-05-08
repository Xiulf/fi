mod lower;

use crate::arena::{Arena, ArenaMap};
use crate::db::DefDatabase;
use crate::expr::{Expr, ExprId};
use crate::id::{DefWithBodyId, HasModule, HasSource, Lookup, ModuleId};
use crate::in_file::InFile;
use crate::pat::{Pat, PatId};
use crate::type_ref::{LocalTypeRefId, TypeMap, TypeRef, TypeSourceMap};
use base_db::input::FileId;
use rustc_hash::FxHashMap;
use std::sync::Arc;
use syntax::{ast, AstPtr};

#[derive(Debug, PartialEq, Eq)]
pub struct Body {
    exprs: Arena<Expr>,
    pats: Arena<Pat>,
    params: Vec<PatId>,
    body_expr: ExprId,
    type_map: TypeMap,
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

    type_source_map: TypeSourceMap,
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

    pub fn params(&self) -> &[PatId] {
        &self.params
    }

    pub fn body_expr(&self) -> ExprId {
        self.body_expr
    }

    pub fn type_map(&self) -> &TypeMap {
        &self.type_map
    }
}

impl std::ops::Deref for BodySourceMap {
    type Target = TypeSourceMap;

    fn deref(&self) -> &Self::Target {
        &self.type_source_map
    }
}

impl std::ops::Index<ExprId> for Body {
    type Output = Expr;

    fn index(&self, expr: ExprId) -> &Self::Output {
        &self.exprs[expr]
    }
}

impl std::ops::Index<PatId> for Body {
    type Output = Pat;

    fn index(&self, pat: PatId) -> &Self::Output {
        &self.pats[pat]
    }
}

impl std::ops::Index<LocalTypeRefId> for Body {
    type Output = TypeRef;

    fn index(&self, id: LocalTypeRefId) -> &Self::Output {
        &self.type_map[id]
    }
}
