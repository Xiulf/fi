mod lower;

use std::sync::Arc;

use arena::{Arena, ArenaMap};
use base_db::input::FileId;
use either::Either;
use rustc_hash::FxHashMap;
use syntax::{ast, AstPtr};

use crate::db::DefDatabase;
use crate::expr::{Expr, ExprId};
use crate::id::{DefWithBodyId, HasModule, Lookup, ModuleId};
use crate::in_file::InFile;
use crate::pat::{Pat, PatId};
use crate::source::HasSource;
use crate::type_ref::{LocalTypeRefId, TypeMap, TypeRef, TypeSourceMap};

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
    expr_map_back: ArenaMap<ExprId, Either<ExprSource, SyntheticSyntax>>,

    pat_map: FxHashMap<PatSource, PatId>,
    pat_map_back: ArenaMap<PatId, Either<PatSource, SyntheticSyntax>>,

    type_source_map: TypeSourceMap,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SyntheticSyntax(pub DefWithBodyId);

#[derive(Debug)]
enum BodyExpr {
    Single(ast::Expr),
    Case(Vec<BodyArm>),
}

#[derive(Debug)]
enum BodyArm {
    Expr(ast::Expr),
    Guarded(ast::CaseValueGuarded),
}

impl Body {
    pub fn body_source_map_query(db: &dyn DefDatabase, def: DefWithBodyId) -> (Arc<Body>, Arc<BodySourceMap>) {
        let mut params = None;

        let (file_id, module, body) = match def {
            | DefWithBodyId::FuncId(f) => {
                let f = f.lookup(db);
                let src = f.source(db);
                let group = src.value.iter().collect::<Vec<_>>();
                let expr = if group.len() == 1 || (group.len() == 2 && group[0].ty().is_some()) {
                    group.last().and_then(|it| {
                        it.body()
                            .map(BodyExpr::Single)
                            .or_else(|| it.guarded().map(|g| BodyExpr::Case(vec![BodyArm::Guarded(g)])))
                    })
                } else {
                    Some(BodyExpr::Case(
                        group
                            .iter()
                            .filter_map(|it| {
                                it.body()
                                    .map(BodyArm::Expr)
                                    .or_else(|| it.guarded().map(BodyArm::Guarded))
                            })
                            .collect(),
                    ))
                };

                params = Some(
                    group
                        .iter()
                        .filter_map(|it| {
                            if it.body().is_some() || it.guarded().is_some() {
                                Some(it.args())
                            } else {
                                None
                            }
                        })
                        .collect::<Vec<_>>(),
                );

                (src.file_id, f.module(db), expr)
            },
            | DefWithBodyId::ConstId(c) => {
                let c = c.lookup(db);
                let src = c.source(db);
                let mut iter = src.value.iter();
                let first = iter.next().unwrap();
                let expr = first
                    .value()
                    .or_else(|| iter.next().and_then(|it| it.value()))
                    .map(BodyExpr::Single);

                (src.file_id, c.module(db), expr)
            },
            | DefWithBodyId::StaticId(s) => {
                let s = s.lookup(db);
                let src = s.source(db);
                let mut iter = src.value.iter();
                let first = iter.next().unwrap();
                let expr = first
                    .value()
                    .or_else(|| iter.next().and_then(|it| it.value()))
                    .map(BodyExpr::Single);

                (src.file_id, s.module(db), expr)
            },
        };

        let (body, source_map) = Body::new(db, def, params, body, file_id, module);

        (Arc::new(body), Arc::new(source_map))
    }

    pub fn body_query(db: &dyn DefDatabase, def: DefWithBodyId) -> Arc<Body> {
        db.body_source_map(def).0
    }

    fn new(
        db: &dyn DefDatabase,
        owner: DefWithBodyId,
        params: Option<Vec<ast::AstChildren<ast::Pat>>>,
        body: Option<BodyExpr>,
        file_id: FileId,
        module: ModuleId,
    ) -> (Body, BodySourceMap) {
        lower::lower(db, owner, params, body, file_id, module)
    }

    pub fn has_body(&self) -> bool {
        self[self.body_expr] != Expr::Missing
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

impl BodySourceMap {
    pub fn expr_syntax(&self, expr: ExprId) -> Either<ExprSource, SyntheticSyntax> {
        self.expr_map_back[expr].clone()
    }

    pub fn node_expr(&self, node: InFile<&ast::Expr>) -> Option<ExprId> {
        let src = node.map(AstPtr::new);
        self.expr_map.get(&src).cloned()
    }

    pub fn pat_syntax(&self, pat: PatId) -> Either<PatSource, SyntheticSyntax> {
        self.pat_map_back[pat].clone()
    }

    pub fn node_pat(&self, node: InFile<&ast::Pat>) -> Option<PatId> {
        let src = node.map(AstPtr::new);
        self.pat_map.get(&src).cloned()
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
