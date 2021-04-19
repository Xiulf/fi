use crate::arena::{Arena, Idx};
use crate::body::Body;
use crate::db::DefDatabase;
use crate::expr::ExprId;
use crate::id::DefWithBodyId;
use crate::name::Name;
use crate::pat::PatId;
use rustc_hash::FxHashMap;
use std::sync::Arc;

pub type ExprScopeId = Idx<ExprScopeData>;
pub type TypeScopeId = Idx<TypeScopeData>;

#[derive(Debug, PartialEq, Eq)]
pub struct ExprScopes {
    scopes: Arena<ExprScopeData>,
    scopes_by_expr: FxHashMap<ExprId, ExprScopeId>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct ExprScopeData {
    parent: Option<ExprScopeId>,
    entries: Vec<ExprScopeEntry>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct ExprScopeEntry {
    name: Name,
    pat: PatId,
}

#[derive(Debug, PartialEq, Eq)]
pub struct TypeScopes {
    scopes: Arena<TypeScopeData>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct TypeScopeData {
    parent: Option<TypeScopeId>,
    entries: Vec<TypeScopeEntry>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct TypeScopeEntry {
    name: Name,
}

impl ExprScopeEntry {
    pub fn name(&self) -> &Name {
        &self.name
    }

    pub fn pat(&self) -> PatId {
        self.pat
    }
}

impl ExprScopes {
    pub fn expr_scopes_query(db: &dyn DefDatabase, def: DefWithBodyId) -> Arc<ExprScopes> {
        let body = db.body(def);

        Arc::new(ExprScopes::new(&body))
    }

    fn new(body: &Body) -> ExprScopes {
        unimplemented!()
    }
}
