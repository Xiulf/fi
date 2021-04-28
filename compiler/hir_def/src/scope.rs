use crate::arena::{Arena, Idx};
use crate::body::Body;
use crate::db::DefDatabase;
use crate::expr::{Expr, ExprId, Stmt};
use crate::id::DefWithBodyId;
use crate::name::Name;
use crate::pat::{Pat, PatId};
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
        let mut scopes = ExprScopes {
            scopes: Arena::default(),
            scopes_by_expr: FxHashMap::default(),
        };

        let root = scopes.root_scope();

        scopes.add_params_bindings(body, root, &body.params);
        compute_expr_scopes(body.body_expr, body, &mut scopes, root);
        scopes
    }

    pub fn entries(&self, id: ExprScopeId) -> &[ExprScopeEntry] {
        &self.scopes[id].entries
    }

    pub fn scope_chain(&self, scope: Option<ExprScopeId>) -> impl Iterator<Item = ExprScopeId> + '_ {
        std::iter::successors(scope, move |&scope| self.scopes[scope].parent)
    }

    pub fn scope_for(&self, id: ExprId) -> Option<ExprScopeId> {
        self.scopes_by_expr.get(&id).copied()
    }

    fn root_scope(&mut self) -> ExprScopeId {
        self.scopes.alloc(ExprScopeData {
            parent: None,
            entries: Vec::new(),
        })
    }

    fn new_scope(&mut self, parent: ExprScopeId) -> ExprScopeId {
        self.scopes.alloc(ExprScopeData {
            parent: Some(parent),
            entries: Vec::new(),
        })
    }

    fn add_bindings(&mut self, body: &Body, scope: ExprScopeId, pat: PatId) {
        let pattern = &body[pat];

        if let Pat::Bind { name, .. } = pattern {
            let entry = ExprScopeEntry {
                name: name.clone(),
                pat,
            };

            self.scopes[scope].entries.push(entry);
        }

        pattern.walk(|pat| self.add_bindings(body, scope, pat));
    }

    fn add_params_bindings(&mut self, body: &Body, scope: ExprScopeId, params: &[PatId]) {
        params.iter().for_each(|pat| self.add_bindings(body, scope, *pat));
    }

    fn set_scope(&mut self, expr: ExprId, scope: ExprScopeId) {
        self.scopes_by_expr.insert(expr, scope);
    }
}

fn compute_expr_scopes(expr: ExprId, body: &Body, scopes: &mut ExprScopes, scope: ExprScopeId) {
    scopes.set_scope(expr, scope);

    match &body[expr] {
        | Expr::Do { stmts } => {
            let scope = scopes.new_scope(scope);

            scopes.set_scope(expr, scope);
            compute_block_scopes(stmts, body, scopes, scope);
        },
        | Expr::Case { pred, arms } => {
            compute_expr_scopes(*pred, body, scopes, scope);

            for arm in arms {
                let scope = scopes.new_scope(scope);

                scopes.add_bindings(body, scope, arm.pat);

                if let Some(guard) = arm.guard {
                    compute_expr_scopes(guard, body, scopes, scope);
                }

                compute_expr_scopes(arm.expr, body, scopes, scope);
            }
        },
        | e => e.walk(|e| compute_expr_scopes(e, body, scopes, scope)),
    }
}

fn compute_block_scopes(stmts: &[Stmt], body: &Body, scopes: &mut ExprScopes, mut scope: ExprScopeId) {
    for stmt in stmts {
        match stmt {
            | Stmt::Let { pat, val } | Stmt::Bind { pat, val } => {
                compute_expr_scopes(*val, body, scopes, scope);
                scope = scopes.new_scope(scope);
                scopes.add_bindings(body, scope, *pat);
            },
            | Stmt::Expr { expr } => {
                compute_expr_scopes(*expr, body, scopes, scope);
            },
        }
    }
}
