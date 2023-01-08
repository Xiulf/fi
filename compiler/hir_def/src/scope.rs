use std::sync::Arc;

use arena::{Arena, Idx};
use rustc_hash::FxHashMap;

use crate::body::Body;
use crate::db::DefDatabase;
use crate::expr::{CaseValue, Expr, ExprId, Stmt};
use crate::id::{DefWithBodyId, TypeVarOwner};
use crate::name::Name;
use crate::pat::{Pat, PatId};
use crate::type_ref::{LocalTypeRefId, LocalTypeVarId, TypeMap, TypeRef, WhereClause};

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
    scopes_by_ty: FxHashMap<LocalTypeRefId, TypeScopeId>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct TypeScopeData {
    parent: Option<TypeScopeId>,
    entries: Vec<TypeScopeEntry>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct TypeScopeEntry {
    name: Name,
    id: LocalTypeVarId,
}

impl ExprScopeEntry {
    pub fn name(&self) -> &Name {
        &self.name
    }

    pub fn pat(&self) -> PatId {
        self.pat
    }
}

impl TypeScopeEntry {
    pub fn name(&self) -> &Name {
        &self.name
    }

    pub fn id(&self) -> LocalTypeVarId {
        self.id
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

        scopes.add_params_bindings(body, root, body.params());
        compute_expr_scopes(body.body_expr(), body, &mut scopes, root);
        scopes
    }

    pub fn scopes_by_expr(&self) -> &FxHashMap<ExprId, ExprScopeId> {
        &self.scopes_by_expr
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
        | Expr::Do { stmts } | Expr::Try { stmts } => {
            compute_block_scopes(stmts, body, scopes, scope);
        },
        | Expr::Lambda { pats, body: b } => {
            let scope = scopes.new_scope(scope);

            for &pat in pats.iter() {
                scopes.add_bindings(body, scope, pat);
            }

            compute_expr_scopes(*b, body, scopes, scope);
        },
        | Expr::Case { pred, arms } => {
            compute_expr_scopes(*pred, body, scopes, scope);

            for arm in arms.iter() {
                let scope = scopes.new_scope(scope);

                scopes.add_bindings(body, scope, arm.pat);

                match arm.value {
                    | CaseValue::Normal(expr) => compute_expr_scopes(expr, body, scopes, scope),
                    | CaseValue::Guarded(ref guards, ref exprs) => {
                        guards.iter().for_each(|&g| compute_expr_scopes(g, body, scopes, scope));
                        exprs.iter().for_each(|&e| compute_expr_scopes(e, body, scopes, scope));
                    },
                }
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

impl TypeScopes {
    pub fn type_scopes_query(db: &dyn DefDatabase, def: TypeVarOwner) -> Arc<TypeScopes> {
        def.with_type_map(db, |type_map| Arc::new(TypeScopes::new(type_map, def.type_vars(db))))
    }

    fn new(type_map: &TypeMap, vars: Option<Box<[LocalTypeVarId]>>) -> Self {
        let mut scopes = TypeScopes {
            scopes: Arena::default(),
            scopes_by_ty: FxHashMap::default(),
        };

        let root = scopes.root_scope();

        if let Some(vars) = vars {
            for &var in vars.iter() {
                let name = type_map[var].name.clone();

                scopes.add_binding(root, name, var);
            }
        }

        for (id, _) in type_map.iter() {
            if !scopes.scopes_by_ty.contains_key(&id) {
                compute_type_scopes(id, type_map, &mut scopes, root);
            }
        }

        scopes
    }

    pub fn root(&self) -> TypeScopeId {
        TypeScopeId::DUMMY
    }

    pub fn scopes_by_ty(&self) -> &FxHashMap<LocalTypeRefId, TypeScopeId> {
        &self.scopes_by_ty
    }

    pub fn entries(&self, id: TypeScopeId) -> &[TypeScopeEntry] {
        &self.scopes[id].entries
    }

    pub fn scope_chain(&self, scope: Option<TypeScopeId>) -> impl Iterator<Item = TypeScopeId> + '_ {
        std::iter::successors(scope, move |&scope| self.scopes[scope].parent)
    }

    pub fn scope_for(&self, id: LocalTypeRefId) -> Option<TypeScopeId> {
        self.scopes_by_ty.get(&id).copied()
    }

    fn root_scope(&mut self) -> TypeScopeId {
        self.scopes.alloc(TypeScopeData {
            parent: None,
            entries: Vec::new(),
        })
    }

    fn new_scope(&mut self, parent: TypeScopeId) -> TypeScopeId {
        self.scopes.alloc(TypeScopeData {
            parent: Some(parent),
            entries: Vec::new(),
        })
    }

    fn add_binding(&mut self, scope: TypeScopeId, name: Name, id: LocalTypeVarId) {
        let entry = TypeScopeEntry { name, id };

        self.scopes[scope].entries.push(entry);
    }

    fn set_scope(&mut self, ty: LocalTypeRefId, scope: TypeScopeId) {
        self.scopes_by_ty.insert(ty, scope);
    }
}

fn compute_type_scopes(id: LocalTypeRefId, type_map: &TypeMap, scopes: &mut TypeScopes, scope: TypeScopeId) {
    scopes.set_scope(id, scope);

    match type_map[id] {
        | TypeRef::Forall(ref vars, inner) => {
            let scope = scopes.new_scope(scope);

            for &var in vars.iter() {
                let name = type_map[var].name.clone();

                scopes.add_binding(scope, name, var);
            }

            compute_type_scopes(inner, type_map, scopes, scope);
        },
        | TypeRef::App(a, b) => {
            compute_type_scopes(a, type_map, scopes, scope);
            compute_type_scopes(b, type_map, scopes, scope);
        },
        | TypeRef::Infix(ref tys, _) => {
            for &ty in tys.iter() {
                compute_type_scopes(ty, type_map, scopes, scope);
            }
        },
        | TypeRef::Record(ref fields, tail) => {
            for field in fields.iter() {
                compute_type_scopes(field.ty, type_map, scopes, scope);
            }

            if let Some(tail) = tail {
                compute_type_scopes(tail, type_map, scopes, scope);
            }
        },
        | TypeRef::Row(ref fields, tail) => {
            for field in fields.iter() {
                compute_type_scopes(field.ty, type_map, scopes, scope);
            }

            if let Some(tail) = tail {
                compute_type_scopes(tail, type_map, scopes, scope);
            }
        },
        | TypeRef::Where(ref clause, inner) => {
            compute_where_scopes(clause, type_map, scopes, scope);
            compute_type_scopes(inner, type_map, scopes, scope);
        },
        | _ => {},
    }
}

fn compute_where_scopes(clause: &WhereClause, type_map: &TypeMap, scopes: &mut TypeScopes, scope: TypeScopeId) {
    for ctnt in clause.constraints.iter() {
        for &ty in ctnt.types.iter() {
            compute_type_scopes(ty, type_map, scopes, scope);
        }
    }

    for var_kind in clause.type_var_kinds.iter() {
        compute_type_scopes(var_kind.kind, type_map, scopes, scope);
    }
}
