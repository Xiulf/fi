use std::sync::Arc;

use base_db::libs::LibId;

use crate::db::DefDatabase;
use crate::def_map::DefMap;
use crate::expr::ExprId;
use crate::id::*;
use crate::name::Name;
use crate::pat::PatId;
use crate::path::Path;
use crate::per_ns::PerNs;
use crate::scope::{ExprScopeId, ExprScopes, TypeScopeId, TypeScopes};
use crate::type_ref::{LocalTypeRefId, TypeRef};
use crate::visibility::Visibility;

#[derive(Default, Debug, Clone)]
pub struct Resolver {
    scopes: Vec<Scope>,
}

#[derive(Debug, Clone)]
enum Scope {
    ModuleScope(ModuleItemMap),
    ExprScope(ExprScope),
    TypeScope(TypeScope),
}

#[derive(Debug, Clone)]
struct ModuleItemMap {
    def_map: Arc<DefMap>,
    module_id: LocalModuleId,
}

#[derive(Debug, Clone)]
struct ExprScope {
    owner: DefWithBodyId,
    expr_scopes: Arc<ExprScopes>,
    scope_id: ExprScopeId,
}

#[derive(Debug, Clone)]
struct TypeScope {
    owner: TypeVarOwner,
    type_scopes: Arc<TypeScopes>,
    scope_id: TypeScopeId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TypeNs {
    Fixity(FixityId),
    TypeAlias(TypeAliasId),
    TypeCtor(TypeCtorId),
    Class(ClassId),
    TypeVar(TypeVarId),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ValueNs {
    Local(PatId),
    Fixity(FixityId),
    Func(FuncId),
    Const(ConstId),
    Static(StaticId),
    Ctor(CtorId),
}

pub trait HasResolver: Copy {
    fn resolver(self, db: &dyn DefDatabase) -> Resolver;
}

impl Resolver {
    pub fn resolve_module_path(&self, db: &dyn DefDatabase, path: &Path) -> PerNs {
        let (item_map, module) = match self.module_scope() {
            | Some(it) => it,
            | None => return PerNs::none(),
        };

        let (module_res, segment_index) = item_map.resolve_path(db, module, &path);

        if segment_index.is_some() {
            return PerNs::none();
        }

        module_res
    }

    pub fn resolve_type(&self, db: &dyn DefDatabase, path: &Path) -> Option<(TypeNs, Visibility, Option<usize>)> {
        let n_segments = path.segments().len();
        let first_name = path.segments().first()?;

        for scope in self.scopes.iter().rev() {
            match scope {
                | Scope::TypeScope(scope) if n_segments == 1 => {
                    let entry = scope
                        .type_scopes
                        .entries(scope.scope_id)
                        .iter()
                        .find(|entry| entry.name() == first_name);

                    if let Some(e) = entry {
                        let id = TypeVarId {
                            owner: scope.owner,
                            local_id: e.id(),
                        };

                        return Some((TypeNs::TypeVar(id), Visibility::Public, None));
                    }
                },
                | Scope::TypeScope(_) => continue,
                | Scope::ExprScope(_) => continue,
                | Scope::ModuleScope(m) => {
                    if let Some(res) = m.resolve_type(db, path) {
                        return Some(res);
                    }
                },
            }
        }

        None
    }

    pub fn resolve_value(&self, db: &dyn DefDatabase, path: &Path) -> Option<(ValueNs, Visibility, Option<usize>)> {
        let n_segments = path.segments().len();
        let first_name = path.segments().first()?;

        for scope in self.scopes.iter().rev() {
            match scope {
                | Scope::ExprScope(scope) if n_segments <= 1 => {
                    let entry = scope
                        .expr_scopes
                        .entries(scope.scope_id)
                        .iter()
                        .find(|entry| entry.name() == first_name);

                    if let Some(e) = entry {
                        return Some((ValueNs::Local(e.pat()), Visibility::Public, None));
                    }
                },
                | Scope::ExprScope(_) => continue,
                | Scope::TypeScope(_) => continue,
                | Scope::ModuleScope(m) => {
                    if let Some(def) = m.resolve_value(db, path) {
                        return Some(def);
                    }
                },
            }
        }

        None
    }

    pub fn resolve_value_fully(&self, db: &dyn DefDatabase, path: &Path) -> Option<(ValueNs, Visibility)> {
        let (value, vis, idx) = self.resolve_value(db, path)?;

        match idx {
            | None => Some((value, vis)),
            | Some(_) => None,
        }
    }

    pub fn iter_values<'a>(&'a self) -> impl Iterator<Item = (&'a Name, ValueNs)> + 'a {
        self.scopes.iter().rev().flat_map(|scope| match scope {
            | Scope::ExprScope(scope) => scope
                .expr_scopes
                .entries(scope.scope_id)
                .iter()
                .map(|entry| (entry.name(), ValueNs::Local(entry.pat())))
                .collect::<Vec<_>>(),
            | Scope::ModuleScope(scope) => scope.values(),
            | Scope::TypeScope(_) => Vec::new(),
        })
    }

    pub fn iter_types(&self) -> impl Iterator<Item = TypeNs> + '_ {
        self.scopes.iter().rev().flat_map(|scope| match scope {
            | Scope::TypeScope(scope) => scope
                .type_scopes
                .entries(scope.scope_id)
                .iter()
                .map(|entry| {
                    TypeNs::TypeVar(TypeVarId {
                        owner: scope.owner,
                        local_id: entry.id(),
                    })
                })
                .collect::<Vec<_>>(),
            | Scope::ModuleScope(scope) => scope.types(),
            | Scope::ExprScope(_) => Vec::new(),
        })
    }

    fn module_scope(&self) -> Option<(&DefMap, LocalModuleId)> {
        self.scopes.iter().rev().find_map(|scope| match scope {
            | Scope::ModuleScope(m) => Some((&*m.def_map, m.module_id)),
            | _ => None,
        })
    }

    pub fn lib(&self) -> Option<LibId> {
        match self.scopes.first()? {
            | Scope::ModuleScope(m) => Some(m.def_map.lib()),
            | _ => None,
        }
    }

    pub fn body_owner(&self) -> Option<DefWithBodyId> {
        self.scopes.iter().rev().find_map(|scope| match scope {
            | Scope::ExprScope(it) => Some(it.owner),
            | _ => None,
        })
    }

    pub fn module(&self) -> Option<ModuleId> {
        let (def_map, local_id) = self.module_scope()?;

        Some(def_map.module_id(local_id))
    }

    pub fn type_var_index(&self, id: TypeVarId) -> Option<(usize, usize)> {
        let scopes = self.scopes.iter().rev().filter_map(|s| match s {
            | Scope::TypeScope(s) => Some(s),
            | _ => None,
        });

        let mut depth = 0;

        for scope in scopes {
            if let Some(idx) = scope
                .type_scopes
                .entries(scope.scope_id)
                .iter()
                .position(|entry| id.local_id == entry.id() && id.owner == scope.owner)
            {
                return Some((idx, depth));
            }

            depth += 1;
        }

        None
    }

    pub fn for_expr(db: &dyn DefDatabase, owner: DefWithBodyId, expr_id: ExprId) -> Self {
        let mut r = owner.resolver(db);
        let scopes = db.expr_scopes(owner);
        let scope_id = scopes.scope_for(expr_id);
        let scope_chain = scopes.scope_chain(scope_id).collect::<Vec<_>>();

        for scope in scope_chain.into_iter().rev() {
            r = r.push_expr_scope(owner, Arc::clone(&scopes), scope);
        }

        r
    }

    pub fn for_expr_scope(db: &dyn DefDatabase, owner: DefWithBodyId, scope_id: Option<ExprScopeId>) -> Self {
        let mut r = owner.resolver(db);
        let scopes = db.expr_scopes(owner);
        let scope_chain = scopes.scope_chain(scope_id).collect::<Vec<_>>();

        for scope in scope_chain.into_iter().rev() {
            r = r.push_expr_scope(owner, Arc::clone(&scopes), scope);
        }

        r
    }

    pub fn for_type_ref(db: &dyn DefDatabase, owner: TypeVarOwner, type_ref: LocalTypeRefId) -> Self {
        let mut r = owner.resolver(db);
        let scopes = db.type_scopes(owner);
        let scope_id = scopes.scope_for(type_ref);
        let scope_chain = scopes.scope_chain(scope_id).collect::<Vec<_>>();

        for scope in scope_chain.into_iter().rev() {
            if !scopes.entries(scope).is_empty() {
                r = r.push_type_scope(owner, Arc::clone(&scopes), scope);
            }
        }

        r
    }

    pub fn with_type_vars(self, db: &dyn DefDatabase, owner: TypeVarOwner) -> Self {
        let scopes = db.type_scopes(owner);
        let scope = scopes.root();

        self.push_type_scope(owner, scopes, scope)
    }
}

impl Resolver {
    fn push_scope(mut self, scope: Scope) -> Self {
        self.scopes.push(scope);
        self
    }

    fn push_module_scope(self, def_map: Arc<DefMap>, module_id: LocalModuleId) -> Self {
        std::iter::successors(Some(module_id), |&id| def_map[id].parent)
            .collect::<Vec<_>>()
            .into_iter()
            .rev()
            .fold(self, |this, module_id| {
                this.push_scope(Scope::ModuleScope(ModuleItemMap {
                    def_map: def_map.clone(),
                    module_id,
                }))
            })
    }

    fn push_expr_scope(self, owner: DefWithBodyId, expr_scopes: Arc<ExprScopes>, scope_id: ExprScopeId) -> Self {
        self.push_scope(Scope::ExprScope(ExprScope {
            owner,
            expr_scopes,
            scope_id,
        }))
    }

    fn push_type_scope(self, owner: TypeVarOwner, type_scopes: Arc<TypeScopes>, scope_id: TypeScopeId) -> Self {
        self.push_scope(Scope::TypeScope(TypeScope {
            owner,
            type_scopes,
            scope_id,
        }))
    }
}

impl ModuleItemMap {
    fn resolve_type(&self, db: &dyn DefDatabase, path: &Path) -> Option<(TypeNs, Visibility, Option<usize>)> {
        let (module_def, idx) = self.def_map.resolve_path(db, self.module_id, path);
        let (res, vis) = to_type_ns(module_def)?;

        Some((res, vis, idx))
    }

    fn resolve_value(&self, db: &dyn DefDatabase, path: &Path) -> Option<(ValueNs, Visibility, Option<usize>)> {
        let (module_def, idx) = self.def_map.resolve_path(db, self.module_id, path);
        let (res, vis) = to_value_ns(module_def)?;

        Some((res, vis, idx))
    }

    fn values(&self) -> Vec<(&Name, ValueNs)> {
        self.def_map[self.module_id]
            .scope
            .entries()
            .map(|(name, ns)| (name, ns))
            .filter_map(|(n, ns)| Some((n, to_value_ns(ns)?)))
            .map(|(n, (ns, _))| (n, ns))
            .collect()
    }

    fn types(&self) -> Vec<TypeNs> {
        self.def_map[self.module_id]
            .scope
            .entries()
            .map(|(_, ns)| ns)
            .filter_map(to_type_ns)
            .map(|(ns, _)| ns)
            .collect()
    }
}

fn to_type_ns(per_ns: PerNs) -> Option<(TypeNs, Visibility)> {
    let (def, vis) = per_ns.types?;
    let res = match def {
        | ModuleDefId::FixityId(id) => TypeNs::Fixity(id),
        | ModuleDefId::TypeAliasId(id) => TypeNs::TypeAlias(id),
        | ModuleDefId::TypeCtorId(id) => TypeNs::TypeCtor(id),
        | ModuleDefId::ClassId(id) => TypeNs::Class(id),
        | _ => return None,
    };

    Some((res, vis))
}

fn to_value_ns(per_ns: PerNs) -> Option<(ValueNs, Visibility)> {
    let (def, vis) = per_ns.values?;
    let res = match def {
        | ModuleDefId::FixityId(id) => ValueNs::Fixity(id),
        | ModuleDefId::FuncId(id) => ValueNs::Func(id),
        | ModuleDefId::ConstId(id) => ValueNs::Const(id),
        | ModuleDefId::StaticId(id) => ValueNs::Static(id),
        | ModuleDefId::CtorId(id) => ValueNs::Ctor(id),
        | _ => return None,
    };

    Some((res, vis))
}

impl HasResolver for ModuleId {
    fn resolver(self, db: &dyn DefDatabase) -> Resolver {
        let def_map = db.def_map(self.lib);

        Resolver::default().push_module_scope(def_map, self.local_id)
    }
}

impl HasResolver for FixityId {
    fn resolver(self, db: &dyn DefDatabase) -> Resolver {
        self.lookup(db).module.resolver(db)
    }
}

impl HasResolver for FuncId {
    fn resolver(self, db: &dyn DefDatabase) -> Resolver {
        self.lookup(db).container.resolver(db)
    }
}

impl HasResolver for StaticId {
    fn resolver(self, db: &dyn DefDatabase) -> Resolver {
        self.lookup(db).container.resolver(db)
    }
}

impl HasResolver for ConstId {
    fn resolver(self, db: &dyn DefDatabase) -> Resolver {
        self.lookup(db).module.resolver(db)
    }
}

impl HasResolver for DefWithBodyId {
    fn resolver(self, db: &dyn DefDatabase) -> Resolver {
        match self {
            | DefWithBodyId::FuncId(id) => {
                let data = db.func_data(id);

                if let Some(ty) = data.ty {
                    let type_map = data.type_map();

                    if let TypeRef::Forall(_, inner) = type_map[ty] {
                        let owner = TypeVarOwner::TypedDefId(id.into());

                        return Resolver::for_type_ref(db, owner, inner);
                    }
                }

                id.lookup(db).container.resolver(db)
            },
            | DefWithBodyId::ConstId(id) => id.resolver(db),
            | DefWithBodyId::StaticId(id) => id.resolver(db),
        }
    }
}

impl HasResolver for CtorId {
    fn resolver(self, db: &dyn DefDatabase) -> Resolver {
        self.parent.resolver(db)
    }
}

impl HasResolver for TypeAliasId {
    fn resolver(self, db: &dyn DefDatabase) -> Resolver {
        self.lookup(db)
            .module
            .resolver(db)
            .with_type_vars(db, TypeVarOwner::TypedDefId(self.into()))
    }
}

impl HasResolver for TypeCtorId {
    fn resolver(self, db: &dyn DefDatabase) -> Resolver {
        self.lookup(db)
            .module
            .resolver(db)
            .with_type_vars(db, TypeVarOwner::TypedDefId(self.into()))
    }
}

impl HasResolver for ClassId {
    fn resolver(self, db: &dyn DefDatabase) -> Resolver {
        self.lookup(db)
            .module
            .resolver(db)
            .with_type_vars(db, TypeVarOwner::TypedDefId(self.into()))
    }
}

impl HasResolver for MemberId {
    fn resolver(self, db: &dyn DefDatabase) -> Resolver {
        self.lookup(db)
            .module
            .resolver(db)
            .with_type_vars(db, TypeVarOwner::TypedDefId(self.into()))
    }
}

impl HasResolver for ContainerId {
    fn resolver(self, db: &dyn DefDatabase) -> Resolver {
        match self {
            | ContainerId::Module(id) => id.resolver(db),
            | ContainerId::Class(id) => id.resolver(db),
            | ContainerId::Member(id) => id.resolver(db),
        }
    }
}

impl HasResolver for TypeVarOwner {
    fn resolver(self, db: &dyn DefDatabase) -> Resolver {
        match self {
            | TypeVarOwner::DefWithBodyId(id) => id.resolver(db),
            | TypeVarOwner::TypedDefId(id) => id.resolver(db),
        }
    }
}

impl HasResolver for TypedDefId {
    fn resolver(self, db: &dyn DefDatabase) -> Resolver {
        match self {
            | TypedDefId::FuncId(id) => id.resolver(db),
            | TypedDefId::StaticId(id) => id.resolver(db),
            | TypedDefId::ConstId(id) => id.resolver(db),
            | TypedDefId::TypeAliasId(id) => id.resolver(db),
            | TypedDefId::TypeCtorId(id) => id.resolver(db),
            | TypedDefId::CtorId(id) => id.resolver(db),
            | TypedDefId::ClassId(id) => id.resolver(db),
            | TypedDefId::MemberId(id) => id.resolver(db),
        }
    }
}
