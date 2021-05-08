use crate::db::DefDatabase;
use crate::def_map::DefMap;
use crate::expr::ExprId;
use crate::id::*;
use crate::name::Name;
use crate::pat::PatId;
use crate::path::Path;
use crate::per_ns::PerNs;
use crate::scope::{ExprScopeId, ExprScopes, TypeScopeId, TypeScopes};
use crate::type_ref::{LocalTypeRefId, LocalTypeVarId, TypeMap};
use base_db::libs::LibId;
use std::sync::Arc;

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
    type_scopes: Arc<TypeScopes>,
    scope_id: TypeScopeId,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeNs {
    TypeAlias(TypeAliasId),
    TypeCtor(TypeCtorId),
    Class(ClassId),
    TypeVar(TypeVarId),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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

    pub fn resolve_type(&self, db: &dyn DefDatabase, path: &Path) -> Option<(TypeNs, Option<usize>)> {
        let n_segments = path.segments().len();
        let first_name = path.segments().first()?;

        for scope in self.scopes.iter().rev() {
            match scope {
                | Scope::TypeScope(scope) if n_segments <= 1 => {
                    let entry = scope
                        .type_scopes
                        .entries(scope.scope_id)
                        .iter()
                        .find(|entry| entry.name() == first_name);

                    if let Some(e) = entry {
                        return Some((TypeNs::TypeVar(e.type_var()), None));
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

    pub fn resolve_value(&self, db: &dyn DefDatabase, path: &Path) -> Option<(ValueNs, Option<usize>)> {
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
                        return Some((ValueNs::Local(e.pat()), None));
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

    pub fn resolve_value_fully(&self, db: &dyn DefDatabase, path: &Path) -> Option<ValueNs> {
        let (value, idx) = self.resolve_value(db, path)?;

        match idx {
            | None => Some(value),
            | Some(_) => None,
        }
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

    pub fn type_var_index(&self, tv: TypeVarId) -> Option<usize> {
        let scopes = self.scopes.iter().rev().filter_map(|scope| match scope {
            | Scope::TypeScope(it) => Some(it),
            | _ => None,
        });

        let mut depth = 0;

        for scope in scopes {
            if let Some(idx) = scope
                .type_scopes
                .entries(scope.scope_id)
                .iter()
                .position(|e| e.type_var() == tv)
            {
                return Some(depth + idx);
            }

            depth += scope.type_scopes.entries(scope.scope_id).len();
        }

        None
    }

    pub fn with_type_scopes(mut self, map: &TypeMap, owner: TypeVarOwner, id: LocalTypeRefId) -> Self {
        let scopes = Arc::new(TypeScopes::new(map, owner, id));
        let scope_id = scopes.scope_for(id);
        let scope_chain = scopes.scope_chain(scope_id).collect::<Vec<_>>();

        for scope in scope_chain.into_iter().rev() {
            self = self.push_type_scope(Arc::clone(&scopes), scope);
        }

        self
    }

    pub fn with_type_vars(
        mut self,
        map: &TypeMap,
        owner: TypeVarOwner,
        vars: impl Iterator<Item = LocalTypeVarId>,
    ) -> Self {
        let (scopes, root) = TypeScopes::from_type_vars(map, owner, vars);
        let scopes = Arc::new(scopes);

        self.push_type_scope(scopes, root)
    }
}

pub fn resolver_for_expr(db: &dyn DefDatabase, owner: DefWithBodyId, expr_id: ExprId) -> Resolver {
    let scopes = db.expr_scopes(owner);

    resolver_for_scope(db, owner, scopes.scope_for(expr_id))
}

pub fn resolver_for_scope(db: &dyn DefDatabase, owner: DefWithBodyId, scope_id: Option<ExprScopeId>) -> Resolver {
    let mut r = owner.resolver(db);
    let scopes = db.expr_scopes(owner);
    let scope_chain = scopes.scope_chain(scope_id).collect::<Vec<_>>();

    for scope in scope_chain.into_iter().rev() {
        r = r.push_expr_scope(owner, Arc::clone(&scopes), scope);
    }

    r
}

impl Resolver {
    fn push_scope(mut self, scope: Scope) -> Self {
        self.scopes.push(scope);
        self
    }

    fn push_module_scope(self, def_map: Arc<DefMap>, module_id: LocalModuleId) -> Self {
        self.push_scope(Scope::ModuleScope(ModuleItemMap { def_map, module_id }))
    }

    fn push_expr_scope(self, owner: DefWithBodyId, expr_scopes: Arc<ExprScopes>, scope_id: ExprScopeId) -> Self {
        self.push_scope(Scope::ExprScope(ExprScope {
            owner,
            expr_scopes,
            scope_id,
        }))
    }

    fn push_type_scope(self, type_scopes: Arc<TypeScopes>, scope_id: TypeScopeId) -> Self {
        self.push_scope(Scope::TypeScope(TypeScope { type_scopes, scope_id }))
    }
}

impl ModuleItemMap {
    fn resolve_type(&self, db: &dyn DefDatabase, path: &Path) -> Option<(TypeNs, Option<usize>)> {
        let (module_def, idx) = self.def_map.resolve_path(db, self.module_id, path);
        let res = to_type_ns(module_def)?;

        Some((res, idx))
    }

    fn resolve_value(&self, db: &dyn DefDatabase, path: &Path) -> Option<(ValueNs, Option<usize>)> {
        let (module_def, idx) = self.def_map.resolve_path(db, self.module_id, path);
        let res = to_value_ns(module_def)?;

        Some((res, idx))
    }
}

fn to_type_ns(per_ns: PerNs) -> Option<TypeNs> {
    let res = match per_ns.types? {
        | ModuleDefId::TypeAliasId(id) => TypeNs::TypeAlias(id),
        | ModuleDefId::TypeCtorId(id) => TypeNs::TypeCtor(id),
        | ModuleDefId::ClassId(id) => TypeNs::Class(id),
        | _ => return None,
    };

    Some(res)
}

fn to_value_ns(per_ns: PerNs) -> Option<ValueNs> {
    let res = match per_ns.values? {
        | ModuleDefId::FixityId(id) => ValueNs::Fixity(id),
        | ModuleDefId::FuncId(id) => ValueNs::Func(id),
        | ModuleDefId::ConstId(id) => ValueNs::Const(id),
        | ModuleDefId::StaticId(id) => ValueNs::Static(id),
        | ModuleDefId::CtorId(id) => ValueNs::Ctor(id),
        | _ => return None,
    };

    Some(res)
}

impl HasResolver for ModuleId {
    fn resolver(self, db: &dyn DefDatabase) -> Resolver {
        let def_map = db.def_map(self.lib);

        Resolver::default().push_module_scope(def_map, self.local_id)
    }
}

impl HasResolver for FuncId {
    fn resolver(self, db: &dyn DefDatabase) -> Resolver {
        let data = db.func_data(self);

        self.lookup(db).container.resolver(db).with_type_vars(
            data.type_map(),
            TypedDefId::FuncId(self).into(),
            data.vars.iter().copied(),
        )
    }
}

impl HasResolver for ConstId {
    fn resolver(self, db: &dyn DefDatabase) -> Resolver {
        self.lookup(db).container.resolver(db)
    }
}

impl HasResolver for StaticId {
    fn resolver(self, db: &dyn DefDatabase) -> Resolver {
        self.lookup(db).container.resolver(db)
    }
}

impl HasResolver for DefWithBodyId {
    fn resolver(self, db: &dyn DefDatabase) -> Resolver {
        match self {
            | DefWithBodyId::FuncId(id) => id.resolver(db),
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
        let data = db.type_alias_data(self);

        self.lookup(db).module.resolver(db).with_type_vars(
            data.type_map(),
            TypedDefId::TypeAliasId(self).into(),
            data.vars.iter().copied(),
        )
    }
}

impl HasResolver for TypeCtorId {
    fn resolver(self, db: &dyn DefDatabase) -> Resolver {
        let data = db.type_ctor_data(self);

        self.lookup(db).module.resolver(db).with_type_vars(
            data.type_map(),
            TypedDefId::TypeCtorId(self).into(),
            data.vars.iter().copied(),
        )
    }
}

impl HasResolver for ClassId {
    fn resolver(self, db: &dyn DefDatabase) -> Resolver {
        let data = db.class_data(self);

        self.lookup(db).module.resolver(db).with_type_vars(
            data.type_map(),
            TypedDefId::ClassId(self).into(),
            data.vars.iter().copied(),
        )
    }
}

impl HasResolver for InstanceId {
    fn resolver(self, db: &dyn DefDatabase) -> Resolver {
        let data = db.instance_data(self);

        self.lookup(db).module.resolver(db).with_type_vars(
            data.type_map(),
            TypedDefId::InstanceId(self).into(),
            data.vars.iter().copied(),
        )
    }
}

impl HasResolver for ContainerId {
    fn resolver(self, db: &dyn DefDatabase) -> Resolver {
        match self {
            | ContainerId::Module(id) => id.resolver(db),
            | ContainerId::Class(id) => id.resolver(db),
            | ContainerId::Instance(id) => id.resolver(db),
        }
    }
}
