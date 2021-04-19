use crate::db::DefDatabase;
use crate::def_map::DefMap;
use crate::id::{DefWithBodyId, LocalModuleId};
use crate::path::Path;
use crate::per_ns::PerNs;
use crate::scope::{ExprScopeId, ExprScopes, TypeScopeId, TypeScopes};
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
    owner: DefWithBodyId,
    type_scopes: Arc<TypeScopes>,
    scope_id: TypeScopeId,
}

impl Resolver {
    pub fn resolve_path(&self, db: &dyn DefDatabase, path: &Path) -> PerNs {
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

    fn module_scope(&self) -> Option<(&DefMap, LocalModuleId)> {
        self.scopes.iter().rev().find_map(|scope| match scope {
            | Scope::ModuleScope(m) => Some((&*m.def_map, m.module_id)),
            | _ => None,
        })
    }
}
