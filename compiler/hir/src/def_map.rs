mod collector;
mod path_resolution;

use crate::arena::Arena;
use crate::ast_id::{AstId, FileAstId};
use crate::db::DefDatabase;
use crate::diagnostics::DefDiagnostic;
use crate::id::{LocalModuleId, ModuleDefId, ModuleId};
use crate::item_scope::ItemScope;
use crate::name::Name;
use base_db::input::FileId;
use base_db::libs::LibId;
use rustc_hash::FxHashMap;
use std::ops::Index;
use std::sync::Arc;
use syntax::ast;

#[derive(Debug, PartialEq, Eq)]
pub struct DefMap {
    lib: LibId,
    root: LocalModuleId,
    modules: Arena<ModuleData>,
    extern_prelude: FxHashMap<Name, ModuleDefId>,
    diagnostics: Vec<DefDiagnostic>,
}

#[derive(Default, Debug, PartialEq, Eq)]
pub struct ModuleData {
    pub scope: ItemScope,
    pub origin: ModuleOrigin,
    pub children: FxHashMap<Name, LocalModuleId>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ModuleOrigin {
    pub declaration: AstId<ast::Module>,
    pub file_id: FileId,
}

impl DefMap {
    pub fn def_map_query(db: &dyn DefDatabase, lib: LibId) -> Arc<DefMap> {
        let def_map = DefMap::empty(lib);
        let def_map = collector::collect_defs(db, def_map);

        Arc::new(def_map)
    }

    fn empty(lib: LibId) -> Self {
        DefMap {
            lib,
            root: LocalModuleId::DUMMY,
            modules: Arena::default(),
            extern_prelude: FxHashMap::default(),
            diagnostics: Vec::new(),
        }
    }

    pub(crate) fn add_module(&mut self) -> LocalModuleId {
        self.modules.alloc(ModuleData::default())
    }

    pub fn modules_for_file(&self, file_id: FileId) -> impl Iterator<Item = LocalModuleId> + '_ {
        self.modules
            .iter()
            .filter(move |(_, data)| data.origin.file_id == file_id)
            .map(|(id, _)| id)
    }

    pub fn modules(&self) -> impl Iterator<Item = (LocalModuleId, &ModuleData)> + '_ {
        self.modules.iter()
    }

    pub fn diagnostics(&self) -> impl Iterator<Item = &DefDiagnostic> + '_ {
        self.diagnostics.iter()
    }

    pub fn lib(&self) -> LibId {
        self.lib
    }

    pub fn module_id(&self, local_id: LocalModuleId) -> ModuleId {
        ModuleId {
            lib: self.lib,
            local_id,
        }
    }
}

impl Index<LocalModuleId> for DefMap {
    type Output = ModuleData;

    fn index(&self, id: LocalModuleId) -> &Self::Output {
        &self.modules[id]
    }
}

impl Default for ModuleOrigin {
    fn default() -> Self {
        ModuleOrigin {
            declaration: AstId::new(FileId(0), FileAstId::DUMMY),
            file_id: FileId(0),
        }
    }
}
