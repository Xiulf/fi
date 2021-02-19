use crate::arena::Arena;
use crate::ast_id::{AstId, FileAstId};
use crate::db::DefDatabase;
use crate::id::LocalModuleId;
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
    root: LocalModuleId,
    modules: Arena<ModuleData>,
    lib: LibId,
}

#[derive(Default, Debug, PartialEq, Eq)]
pub struct ModuleData {
    pub parent: Option<LocalModuleId>,
    pub children: FxHashMap<Name, LocalModuleId>,
    pub scope: ItemScope,
    pub origin: ModuleOrigin,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ModuleOrigin {
    File { declaration: AstId<ast::Module>, definition: FileId },
    Inline { definition: AstId<ast::Module> },
}

impl DefMap {
    pub fn def_map_query(db: &dyn DefDatabase, lib: LibId) -> Arc<DefMap> {
        let def_map = DefMap::empty(lib);
        // let def_map = collector::collect_defs(db, def_map, None);
        //
        // Arc::new(def_map)
        unimplemented!();
    }

    fn empty(lib: LibId) -> Self {
        let mut modules: Arena<ModuleData> = Arena::default();
        let root = modules.alloc(ModuleData::default());

        DefMap { root, modules, lib }
    }

    pub fn modules_for_file(&self, file_id: FileId) -> impl Iterator<Item = LocalModuleId> + '_ {
        self.modules
            .iter()
            .filter(move |(_, data)| data.origin.file_id() == Some(file_id))
            .map(|(id, _)| id)
    }
}

impl Index<LocalModuleId> for DefMap {
    type Output = ModuleData;

    fn index(&self, id: LocalModuleId) -> &Self::Output {
        &self.modules[id]
    }
}

impl ModuleOrigin {
    pub fn file_id(&self) -> Option<FileId> {
        match self {
            | ModuleOrigin::File { definition, .. } => Some(*definition),
            | _ => None,
        }
    }
}

impl Default for ModuleOrigin {
    fn default() -> Self {
        ModuleOrigin::File {
            declaration: AstId::new(FileId(0), FileAstId::DUMMY),
            definition: FileId(0),
        }
    }
}
