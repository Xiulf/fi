mod collector;
mod path_resolution;

use std::io;
use std::ops::Index;
use std::sync::Arc;

use arena::Arena;
use base_db::input::FileId;
use base_db::libs::LibId;
use rustc_hash::FxHashMap;
use syntax::ast;

use crate::ast_id::{AstId, FileAstId};
use crate::db::DefDatabase;
use crate::diagnostics::DefDiagnostic;
use crate::id::{LocalModuleId, ModuleId};
use crate::in_file::InFile;
use crate::item_scope::{ItemExports, ItemScope};
use crate::item_tree::{self, ItemTreeId};
use crate::name::Name;

#[derive(Debug, PartialEq, Eq)]
pub struct DefMap {
    lib: LibId,
    modules: Arena<ModuleData>,
    module_names: FxHashMap<Name, ModuleId>,
    diagnostics: Vec<DefDiagnostic>,
}

#[derive(Default, Debug, PartialEq, Eq)]
pub struct ModuleData {
    pub name: Name,
    pub scope: ItemScope,
    pub exports: ItemExports,
    pub origin: ModuleOrigin,
    pub parent: Option<LocalModuleId>,
    pub children: FxHashMap<Name, LocalModuleId>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ModuleOrigin {
    Normal {
        declaration: AstId<ast::ItemModule>,
    },
    Inline {
        def_id: ItemTreeId<item_tree::Module>,
        def: AstId<ast::ItemModule>,
    },
    Virtual {
        parent: LocalModuleId,
    },
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
            modules: Arena::default(),
            module_names: FxHashMap::default(),
            diagnostics: Vec::new(),
        }
    }

    pub(crate) fn add_module(&mut self, name: Name, parent: Option<LocalModuleId>) -> LocalModuleId {
        let local_id = self.modules.alloc(ModuleData {
            name: name.clone(),
            parent,
            ..ModuleData::default()
        });

        self.module_names.insert(name, ModuleId {
            lib: self.lib,
            local_id,
        });

        local_id
    }

    pub fn modules_for_file(&self, file_id: FileId) -> impl Iterator<Item = LocalModuleId> + '_ {
        self.modules
            .iter()
            .filter(move |(_, data)| data.origin.file_id() == Some(file_id))
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

    pub fn dump(&self, writer: &mut dyn io::Write) -> io::Result<()> {
        for (_module_id, module) in self.modules.iter() {
            writeln!(writer, "{}", module.name)?;
            module.scope.dump(writer)?;
            writeln!(writer)?;
        }

        Ok(())
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
        ModuleOrigin::Normal {
            declaration: AstId::new(FileId(0), FileAstId::DUMMY),
        }
    }
}

impl ModuleOrigin {
    pub fn file_id(&self) -> Option<FileId> {
        match *self {
            | ModuleOrigin::Normal { declaration } => Some(declaration.file_id),
            | _ => None,
        }
    }

    pub fn is_virtual(&self) -> bool {
        matches!(self, ModuleOrigin::Virtual { .. })
    }

    pub fn declaration(&self, db: &dyn DefDatabase, def_map: &DefMap) -> InFile<ast::ItemModule> {
        match self {
            | ModuleOrigin::Normal { declaration } => {
                let value = declaration.to_node(db);

                InFile {
                    file_id: declaration.file_id,
                    value,
                }
            },
            | ModuleOrigin::Inline { def, .. } => {
                let value = def.to_node(db);

                InFile {
                    file_id: def.file_id,
                    value,
                }
            },
            | ModuleOrigin::Virtual { parent } => def_map[*parent].origin.declaration(db, def_map),
        }
    }
}
