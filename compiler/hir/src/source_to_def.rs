use crate::db::HirDatabase;
use base_db::input::FileId;
use hir_def::dyn_map::DynMap;
use hir_def::id::{DefWithBodyId, ModuleId};
use hir_def::in_file::InFile;
use rustc_hash::FxHashMap;
use syntax::SyntaxNode;

pub(super) type SourceToDefCache = FxHashMap<ChildContainer, DynMap>;

pub(super) struct SourceToDefCtx<'a, 'b> {
    pub db: &'b dyn HirDatabase,
    pub cache: &'a mut SourceToDefCache,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(super) enum ChildContainer {
    DefwithBodyId(DefWithBodyId),
    ModuleId(ModuleId),
}

impl<'a, 'b> SourceToDefCtx<'a, 'b> {
    pub fn file_to_def(&mut self, file: FileId) -> Option<ModuleId> {
        let lib = self.db.file_lib(file);
        let def_map = self.db.def_map(lib);
        let module = def_map.modules_for_file(file).next()?;

        Some(def_map.module_id(module))
    }

    pub fn find_container(&mut self, src: InFile<&SyntaxNode>) -> Option<ChildContainer> {
        let def = self.file_to_def(src.file_id)?;

        Some(def.into())
    }
}

impl From<DefWithBodyId> for ChildContainer {
    fn from(id: DefWithBodyId) -> Self {
        ChildContainer::DefwithBodyId(id)
    }
}

impl From<ModuleId> for ChildContainer {
    fn from(id: ModuleId) -> Self {
        ChildContainer::ModuleId(id)
    }
}
