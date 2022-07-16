use base_db::input::FileId;
use hir_def::child_by_source::ChildBySource;
use hir_def::dyn_map::DynMap;
use hir_def::id::{DefWithBodyId, FuncId, ModuleId};
use hir_def::in_file::InFile;
use hir_def::keys::{self, Key};
use rustc_hash::FxHashMap;
use syntax::{ast, AstNode, SyntaxNode};

use crate::db::HirDatabase;

pub(super) type SourceToDefCache = FxHashMap<(ChildContainer, FileId), DynMap>;

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
        for node in src.value.ancestors().skip(1) {
            if let Some(res) = self.container_to_def(InFile::new(src.file_id, node)) {
                return Some(res);
            }
        }

        let def = self.file_to_def(src.file_id)?;

        Some(def.into())
    }

    fn container_to_def(&mut self, container: InFile<SyntaxNode>) -> Option<ChildContainer> {
        let cont = syntax::match_ast! {
            match (container.value) {
                Module(it) => self.module_to_def(container.with_value(it))?.into(),
                ItemFun(it) => self.func_to_def(container.with_value(it))?.into(),
                _ => return None,
            }
        };

        Some(cont)
    }

    fn module_to_def(&mut self, src: InFile<ast::Module>) -> Option<ModuleId> {
        let lib = self.db.file_lib(src.file_id);
        let def_map = self.db.def_map(lib);
        let mut modules = def_map.modules_for_file(src.file_id);

        modules.next().map(|id| def_map.module_id(id))
    }

    fn func_to_def(&mut self, src: InFile<ast::ItemFun>) -> Option<FuncId> {
        self.to_def(src, keys::FUNC)
    }

    fn to_def<N: AstNode + 'static, ID: Copy + 'static>(&mut self, src: InFile<N>, key: Key<N, ID>) -> Option<ID> {
        self.dyn_map(src.as_ref())?[key].get(&src).copied()
    }

    fn dyn_map<N: AstNode + 'static>(&mut self, src: InFile<&N>) -> Option<&DynMap> {
        let container = self.find_container(src.map(|it| it.syntax()))?;

        Some(self.cache_for(container, src.file_id))
    }

    fn cache_for(&mut self, container: ChildContainer, file_id: FileId) -> &DynMap {
        let db = self.db;

        self.cache
            .entry((container, file_id))
            .or_insert_with(|| container.child_by_source(db, file_id))
    }
}

impl ChildContainer {
    fn child_by_source(self, db: &dyn HirDatabase, file_id: FileId) -> DynMap {
        let db = db.upcast();

        match self {
            | ChildContainer::ModuleId(it) => it.child_by_source(db, file_id),
            | ChildContainer::DefwithBodyId(_it) => DynMap::default(),
        }
    }
}

impl From<DefWithBodyId> for ChildContainer {
    fn from(id: DefWithBodyId) -> Self {
        ChildContainer::DefwithBodyId(id)
    }
}

impl From<FuncId> for ChildContainer {
    fn from(id: FuncId) -> Self {
        ChildContainer::DefwithBodyId(DefWithBodyId::FuncId(id))
    }
}

impl From<ModuleId> for ChildContainer {
    fn from(id: ModuleId) -> Self {
        ChildContainer::ModuleId(id)
    }
}
