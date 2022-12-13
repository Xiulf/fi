use base_db::input::FileId;
use hir_def::child_by_source::ChildBySource;
use hir_def::dyn_map::DynMap;
use hir_def::id::{ClassId, DefWithBodyId, FuncId, ModuleId};
use hir_def::in_file::InFile;
use hir_def::keys::{self, Key};
use hir_def::name::AsName;
use hir_def::pat::PatId;
use rustc_hash::FxHashMap;
use syntax::{ast, AstNode, NameOwner, SyntaxNode};

use crate::db::HirDatabase;
use crate::semantics::SemanticsImpl;

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

pub trait ToDef: AstNode + Clone {
    type Def;

    fn to_def(sema: &SemanticsImpl, src: InFile<Self>) -> Option<Self::Def>;
}

impl<'a, 'b> SourceToDefCtx<'a, 'b> {
    pub fn file_to_def(&mut self, file: FileId) -> Vec<ModuleId> {
        let mut modules = Vec::with_capacity(1);

        for &lib in self.db.relevant_libs(file).iter() {
            let def_map = self.db.def_map(lib);

            modules.extend(def_map.modules_for_file(file).map(|id| def_map.module_id(id)));
        }

        modules
    }

    pub fn find_container(&mut self, src: InFile<&SyntaxNode>) -> Option<ChildContainer> {
        for container in src.ancestors() {
            if let Some(res) = self.container_to_def(container) {
                return Some(res);
            }
        }

        let def = self.file_to_def(src.file_id).get(0).copied()?;

        Some(def.into())
    }

    fn container_to_def(&mut self, container: InFile<SyntaxNode>) -> Option<ChildContainer> {
        let cont = syntax::match_ast! {
            match (container.value) {
                ItemModule(it) => self.module_to_def(container.with_value(it))?.into(),
                ItemFunc(it) => self.func_to_def(container.with_value(it))?.into(),
                _ => return None,
            }
        };

        Some(cont)
    }

    pub(super) fn module_to_def(&mut self, src: InFile<ast::ItemModule>) -> Option<ModuleId> {
        let parent_decl = src
            .syntax()
            .ancestors()
            .find_map(|it| it.map(ast::ItemModule::cast).transpose());

        let parent_module = match parent_decl {
            | Some(parent) => self.module_to_def(parent)?,
            | None => return self.file_to_def(src.file_id).get(0).copied(),
        };

        let def_map = self.db.def_map(parent_module.lib);
        let child_name = src.value.name()?.as_name();
        let child_id = *def_map[parent_module.local_id].children.get(&child_name)?;

        Some(def_map.module_id(child_id))
    }

    pub(super) fn func_to_def(&mut self, src: InFile<ast::ItemFunc>) -> Option<FuncId> {
        self.to_def(src, keys::FUNC)
    }

    pub(super) fn class_to_def(&mut self, src: InFile<ast::ItemClass>) -> Option<ClassId> {
        self.to_def(src, keys::CLASS)
    }

    pub(super) fn pat_bind_to_def(&mut self, src: InFile<ast::PatBind>) -> Option<(DefWithBodyId, PatId)> {
        let container = self.find_pat_container(src.syntax())?;
        let (body, source_map) = self.db.body_source_map(container);
        let src = src.map(ast::Pat::from);
        let pat_id = source_map.node_pat(src.as_ref())?;

        if let crate::Pat::Bind { .. } = body[pat_id] {
            Some((container, pat_id))
        } else {
            None
        }
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

    fn find_pat_container(&mut self, src: InFile<&SyntaxNode>) -> Option<DefWithBodyId> {
        for InFile { file_id, value } in src.ancestors() {
            let item = match ast::Item::cast(value) {
                | Some(it) => it,
                | None => continue,
            };

            let res: DefWithBodyId = match item {
                | ast::Item::Func(it) => self.func_to_def(InFile::new(file_id, it))?.into(),
                // | ast::Item::Static(it) => self.static_to_def(InFile::new(file_id, it))?.into(),
                // | ast::Item::Const(it) => self.const_to_def(InFile::new(file_id, it))?.into(),
                | _ => continue,
            };

            return Some(res);
        }

        None
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
