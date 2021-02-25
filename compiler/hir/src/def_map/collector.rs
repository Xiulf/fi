use crate::db::DefDatabase;
use crate::def_map::path_resolution::{FixPoint, ResolveMode, ResolveResult};
use crate::def_map::DefMap;
use crate::diagnostics::DefDiagnostic;
use crate::id::{ContainerId, FuncLoc, Intern, LocalModuleId, ModuleDefId};
use crate::in_file::InFile;
use crate::item_tree::{self, Item, ItemTree, ItemTreeId};
use crate::name::Name;
use crate::path::ModPath;
use crate::per_ns::PerNs;
use base_db::input::{FileId, FileTree};
use rustc_hash::FxHashMap;

const FIXED_POINT_LIMIT: usize = 8192;

pub fn collect_defs(db: &dyn DefDatabase, def_map: DefMap) -> DefMap {
    let mut collector = DefCollector {
        db,
        def_map,
        glob_imports: FxHashMap::default(),
        unresolved_imports: Vec::new(),
        resolved_imports: Vec::new(),
    };

    collector.seed_with_items();
    collector.collect();
    collector.finish()
}

struct DefCollector<'a> {
    db: &'a dyn DefDatabase,
    def_map: DefMap,
    glob_imports: FxHashMap<LocalModuleId, Vec<LocalModuleId>>,
    unresolved_imports: Vec<ImportDirective>,
    resolved_imports: Vec<ImportDirective>,
}

struct ModCollector<'a, 'b> {
    def_collector: &'a mut DefCollector<'b>,
    module_id: LocalModuleId,
    file_id: FileId,
    item_tree: &'a ItemTree,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Import {
    source: ItemTreeId<item_tree::Import>,
    alias: Option<Name>,
    path: ModPath,
    is_glob: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum PartialResolvedImport {
    Unresolved,
    Indeterminate(PerNs),
    Resolved(PerNs),
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct ImportDirective {
    module_id: LocalModuleId,
    import: Import,
    status: PartialResolvedImport,
}

struct DefData<'a> {
    id: ModuleDefId,
    name: &'a Name,
    has_ctor: bool,
}

impl<'a> DefCollector<'a> {
    fn seed_with_items(&mut self) {
        let root_file = self.db.libs()[self.def_map.lib].root_file;
        let file_tree = self.db.file_tree(self.def_map.lib);

        go(self, &file_tree, root_file);

        fn go(this: &mut DefCollector, file_tree: &FileTree, file_id: FileId) -> (Name, LocalModuleId) {
            use crate::name::AsName;
            use syntax::ast::NameOwner;
            let module = this.db.parse(file_id).tree();
            let name = module.name().unwrap().as_name();
            let item_tree = this.db.item_tree(file_id);
            let ast_id_map = this.db.ast_id_map(file_id);
            let module_id = this.def_map.add_module();
            let declaration = ast_id_map.ast_id(&module).with_file_id(file_id);

            this.def_map.modules[module_id].origin = super::ModuleOrigin { declaration, file_id };

            ModCollector {
                def_collector: this,
                item_tree: &item_tree,
                module_id,
                file_id,
            }
            .collect(item_tree.top_level());

            for child in file_tree.children(file_id) {
                let (name, child) = go(this, file_tree, child);
                let module = this.def_map.module_id(child);
                let def = ModuleDefId::ModuleId(module);

                this.def_map.modules[module_id].children.insert(name, child);
                this.def_map.modules[module_id].scope.define_def(def);
            }

            (name, module_id)
        }
    }

    fn finish(mut self) -> DefMap {
        for directive in &self.unresolved_imports {
            let import = &directive.import.source;
            let item_tree = self.db.item_tree(import.file_id);
            let import_data = &item_tree[import.value];

            self.def_map.diagnostics.push(DefDiagnostic::unresolved_import(
                directive.module_id,
                InFile::new(import.file_id, import_data.ast_id),
                import_data.index,
            ));
        }

        self.def_map
    }

    fn collect(&mut self) {
        self.db.check_canceled();
        self.resolve_imports();
    }

    fn resolve_imports(&mut self) {
        let mut n_prev_unresolved = self.unresolved_imports.len() + 1;

        while self.unresolved_imports.len() < n_prev_unresolved {
            n_prev_unresolved = self.unresolved_imports.len();

            let imports = std::mem::replace(&mut self.unresolved_imports, Vec::new());

            for mut directive in imports {
                directive.status = self.resolve_import(directive.module_id, &directive.import);

                match directive.status {
                    | PartialResolvedImport::Indeterminate(_) => {
                        self.record_resolved_import(&directive);
                        self.resolved_imports.push(directive);
                    },
                    | PartialResolvedImport::Resolved(_) => {
                        self.record_resolved_import(&directive);
                        self.resolved_imports.push(directive);
                    },
                    | PartialResolvedImport::Unresolved => {
                        self.unresolved_imports.push(directive);
                    },
                }
            }
        }
    }

    fn resolve_import(&self, module_id: LocalModuleId, import: &Import) -> PartialResolvedImport {
        let res = self
            .def_map
            .resolve_mod_path(self.db, ResolveMode::Import, module_id, &import.path);
        let def = res.resolved_def;

        if res.fixpoint == FixPoint::No || def.is_none() {
            return PartialResolvedImport::Unresolved;
        }

        if let Some(lib) = res.lib {
            if lib != self.def_map.lib {
                return PartialResolvedImport::Resolved(def);
            }
        }

        if def.types.is_some() && def.values.is_some() && def.modules.is_some() {
            PartialResolvedImport::Resolved(def)
        } else {
            PartialResolvedImport::Indeterminate(def)
        }
    }

    fn record_resolved_import(&mut self, directive: &ImportDirective) {
    }
}

impl<'a, 'b> ModCollector<'a, 'b> {
    fn collect(&mut self, items: &[Item]) {
        let lib = self.def_collector.def_map.lib;

        for &item in items {
            let module = self.def_collector.def_map.module_id(self.module_id);
            let container = ContainerId::Module(module);
            let mut def = None;

            match item {
                | Item::Import(id) => {
                    let it = &self.item_tree[id];

                    self.def_collector.unresolved_imports.push(ImportDirective {
                        module_id: self.module_id,
                        status: PartialResolvedImport::Unresolved,
                        import: Import {
                            source: InFile::new(self.file_id, id),
                            alias: it.alias.clone(),
                            path: it.path.clone(),
                            is_glob: it.is_glob,
                        },
                    });
                },
                | Item::Func(id) => {
                    let func = &self.item_tree[id];

                    def = Some(DefData {
                        id: ModuleDefId::FuncId(
                            FuncLoc {
                                id: ItemTreeId::new(self.file_id, id),
                                container,
                            }
                            .intern(self.def_collector.db),
                        ),
                        name: &func.name,
                        has_ctor: false,
                    });
                },
                | _ => unimplemented!(),
            }

            if let Some(DefData { id, name, has_ctor }) = def {
                self.def_collector.def_map.modules[self.module_id].scope.define_def(id);
            }
        }
    }
}
