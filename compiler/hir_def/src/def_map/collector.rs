use crate::db::DefDatabase;
use crate::def_map::path_resolution::{FixPoint, ResolveMode, ResolveResult};
use crate::def_map::DefMap;
use crate::diagnostics::DefDiagnostic;
use crate::id::*;
use crate::in_file::InFile;
use crate::item_scope::{ImportType, PerNsAllImports};
use crate::item_tree::{self, Item, ItemTree, ItemTreeId};
use crate::name::{AsName, Name};
use crate::path::Path;
use crate::per_ns::PerNs;
use base_db::input::{FileId, FileTree};
use rustc_hash::FxHashMap;
use syntax::ast;

const FIXED_POINT_LIMIT: usize = 8192;
const GLOBAL_RECURSION_LIMIT: usize = 100;

pub fn collect_defs(db: &dyn DefDatabase, def_map: DefMap) -> DefMap {
    let mut collector = DefCollector {
        db,
        def_map,
        glob_imports: FxHashMap::default(),
        unresolved_imports: Vec::new(),
        resolved_imports: Vec::new(),
        from_all_import: PerNsAllImports::default(),
    };

    for &dep in &db.libs()[collector.def_map.lib].deps {
        let dep_def_map = db.def_map(dep);
        let name = db.libs()[dep].name.as_str().as_name();
        let root = dep_def_map.module_id(dep_def_map.root);
        let root = ModuleDefId::ModuleId(root);

        collector.def_map.extern_prelude.insert(name, root);
    }

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
    from_all_import: PerNsAllImports,
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
    path: Path,
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
}

impl PartialResolvedImport {
    fn namespaces(&self) -> PerNs {
        match *self {
            | PartialResolvedImport::Unresolved => unreachable!(),
            | PartialResolvedImport::Resolved(ns) => ns,
            | PartialResolvedImport::Indeterminate(ns) => ns,
        }
    }
}

impl<'a> DefCollector<'a> {
    fn seed_with_items(&mut self) {
        let file_tree = self.db.file_tree(self.def_map.lib);

        if let None = go(self, &file_tree, file_tree.root()) {
            let module_id = self.def_map.add_module(Name::default());

            self.def_map.root = module_id;
        }

        fn go(this: &mut DefCollector, file_tree: &FileTree, file_id: FileId) -> Option<(Name, LocalModuleId)> {
            use syntax::ast::NameOwner;
            let module = this.db.parse(file_id).tree();
            let name = module.name()?.as_name();
            let item_tree = this.db.item_tree(file_id);
            let ast_id_map = this.db.ast_id_map(file_id);
            let module_id = this.def_map.add_module(name.clone());
            let declaration = ast_id_map.ast_id(&module).with_file_id(file_id);

            if file_id == file_tree.root() {
                let id = this.def_map.module_id(module_id);
                let id = ModuleDefId::ModuleId(id);

                this.def_map.root = module_id;
                this.def_map.extern_prelude.insert(name.clone(), id);
            }

            for child in file_tree.children(file_id) {
                if let Some((name, child)) = go(this, file_tree, child) {
                    let module = this.def_map.module_id(child);
                    let def = ModuleDefId::ModuleId(module);

                    this.def_map.modules[module_id].children.insert(name.clone(), child);
                    this.def_map.modules[module_id].scope.define_def(def);
                    this.def_map.modules[child].parent = Some(module_id);
                    this.update(module_id, &[(name, PerNs::modules(def))], ImportType::Named);
                }
            }

            this.def_map.modules[module_id].origin = super::ModuleOrigin::Normal { declaration };

            let mut mcoll = ModCollector {
                def_collector: this,
                item_tree: &item_tree,
                module_id,
                file_id,
            };

            mcoll.collect(item_tree.top_level());
            mcoll.collect_exports(module.exports());

            Some((name, module_id))
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
        let module_id = directive.module_id;
        let import = &directive.import;
        let def = directive.status.namespaces();

        if import.is_glob {
            if let Some(alias) = &import.alias {
                self.update(module_id, &[(alias.clone(), def)], ImportType::Named);
            } else {
                match def.modules {
                    | Some(ModuleDefId::ModuleId(m)) => {
                        if m.lib != self.def_map.lib {
                            let item_map = self.db.def_map(m.lib);
                            let scope = &item_map[m.local_id].scope;
                            let items = scope
                                .resolutions()
                                .filter(|(_, res)| !res.is_none())
                                .collect::<Vec<_>>();

                            self.update(module_id, &items, ImportType::Glob);
                        } else {
                            let scope = &self.def_map[m.local_id].scope;
                            let items = scope
                                .resolutions()
                                .filter(|(_, res)| !res.is_none())
                                .collect::<Vec<_>>();

                            self.update(module_id, &items, ImportType::Glob);

                            let glob = self.glob_imports.entry(m.local_id).or_default();

                            if !glob.iter().any(|mid| *mid == module_id) {
                                glob.push(module_id);
                            }
                        }
                    },
                    | Some(_) => unreachable!("invalid type stored in module namespace"),
                    | None => unreachable!("glob import didn't resolve to a module"),
                }
            }
        } else {
            let last_segment = import.path.segments().last().unwrap();
            let name = match &import.alias {
                | Some(alias) => alias.clone(),
                | None => last_segment.clone(),
            };

            self.update(module_id, &[(name, def)], ImportType::Named);
        }
    }

    fn update(&mut self, module_id: LocalModuleId, resolutions: &[(Name, PerNs)], import_type: ImportType) {
        self.db.check_canceled();
        self.update_recursive(module_id, resolutions, import_type, 0);
    }

    fn update_recursive(
        &mut self,
        module_id: LocalModuleId,
        resolutions: &[(Name, PerNs)],
        import_type: ImportType,
        depth: usize,
    ) {
        if depth > GLOBAL_RECURSION_LIMIT {
            panic!("infinite recursion in global imports");
        }

        let mut changed = false;

        for (name, res) in resolutions {
            let module = &mut self.def_map.modules[module_id];

            changed |= module.scope.push_res_with_import(
                &mut self.from_all_import,
                (module_id, name.clone()),
                *res,
                import_type,
            );
        }

        if !changed {
            return;
        }

        let glob_imports = self
            .glob_imports
            .get(&module_id)
            .into_iter()
            .flat_map(|v| v.iter())
            .copied()
            .collect::<Vec<_>>();

        for glob_importing_module in glob_imports {
            self.update_recursive(glob_importing_module, resolutions, ImportType::Glob, depth + 1);
        }
    }
}

impl<'a, 'b> ModCollector<'a, 'b> {
    fn collect_exports(&mut self, exports: Option<ast::Exports>) {
        let def_map = &mut self.def_collector.def_map;

        if let Some(exports) = exports {
            for export in exports {
                match export {
                    | ast::Export::Module(name) => {
                        let name = name.name_ref().unwrap().as_name();

                        if name == def_map[self.module_id].name {
                            Self::collect_module_exports(def_map, self.module_id);
                        } else if let Some(ModuleDefId::ModuleId(m)) = def_map[self.module_id].scope.get(&name).modules
                        {
                            def_map.modules[self.module_id].exports.add_module(m);
                        } else {
                            eprintln!("not implemented at {}", std::panic::Location::caller());
                        }
                    },
                    | ast::Export::Name(name) => {
                        let name = name.name_ref().unwrap().as_name();

                        def_map.modules[self.module_id].exports.add_name(name);
                    },
                }
            }
        } else {
            Self::collect_module_exports(def_map, self.module_id);
        }
    }

    fn collect_module_exports(def_map: &mut DefMap, module_id: LocalModuleId) {
        for (name, res) in def_map[module_id].scope.resolutions().collect::<Vec<_>>() {
            if let Some(ModuleDefId::ModuleId(m)) = res.modules {
                // at this point imports are not yet resolved and thus the module must be from
                // this lib.
                if def_map[m.local_id].origin.is_virtual() {
                    continue;
                }
            }

            def_map.modules[module_id].exports.add_name(name);
        }
    }

    fn collect(&mut self, items: &[Item]) {
        let lib = self.def_collector.def_map.lib;

        for &item in items {
            let module = self.def_collector.def_map.module_id(self.module_id);
            let container = ContainerId::Module(module);
            let mut def = None;

            match item {
                | Item::Import(id) => {
                    let it = &self.item_tree[id];

                    if let Some(qual) = &it.qualify {
                        let module_id = self.def_collector.def_map[self.module_id]
                            .scope
                            .get(qual)
                            .modules
                            .map(|id| match id {
                                | ModuleDefId::ModuleId(id) => id.local_id,
                                | _ => unreachable!(),
                            })
                            .unwrap_or_else(|| {
                                let module_id = self.def_collector.def_map.add_module(qual.clone());

                                self.def_collector.def_map.modules[module_id].exports.export_all = true;
                                self.def_collector.def_map.modules[module_id].origin =
                                    super::ModuleOrigin::Virtual { parent: self.module_id };

                                module_id
                            });

                        self.def_collector.unresolved_imports.push(ImportDirective {
                            module_id,
                            status: PartialResolvedImport::Unresolved,
                            import: Import {
                                source: InFile::new(self.file_id, id),
                                alias: it.alias.clone(),
                                path: it.path.clone(),
                                is_glob: it.is_glob,
                            },
                        });

                        def = Some(DefData {
                            id: ModuleDefId::ModuleId(self.def_collector.def_map.module_id(module_id)),
                            name: qual,
                        });
                    } else {
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
                    }
                },
                | Item::Fixity(id) => {
                    let it = &self.item_tree[id];

                    def = Some(DefData {
                        id: ModuleDefId::FixityId(
                            FixityLoc {
                                id: ItemTreeId::new(self.file_id, id),
                                module,
                            }
                            .intern(self.def_collector.db),
                        ),
                        name: &it.name,
                    });
                },
                | Item::Func(id) => {
                    let it = &self.item_tree[id];

                    def = Some(DefData {
                        id: ModuleDefId::FuncId(
                            FuncLoc {
                                id: ItemTreeId::new(self.file_id, id),
                                container,
                            }
                            .intern(self.def_collector.db),
                        ),
                        name: &it.name,
                    });
                },
                | Item::Static(id) => {
                    let it = &self.item_tree[id];

                    def = Some(DefData {
                        id: ModuleDefId::StaticId(
                            StaticLoc {
                                id: ItemTreeId::new(self.file_id, id),
                                container,
                            }
                            .intern(self.def_collector.db),
                        ),
                        name: &it.name,
                    });
                },
                | Item::Const(id) => {
                    let it = &self.item_tree[id];

                    def = Some(DefData {
                        id: ModuleDefId::ConstId(
                            ConstLoc {
                                id: ItemTreeId::new(self.file_id, id),
                                container,
                            }
                            .intern(self.def_collector.db),
                        ),
                        name: &it.name,
                    });
                },
                | Item::TypeAlias(id) => {
                    let it = &self.item_tree[id];

                    def = Some(DefData {
                        id: ModuleDefId::TypeAliasId(
                            TypeAliasLoc {
                                id: ItemTreeId::new(self.file_id, id),
                                module,
                            }
                            .intern(self.def_collector.db),
                        ),
                        name: &it.name,
                    });
                },
                | Item::TypeCtor(id) => {
                    let it = &self.item_tree[id];
                    let new_id = TypeCtorLoc {
                        id: ItemTreeId::new(self.file_id, id),
                        module,
                    }
                    .intern(self.def_collector.db);

                    if let None = it.kind {
                        let data = self.def_collector.db.type_ctor_data(new_id);

                        for (local_id, data) in data.ctors.iter() {
                            let id = CtorId {
                                parent: new_id,
                                local_id,
                            };

                            let id = ModuleDefId::CtorId(id);

                            self.def_collector.def_map.modules[self.module_id].scope.define_def(id);
                            self.def_collector.update(
                                self.module_id,
                                &[(data.name.clone(), PerNs::from(id))],
                                ImportType::Named,
                            );
                        }
                    }

                    def = Some(DefData {
                        id: ModuleDefId::TypeCtorId(new_id),
                        name: &it.name,
                    });
                },
                | Item::Class(id) => {
                    let it = &self.item_tree[id];
                    let new_id = ClassLoc {
                        id: ItemTreeId::new(self.file_id, id),
                        module,
                    }
                    .intern(self.def_collector.db);

                    let data = self.def_collector.db.class_data(new_id);

                    for (name, id) in data.items.iter() {
                        let id = match *id {
                            | AssocItemId::FuncId(id) => ModuleDefId::FuncId(id),
                            | AssocItemId::StaticId(id) => ModuleDefId::StaticId(id),
                        };

                        self.def_collector.def_map.modules[self.module_id].scope.define_def(id);
                        self.def_collector.update(
                            self.module_id,
                            &[(name.clone(), PerNs::from(id))],
                            ImportType::Named,
                        );
                    }

                    def = Some(DefData {
                        id: ModuleDefId::ClassId(new_id),
                        name: &it.name,
                    });
                },
                | Item::Instance(id) => {
                    let it = &self.item_tree[id];
                    let inst_id = InstanceLoc {
                        id: ItemTreeId::new(self.file_id, id),
                        module,
                    }
                    .intern(self.def_collector.db);

                    self.def_collector.def_map.modules[self.module_id]
                        .scope
                        .define_instance(inst_id);
                },
            }

            if let Some(DefData { id, name }) = def {
                self.def_collector.def_map.modules[self.module_id].scope.define_def(id);
                self.def_collector
                    .update(self.module_id, &[(name.clone(), PerNs::from(id))], ImportType::Named);
            }
        }
    }
}
