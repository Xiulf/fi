use base_db::input::FileId;
use cfg::CfgOptions;
use rustc_hash::{FxHashMap, FxHashSet};
use syntax::{ast, NameOwner};

use crate::db::DefDatabase;
use crate::def_map::path_resolution::{FixPoint, ResolveMode};
use crate::def_map::DefMap;
use crate::diagnostics::DefDiagnostic;
use crate::id::*;
use crate::in_file::InFile;
use crate::item_scope::{ExportGroup, ExportNs, ImportType, PerNsAllImports};
use crate::item_tree::{self, Item, ItemTree, ItemTreeId};
use crate::name::{AsName, Name};
use crate::path::Path;
use crate::per_ns::PerNs;
use crate::visibility::Visibility;

// const FIXED_POINT_LIMIT: usize = 8192;
const GLOBAL_RECURSION_LIMIT: usize = 100;

pub fn collect_defs(db: &dyn DefDatabase, def_map: DefMap) -> DefMap {
    let lib_data = db.libs();
    let mut collector = DefCollector {
        db,
        cfg_opts: &lib_data[def_map.lib].cfg_options,
        def_map,
        glob_imports: FxHashMap::default(),
        reexports: FxHashMap::default(),
        unresolved_imports: Vec::new(),
        resolved_imports: Vec::new(),
        all_imports: PerNsAllImports::default(),
    };

    for &dep in &db.libs()[collector.def_map.lib].deps {
        let dep_def_map = db.def_map(dep);

        for (local_id, module) in dep_def_map.modules() {
            let id = dep_def_map.module_id(local_id);

            collector.def_map.module_names.insert(module.name.clone(), id);
        }
    }

    collector.seed_with_items();
    collector.collect();
    collector.finish()
}

struct DefCollector<'a> {
    db: &'a dyn DefDatabase,
    cfg_opts: &'a CfgOptions,
    def_map: DefMap,
    glob_imports: FxHashMap<LocalModuleId, FxHashSet<LocalModuleId>>,
    reexports: FxHashMap<LocalModuleId, FxHashSet<LocalModuleId>>,
    unresolved_imports: Vec<ImportDirective>,
    resolved_imports: Vec<ImportDirective>,
    all_imports: PerNsAllImports,
}

struct ModCollector<'a, 'b> {
    def_collector: &'a mut DefCollector<'b>,
    module_id: LocalModuleId,
    file_id: FileId,
    item_tree: &'a ItemTree,
    export_all: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Import {
    source: ItemTreeId<item_tree::Import>,
    alias: Option<Name>,
    path: Path,
    is_glob: bool,
    visibility: Visibility,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum PartialResolvedImport {
    Unresolved,
    Indeterminate(PerNs),
    Resolved(PerNs),
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct ImportDirective {
    resolve_in: LocalModuleId,
    module_id: LocalModuleId,
    import: Import,
    status: PartialResolvedImport,
}

struct DefData<'a> {
    id: ModuleDefId,
    name: &'a Name,
    visibility: Visibility,
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
        let source_root = self.db.file_source_root(self.db.libs()[self.def_map.lib].root_file);
        let source_root = self.db.source_root(source_root);
        let mut modules = Vec::new();

        let mut seed = |this: &mut DefCollector, file_id, module: ast::ItemModule| -> Option<()> {
            let name = module.name()?.as_name();
            let exports = module.exports();
            let item_tree = this.db.item_tree(file_id);
            let ast_id_map = this.db.ast_id_map(file_id);
            let module_id = this.def_map.add_module(name.clone(), None);
            let declaration = ast_id_map.ast_id(&module).with_file_id(file_id);

            this.def_map.modules[module_id].origin = super::ModuleOrigin::Normal { declaration };

            let mut mcoll = ModCollector {
                def_collector: this,
                item_tree: &item_tree,
                module_id,
                file_id,
                export_all: false,
            };

            mcoll.collect_exports(exports);
            modules.push((file_id, module_id, mcoll.export_all));

            Some(())
        };

        for file in source_root.source_files() {
            let source_file = self.db.parse(file).tree();

            if let Some(module) = source_file.module() {
                seed(self, file, module);
            }
        }

        for (file_id, module_id, export_all) in modules {
            let item_tree = self.db.item_tree(file_id);
            let mut mcoll = ModCollector {
                def_collector: self,
                item_tree: &item_tree,
                module_id,
                file_id,
                export_all,
            };

            mcoll.collect(item_tree.top_level());
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
                directive.status = self.resolve_import(directive.resolve_in, &directive.import);

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
        let visibility = import.visibility;

        if def
            .filter_vis(|v| v.is_visible_from_def_map(&self.def_map, directive.resolve_in))
            .is_none()
        {
            let import = &import.source;
            let item_tree = self.db.item_tree(import.file_id);
            let import_data = &item_tree[import.value];

            self.def_map.diagnostics.push(DefDiagnostic::private_import(
                directive.resolve_in,
                InFile::new(import.file_id, import_data.ast_id),
                import_data.index,
            ));
        }

        if import.is_glob {
            if let Some(_) = &import.alias {
                unreachable!()
            } else {
                match def.modules {
                    | Some((ModuleDefId::ModuleId(m), _)) => {
                        if m.lib != self.def_map.lib {
                            let def_map = self.db.def_map(m.lib);
                            let scope = &def_map[m.local_id].scope;
                            let items = scope
                                .resolutions()
                                .map(|(n, r)| (n, r.filter_vis(|v| v == Visibility::Public)))
                                .filter(|(_, r)| !r.is_none())
                                .map(|(n, r)| (n, r.map(|(id, _)| id)))
                                .collect::<Vec<_>>();

                            self.update(module_id, &items, visibility, ImportType::Glob);
                        } else {
                            let scope = &self.def_map[m.local_id].scope;
                            let items = scope
                                .resolutions()
                                .map(|(n, r)| {
                                    (n, r.filter_vis(|v| v.is_visible_from_def_map(&self.def_map, module_id)))
                                })
                                .filter(|(_, r)| !r.is_none())
                                .map(|(n, r)| (n, r.map(|(id, _)| id)))
                                .collect::<Vec<_>>();

                            self.update(module_id, &items, visibility, ImportType::Glob);

                            let glob = self.glob_imports.entry(m.local_id).or_default();

                            glob.insert(module_id);

                            for m in self.def_map[m.local_id].exports.modules() {
                                if m.lib == self.def_map.lib {
                                    let glob = self.glob_imports.entry(m.local_id).or_default();

                                    glob.insert(module_id);
                                }
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

            self.update(
                module_id,
                &[(name, def.map(|(id, _)| id))],
                visibility,
                ImportType::Named,
            );
        }
    }

    fn update(
        &mut self,
        module_id: LocalModuleId,
        resolutions: &[(Name, PerNs<ModuleDefId>)],
        visibility: Visibility,
        import_type: ImportType,
    ) {
        self.db.check_canceled();
        self.update_recursive(module_id, resolutions, visibility, import_type, 0);
    }

    fn update_recursive(
        &mut self,
        module_id: LocalModuleId,
        resolutions: &[(Name, PerNs<ModuleDefId>)],
        visibility: Visibility,
        import_type: ImportType,
        depth: usize,
    ) {
        if depth > GLOBAL_RECURSION_LIMIT {
            panic!("infinite recursion in global imports");
        }

        let mut changed = false;

        for (name, res) in resolutions {
            let scope = &mut self.def_map.modules[module_id].scope;

            changed |= scope.push_res_with_import(
                &mut self.all_imports,
                (module_id, name.clone()),
                res.map(|id| (id, visibility)),
                import_type,
            );
        }

        if !changed {
            return;
        }

        for &m in self.reexports.get(&module_id).into_iter().flat_map(|v| v.iter()) {
            for (name, res) in resolutions {
                self.def_map.modules[m].scope.push_reexport(name.clone(), *res);
            }
        }

        let glob_imports = self
            .glob_imports
            .get(&module_id)
            .into_iter()
            .flat_map(|v| v.iter())
            .filter(|module| visibility.is_visible_from_def_map(&self.def_map, **module))
            .copied()
            .collect::<Vec<_>>();

        for glob_importing_module in glob_imports {
            self.update_recursive(
                glob_importing_module,
                resolutions,
                visibility,
                ImportType::Glob,
                depth + 1,
            );
        }

        // let reexports = self
        //     .reexports
        //     .get(&module_id)
        //     .into_iter()
        //     .flat_map(|v| v.iter())
        //     .flat_map(|m| self.glob_imports.get(m).into_iter().flat_map(|v| v.iter()))
        //     .copied()
        //     .collect::<Vec<_>>();
        //
        // for m in reexports {
        //     self.update_recursive(m, resolutions, visibility, ImportType::Glob, depth + 1);
        // }
    }
}

impl<'a, 'b> ModCollector<'a, 'b> {
    fn collect_exports(&mut self, exports: Option<ast::Exports>) {
        let def_map = &mut self.def_collector.def_map;

        if let Some(exports) = exports {
            for export in exports {
                match export {
                    | ast::Export::Module(name) => {
                        let path = crate::path::convert_path(name.path()).unwrap();

                        if path.to_ident() == def_map[self.module_id].name {
                            self.export_all = true;
                        } else {
                            let (res, _) = def_map.resolve_import(self.def_collector.db, self.module_id, &path);

                            if let Some((ModuleDefId::ModuleId(m), vis)) = res.modules {
                                if vis.is_visible_from_def_map(def_map, self.module_id) {
                                    def_map.modules[self.module_id].exports.add_module(m);

                                    if m.lib == def_map.lib {
                                        let entry = self.def_collector.reexports.entry(m.local_id).or_default();

                                        entry.insert(self.module_id);
                                    }
                                } else {
                                    todo!();
                                }
                            } else if let Some(name) = path.as_ident() {
                                let module_id = def_map.add_module(name.clone(), Some(self.module_id));
                                let entry = self.def_collector.reexports.entry(module_id).or_default();

                                entry.insert(self.module_id);
                                def_map.modules[module_id].exports.export_all = true;
                                def_map.modules[module_id].origin =
                                    super::ModuleOrigin::Virtual { parent: self.module_id };
                                // def_map.modules[self.module_id].children.insert(name.clone(), module_id);

                                let module_id = def_map.module_id(module_id);
                                let parent = def_map.module_id(self.module_id);

                                def_map.modules[self.module_id].scope.push_res_with_import(
                                    &mut self.def_collector.all_imports,
                                    (self.module_id, name.clone()),
                                    PerNs::modules((ModuleDefId::ModuleId(module_id), Visibility::Module(parent))),
                                    ImportType::Named,
                                );

                                def_map.modules[self.module_id].exports.add_module(module_id);
                            } else {
                                todo!();
                            }
                        }
                    },
                    | ast::Export::Name(name) => {
                        let name = name.name_ref().unwrap().as_name();

                        def_map.modules[self.module_id].exports.add_name(name, ExportNs::Any);
                    },
                    | ast::Export::Group(group) => {
                        let name = group.name_ref().unwrap().as_name();
                        let group = match group.kind().unwrap() {
                            | ast::ExportGroupKind::All(_) => ExportGroup::All,
                            | ast::ExportGroupKind::Named(g) => {
                                ExportGroup::Named(g.names().map(|n| n.as_name()).collect())
                            },
                        };

                        def_map.modules[self.module_id].exports.add_group(name, group);
                    },
                }
            }
        } else {
            self.export_all = true;
        }
    }

    fn resolve_visibility(&self, name: &Name, ns: ExportNs) -> Visibility {
        if self.export_all {
            return Visibility::Public;
        }

        let in_module = self.def_collector.def_map.module_id(self.module_id);
        let exports = &self.def_collector.def_map[self.module_id].exports;

        exports.resolve_visibility(name, ns, in_module)
    }

    fn resolve_module_visibility(&self, id: ModuleId) -> Visibility {
        let in_module = self.def_collector.def_map.module_id(self.module_id);
        let exports = &self.def_collector.def_map[self.module_id].exports;

        exports.resolve_module_visibility(id, in_module)
    }

    fn collect(&mut self, items: &[Item]) {
        let module = self.def_collector.def_map.module_id(self.module_id);
        let container = ContainerId::Module(module);

        for &item in items {
            let mut def = None;
            let attrs = self.item_tree.attrs(item.into());

            if let Some(cfg) = attrs.cfg() {
                if !cfg.is_enabled(self.def_collector.cfg_opts) {
                    continue;
                }
            }

            match item {
                | Item::Module(id) => {
                    let it = &self.item_tree[id];
                    let ast_map = self.def_collector.db.ast_id_map(self.file_id);
                    let parsed = self.def_collector.db.parse(self.file_id);
                    let node = ast_map.get(it.ast_id).to_node(&parsed.syntax_node());
                    let exports = node.exports();
                    let module_id = self
                        .def_collector
                        .def_map
                        .add_module(it.name.clone(), Some(self.module_id));

                    self.def_collector.def_map.modules[self.module_id]
                        .children
                        .insert(it.name.clone(), module_id);

                    self.def_collector.def_map.modules[module_id].origin = super::ModuleOrigin::Inline {
                        def_id: InFile::new(self.file_id, id),
                        def: it.ast_id.with_file_id(self.file_id),
                    };

                    let mut mcoll = ModCollector {
                        def_collector: self.def_collector,
                        item_tree: self.item_tree,
                        module_id,
                        file_id: self.file_id,
                        export_all: false,
                    };

                    mcoll.collect_exports(exports);
                    mcoll.collect(&it.items);

                    def = Some(DefData {
                        id: ModuleDefId::ModuleId(self.def_collector.def_map.module_id(module_id)),
                        name: &it.name,
                        visibility: Visibility::Module(module),
                    });
                },
                | Item::Import(id) => {
                    let it = &self.item_tree[id];

                    if let Some(qual) = &it.qualify {
                        let module_id = self.def_collector.def_map[self.module_id]
                            .scope
                            .get(qual)
                            .modules
                            .map(|id| match id.0 {
                                | ModuleDefId::ModuleId(id) => id.local_id,
                                | _ => unreachable!(),
                            })
                            .unwrap_or_else(|| {
                                let module_id = self
                                    .def_collector
                                    .def_map
                                    .add_module(qual.clone(), Some(self.module_id));

                                self.def_collector.def_map.modules[module_id].exports.export_all = true;
                                self.def_collector.def_map.modules[module_id].origin =
                                    super::ModuleOrigin::Virtual { parent: self.module_id };

                                def = Some(DefData {
                                    id: ModuleDefId::ModuleId(self.def_collector.def_map.module_id(module_id)),
                                    name: qual,
                                    visibility: self
                                        .resolve_module_visibility(self.def_collector.def_map.module_id(module_id)),
                                });

                                module_id
                            });

                        self.def_collector.unresolved_imports.push(ImportDirective {
                            resolve_in: self.module_id,
                            module_id,
                            status: PartialResolvedImport::Unresolved,
                            import: Import {
                                source: InFile::new(self.file_id, id),
                                alias: it.alias.clone(),
                                path: it.path.clone(),
                                is_glob: it.is_glob,
                                visibility: Visibility::Public,
                            },
                        });
                    } else {
                        self.def_collector.unresolved_imports.push(ImportDirective {
                            resolve_in: self.module_id,
                            module_id: self.module_id,
                            status: PartialResolvedImport::Unresolved,
                            import: Import {
                                source: InFile::new(self.file_id, id),
                                alias: it.alias.clone(),
                                path: it.path.clone(),
                                is_glob: it.is_glob,
                                visibility: Visibility::Module(module),
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
                        visibility: self.resolve_visibility(&it.name, ExportNs::Values),
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
                        visibility: self.resolve_visibility(&it.name, ExportNs::Values),
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
                        visibility: self.resolve_visibility(&it.name, ExportNs::Values),
                    });
                },
                | Item::Const(id) => {
                    let it = &self.item_tree[id];

                    def = Some(DefData {
                        id: ModuleDefId::ConstId(
                            ConstLoc {
                                id: ItemTreeId::new(self.file_id, id),
                                module,
                            }
                            .intern(self.def_collector.db),
                        ),
                        name: &it.name,
                        visibility: self.resolve_visibility(&it.name, ExportNs::Values),
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
                        visibility: self.resolve_visibility(&it.name, ExportNs::Types),
                    });
                },
                | Item::TypeCtor(id) => {
                    let it = &self.item_tree[id];
                    let new_id = TypeCtorLoc {
                        id: ItemTreeId::new(self.file_id, id),
                        module,
                    }
                    .intern(self.def_collector.db);

                    let visibility = self.resolve_visibility(&it.name, ExportNs::Types);
                    let group = self.def_collector.def_map[self.module_id]
                        .exports
                        .get_group(&it.name)
                        .cloned();

                    let export_all = self.export_all;
                    let item_vis = |name| match &group {
                        | Some(ExportGroup::All) => Visibility::Public,
                        | Some(ExportGroup::Named(n)) if n.contains(name) => Visibility::Public,
                        | Some(ExportGroup::Named(_)) => Visibility::Module(module),
                        | None if export_all => Visibility::Public,
                        | None => Visibility::Module(module),
                    };

                    for (i, local_id) in it.ctors.clone().enumerate() {
                        let attrs = self.item_tree.attrs(local_id.into());

                        if let Some(cfg) = attrs.cfg() {
                            if !cfg.is_enabled(self.def_collector.cfg_opts) {
                                continue;
                            }
                        }

                        let data = &self.item_tree[local_id];
                        let id = CtorId {
                            parent: new_id,
                            local_id: LocalCtorId::from_raw(arena::RawIdx::from(i as u32)),
                        };

                        let id = ModuleDefId::CtorId(id);
                        let vis = item_vis(&data.name);

                        self.def_collector.def_map.modules[self.module_id].scope.define_def(id);
                        self.def_collector.update(
                            self.module_id,
                            &[(data.name.clone(), PerNs::from(id))],
                            vis,
                            ImportType::Named,
                        );
                    }

                    def = Some(DefData {
                        id: ModuleDefId::TypeCtorId(new_id),
                        name: &it.name,
                        visibility,
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
                    let visibility = self.resolve_visibility(&it.name, ExportNs::Types);

                    for &(ref name, id) in data.items.iter() {
                        let it = match id {
                            | AssocItemId::FuncId(id) => Item::Func(id.lookup(self.def_collector.db).id.value),
                            | AssocItemId::StaticId(id) => Item::Static(id.lookup(self.def_collector.db).id.value),
                        };

                        let attrs = self.item_tree.attrs(it.into());

                        if let Some(cfg) = attrs.cfg() {
                            if !cfg.is_enabled(self.def_collector.cfg_opts) {
                                continue;
                            }
                        }

                        let id = match id {
                            | AssocItemId::FuncId(id) => ModuleDefId::FuncId(id),
                            | AssocItemId::StaticId(id) => ModuleDefId::StaticId(id),
                        };

                        self.def_collector.def_map.modules[self.module_id].scope.define_def(id);
                        self.def_collector.update(
                            self.module_id,
                            &[(name.clone(), PerNs::from(id))],
                            visibility,
                            ImportType::Named,
                        );
                    }

                    def = Some(DefData {
                        id: ModuleDefId::ClassId(new_id),
                        name: &it.name,
                        visibility,
                    });
                },
                | Item::Member(id) => {
                    let inst_id = MemberLoc {
                        id: ItemTreeId::new(self.file_id, id),
                        module,
                    }
                    .intern(self.def_collector.db);

                    self.def_collector.def_map.modules[self.module_id]
                        .scope
                        .define_member(inst_id);
                },
            }

            if let Some(DefData { id, name, visibility }) = def {
                self.def_collector.def_map.modules[self.module_id].scope.define_def(id);
                self.def_collector.update(
                    self.module_id,
                    &[(name.clone(), PerNs::from(id))],
                    visibility,
                    ImportType::Named,
                );
            }
        }
    }
}
