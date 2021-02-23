use crate::db::DefDatabase;
use crate::def_map::path_resolution::{FixPoint, ResolveMode, ResolveResult};
use crate::def_map::DefMap;
use crate::id::LocalModuleId;
use crate::item_tree::{self, ItemTree, ItemTreeId};
use crate::path::ModPath;
use crate::per_ns::PerNs;
use base_db::input::FileId;
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
    path: ModPath,
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

impl<'a> DefCollector<'a> {
    fn seed_with_items(&mut self) {
        let source_root = self.db.lib_source_root(self.def_map.lib());
        let source_root = self.db.source_root(source_root);

        for file_id in source_root.files() {
            let module = self.db.parse(file_id).tree();
            let item_tree = self.db.item_tree(file_id);
            let ast_id_map = self.db.ast_id_map(file_id);
            let module_id = self.def_map.add_module();
            let declaration = ast_id_map.ast_id(&module).with_file_id(file_id);

            self.def_map.modules[module_id].origin = super::ModuleOrigin { declaration, file_id };
        }
    }

    fn finish(mut self) -> DefMap {
        for directive in &self.unresolved_imports {
            let import = &directive.import.source;
            let item_tree = self.db.item_tree(import.file_id);
            let import_data = &item_tree[import.value];

            eprintln!("unresolved import");
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
        let res = self.def_map.resolve_mod_path(self.db, ResolveMode::Import, module_id, &import.path);

        unimplemented!();
    }

    fn record_resolved_import(&mut self, directive: &ImportDirective) {
    }
}
