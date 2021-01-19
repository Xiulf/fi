use crate::db::CompilerDatabase;
use backend::BackendDatabase;
use hir::HirDatabase;
use serde::{Deserialize, Serialize};
use source::SourceDatabase;
use std::collections::HashMap;
use std::path::PathBuf;
use std::time::SystemTime;

#[derive(Debug, Serialize, Deserialize)]
pub struct LibMetadata {
    pub assemblies: HashMap<hir::ir::ModuleId, PathBuf>,
    pub last_modified: HashMap<PathBuf, SystemTime>,
}

impl CompilerDatabase {
    pub fn store_metadata(&self, lib: source::LibId) {
        let tree = self.module_tree(lib);
        let files = self.files();
        let assemblies = tree
            .toposort(self)
            .into_iter()
            .map(|data| (data.id, self.assembly(lib, data.id).path().to_owned()))
            .collect();
        let last_modified = self
            .lib_files(lib)
            .iter()
            .map(|file| {
                let path: PathBuf = files.name(*file).into();
                let modified = path.metadata().unwrap().modified().unwrap();

                (path, modified)
            })
            .collect();

        let file = std::fs::File::create(format!("{}/meta/lib", self.manifest(lib).package.target_dir.display())).unwrap();

        bincode::serialize_into(file, &LibMetadata { assemblies, last_modified }).unwrap();
        hir::module_tree::save_external(self, lib);
        hir::store_item_data(self, lib);
        typeck::external::store_external(self, lib);
    }

    pub fn load_metadata(&self, lib: source::LibId) -> Option<LibMetadata> {
        let path = format!("{}/meta/lib", self.manifest(lib).package.target_dir.display());
        let file = std::fs::File::open(&path).ok()?;

        bincode::deserialize_from(file).ok()
    }
}
