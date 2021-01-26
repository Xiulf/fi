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
    pub compiler_version: String,
    pub last_modified: HashMap<PathBuf, SystemTime>,
    pub bins: HashMap<String, PathBuf>,
}

impl CompilerDatabase {
    pub fn store_metadata(&self, lib: source::LibId, compiler_version: String) {
        backend::store_external_assemblies(self, lib);
        typeck::external::store_external(self, lib);
        hir::store_item_data(self, lib);
        hir::module_tree::save_external(self, lib);
        hir::lang::store_external(self, lib);

        let files = self.files();
        let last_modified = self
            .lib_files(lib)
            .iter()
            .map(|file| {
                let path: PathBuf = files.name(*file).into();
                let modified = path.metadata().unwrap().modified().unwrap();

                (path, modified)
            })
            .collect();

        let tree = self.module_tree(lib);
        let bins = tree
            .data
            .iter()
            .filter_map(|d| {
                let asm = self.assembly(lib, d.id);

                if let linker::LinkOutputType::Exe = asm.kind {
                    Some((d.name.to_string(), asm.path().to_owned()))
                } else {
                    None
                }
            })
            .collect();

        let file = std::fs::File::create(format!("{}/meta/lib", self.manifest(lib).package.target_dir.display())).unwrap();

        bincode::serialize_into(file, &LibMetadata {
            compiler_version,
            last_modified,
            bins,
        })
        .unwrap();
    }

    pub fn load_metadata(&self, lib: source::LibId) -> Option<LibMetadata> {
        let path = format!("{}/meta/lib", self.manifest(lib).package.target_dir.display());
        let file = std::fs::File::open(&path).ok()?;

        bincode::deserialize_from(file).ok()
    }
}
