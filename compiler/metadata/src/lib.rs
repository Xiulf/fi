use std::fs::File;
use std::io;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::time::SystemTime;

use hir::db::HirDatabase;
use hir::id::LibId;
use relative_path::RelativePath;
use rustc_hash::{FxHashMap, FxHashSet};
use serde::{Deserialize, Serialize};

#[derive(Default, Debug, PartialEq, Eq, Deserialize, Serialize)]
pub struct Metadata {
    version: usize,

    #[serde(skip, default)]
    lib: LibId,
    files: FxHashMap<String, SystemTime>,
}

impl Metadata {
    pub fn has_changed(&self, db: &dyn HirDatabase) -> bool {
        if self.version != 0 {
            return true;
        }

        let source_root = db.lib_source_root(self.lib);
        let source_root = db.source_root(source_root);

        if let Some(dir) = &source_root.dir {
            let mut checked = FxHashSet::default();

            if self.file_changed(&mut checked, dir, RelativePath::new("shadow.toml")) {
                return true;
            }

            for (_, path) in source_root.iter() {
                if self.file_changed(&mut checked, dir, path) {
                    return true;
                }
            }

            let files = self.files.keys().map(|f| f.as_str()).collect();
            let mut diff = checked.difference(&files);

            return diff.next().is_some();
        }

        true
    }

    fn file_changed<'a>(&self, checked: &mut FxHashSet<&'a str>, dir: &PathBuf, path: &'a RelativePath) -> bool {
        match self.files.get(path.as_str()) {
            | None => return true,
            | Some(&timestamp) => {
                let path_buf = path.to_logical_path(dir);
                let meta = match path_buf.metadata() {
                    | Err(_) => return true,
                    | Ok(m) => m,
                };

                let t = match meta.modified() {
                    | Err(_) => return true,
                    | Ok(t) => t,
                };

                if t != timestamp {
                    return true;
                }
            },
        }

        checked.insert(path.as_str());
        false
    }
}

pub fn read_metadata(db: &dyn HirDatabase, lib: LibId, target_dir: &Path) -> Option<Arc<Metadata>> {
    let metadata_dir = target_dir.join(&db.libs()[lib].name).with_extension("metadata");

    if let Ok(mut file) = File::open(metadata_dir) {
        let config = bincode::config::standard();
        let mut meta: Metadata = bincode::serde::decode_from_std_read(&mut file, config).ok()?;

        meta.lib = lib;

        Some(Arc::new(meta))
    } else {
        None
    }
}

pub fn write_metadata(db: &dyn HirDatabase, lib: LibId, target_dir: &Path) -> io::Result<()> {
    let source_root = db.lib_source_root(lib);
    let source_root = db.source_root(source_root);

    if let Some(dir) = &source_root.dir {
        let metadata_dir = target_dir.join(&db.libs()[lib].name).with_extension("metadata");
        let mut metadata = Metadata::default();

        {
            let path = RelativePath::new("shadow.toml");
            let path_buf = path.to_logical_path(dir);
            let meta = path_buf.metadata()?;
            let timestamp = meta.modified()?;

            metadata.files.insert(path.to_string(), timestamp);
        }

        for (_, path) in source_root.iter() {
            let path_buf = path.to_logical_path(dir);
            let meta = path_buf.metadata()?;
            let timestamp = meta.modified()?;

            metadata.files.insert(path.to_string(), timestamp);
        }

        let mut file = File::create(metadata_dir)?;
        let config = bincode::config::standard();

        return match bincode::serde::encode_into_std_write(&metadata, &mut file, config) {
            | Ok(_) => Ok(()),
            | Err(e) => match e {
                | bincode::error::EncodeError::Io { error, .. } => Err(error),
                | _ => panic!("write_metadata: {}", e),
            },
        };
    }

    Ok(())
}
