use std::fs::File;
use std::io;
use std::sync::Arc;
use std::time::SystemTime;

use cfg::CfgOptions;
use hir::db::HirDatabase;
use hir::id::LibId;
use paths::AbsPath;
use rustc_hash::{FxHashMap, FxHashSet};
use serde::{Deserialize, Serialize};
use vfs::VfsPath;

#[derive(Default, Debug, PartialEq, Eq, Deserialize, Serialize)]
pub struct Metadata {
    version: usize,

    #[serde(skip, default)]
    lib: LibId,
    files: FxHashMap<VfsPath, SystemTime>,
}

impl Metadata {
    const EXTENSION: &'static str = "metadata";

    pub fn has_changed(&self, db: &dyn HirDatabase) -> bool {
        if self.version != 0 {
            return true;
        }

        let source_root = db.file_source_root(db.libs()[self.lib].root_file);
        let source_root = db.source_root(source_root);
        let mut checked = FxHashSet::default();

        for file_id in source_root.iter() {
            if self.file_changed(&mut checked, source_root.path_for_file(file_id).unwrap()) {
                return true;
            }
        }

        let files = self.files.keys().collect();
        let mut diff = checked.difference(&files);

        diff.next().is_some()
    }

    fn file_changed<'a>(&self, checked: &mut FxHashSet<&'a VfsPath>, path: &'a VfsPath) -> bool {
        match self.files.get(path) {
            | None => return true,
            | Some(&timestamp) => {
                let path = path.as_path().unwrap().as_ref();
                let meta = match path.metadata() {
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

        checked.insert(path);
        false
    }
}

pub fn read_metadata(db: &dyn HirDatabase, lib: LibId, target_dir: &AbsPath) -> Option<Arc<Metadata>> {
    let metadata_dir = target_dir.join(metadata_name(db, lib));

    if let Ok(mut file) = File::open(metadata_dir) {
        let config = bincode::config::standard();
        let mut meta: Metadata = bincode::serde::decode_from_std_read(&mut file, config).ok()?;

        meta.lib = lib;

        Some(Arc::new(meta))
    } else {
        None
    }
}

pub fn write_metadata(db: &dyn HirDatabase, lib: LibId, target_dir: &AbsPath) -> io::Result<()> {
    let source_root = db.file_source_root(db.libs()[lib].root_file);
    let source_root = db.source_root(source_root);
    let metadata_dir = target_dir.join(metadata_name(db, lib));
    let mut metadata = Metadata::default();

    for file_id in source_root.iter() {
        let path = source_root.path_for_file(file_id).unwrap();
        let path_buf = path.as_path().unwrap();
        let meta = path_buf.as_ref().metadata()?;
        let timestamp = meta.modified()?;

        metadata.files.insert(path.clone(), timestamp);
    }

    let mut file = File::create(metadata_dir)?;
    let config = bincode::config::standard();

    match bincode::serde::encode_into_std_write(&metadata, &mut file, config) {
        | Ok(_) => Ok(()),
        | Err(e) => match e {
            | bincode::error::EncodeError::Io { error, .. } => Err(error),
            | _ => panic!("write_metadata: {}", e),
        },
    }
}

fn metadata_name(db: &dyn HirDatabase, lib: LibId) -> String {
    let libs = db.libs();
    let data = &libs[lib];
    let cfg_hash = hash_cfg(&data.cfg_options);

    format!("{}-{:X}.{}", data.name, cfg_hash, Metadata::EXTENSION)
}

fn hash_cfg(cfg: &CfgOptions) -> u64 {
    use std::hash::{Hash, Hasher};
    let mut hasher = rustc_hash::FxHasher::default();

    for flag in cfg.flags().iter() {
        flag.hash(&mut hasher);
    }

    for (key, value) in cfg.keys().iter() {
        key.hash(&mut hasher);
        value.hash(&mut hasher);
    }

    hasher.finish()
}
