use crate::db::RootDatabase;
use anyhow::{Context, Result};
use base_db::input::{FileId, SourceRoot, SourceRootId};
use base_db::libs::{LibId, LibKind, LibSet};
use base_db::SourceDatabaseExt;
use path_slash::PathExt as _;
use relative_path::RelativePath;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::{Path, PathBuf};

#[derive(Debug, Serialize, Deserialize)]
pub struct Manifest {
    pub project: Project,

    #[serde(default)]
    pub dependencies: HashMap<String, Dependency>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Project {
    pub name: String,
    pub version: String,
    pub entry: String,

    #[serde(default)]
    #[serde(with = "lib_kind")]
    pub output: LibKind,
}

#[derive(Debug, Serialize, Deserialize)]
#[serde(untagged)]
pub enum Dependency {
    Path { path: PathBuf },
}

impl Manifest {
    pub fn load(path: &Path) -> Result<Self> {
        let manifest_path = path.join("shadow.toml");
        let manifest_src = std::fs::read_to_string(&manifest_path)
            .with_context(|| format!("Failed to read manifest from {}", manifest_path.display()))?;

        toml::from_str(&manifest_src).map_err(Into::into)
    }

    pub fn dep_dirs<'a>(&'a self, proj_dir: &'a Path) -> impl Iterator<Item = PathBuf> + 'a {
        self.dependencies.values().map(move |d| d.get_dir(proj_dir))
    }
}

impl Dependency {
    pub fn get_dir(&self, proj_dir: &Path) -> PathBuf {
        match self {
            | Dependency::Path { path } => proj_dir.join(path),
        }
    }
}

const EXTENSION: &'static str = "shade";

pub fn load_project(
    rdb: &mut RootDatabase,
    libs: &mut LibSet,
    roots: &mut u32,
    files: &mut u32,
    path: &Path,
) -> Result<LibId> {
    let mut root = if *roots == 0 {
        SourceRoot::new_local()
    } else {
        SourceRoot::new_library()
    };

    let manifest = Manifest::load(path)?;
    let root_id = SourceRootId(*roots);
    let root_file = FileId(*files);
    let (lib, exists) = libs.add_lib(
        manifest.project.name.clone(),
        manifest.project.output,
        root_id,
        root_file,
    );

    if exists {
        return Ok(lib);
    }

    *roots += 1;

    load_file(
        rdb,
        &mut root,
        root_id,
        root_file,
        lib,
        files,
        path.join(&manifest.project.entry).parent().unwrap(),
        &path.join(&manifest.project.entry),
        true,
    )?;

    for dep in manifest.dep_dirs(path) {
        let dep = load_project(rdb, libs, roots, files, &dep)?;

        libs.add_dep(lib, dep)?;
    }

    rdb.set_source_root(root_id, root.into());

    Ok(lib)
}

fn load_file(
    rdb: &mut RootDatabase,
    root: &mut SourceRoot,
    root_id: SourceRootId,
    file_id: FileId,
    lib: LibId,
    files: &mut u32,
    project: &Path,
    path: &Path,
    _root: bool,
) -> Result<()> {
    let text =
        std::fs::read_to_string(path).with_context(|| format!("Failed to load source file from {}", path.display()))?;
    let file_path = path.strip_prefix(project).ok().and_then(|p| p.to_slash()).unwrap();
    let file_path = RelativePath::from_path(&file_path).unwrap();

    rdb.set_file_text(file_id, text.into());
    rdb.set_file_source_root(file_id, root_id);
    rdb.set_file_lib(file_id, lib);
    root.insert_file(file_id, file_path);
    *files += 1;

    let dir = if _root {
        project.to_path_buf()
    } else {
        file_as_dir(path)
    };

    if let Ok(read_dir) = dir.read_dir() {
        let ext = std::ffi::OsStr::new(EXTENSION);

        for entry in read_dir {
            let child_path = entry?.path();

            if _root && child_path == path {
                continue;
            }

            if child_path.is_file() && child_path.extension() == Some(ext) {
                let file_id = FileId(*files);

                load_file(rdb, root, root_id, file_id, lib, files, project, &child_path, false)?;
            }
        }
    }

    Ok(())
}

fn file_as_dir(path: &Path) -> PathBuf {
    let file_stem = path.file_stem().unwrap();
    let dir = path.parent().unwrap();

    dir.join(file_stem)
}

mod lib_kind {
    use base_db::libs::LibKind;
    use serde::de::{Deserialize, Deserializer, Error, Unexpected};
    use serde::ser::{Serialize, Serializer};

    pub fn serialize<S: Serializer>(lib_kind: &LibKind, serializer: S) -> Result<S::Ok, S::Error> {
        let s = match lib_kind {
            | LibKind::Dynamic => "dynamic",
            | LibKind::Static => "static",
            | LibKind::Executable => "executable",
        };

        s.serialize(serializer)
    }

    pub fn deserialize<'de, D: Deserializer<'de>>(deserializer: D) -> Result<LibKind, D::Error> {
        let s = <&str>::deserialize(deserializer)?;

        match s {
            | "dynamic" => Ok(LibKind::Dynamic),
            | "static" => Ok(LibKind::Static),
            | "executable" => Ok(LibKind::Executable),
            | _ => Err(Error::invalid_value(
                Unexpected::Str(s),
                &"dynamic, static or executable",
            )),
        }
    }
}
