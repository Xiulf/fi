use std::fs::read_to_string;
use std::path::{Path, PathBuf};

use anyhow::{Context, Result};
use base_db::input::{FileId, SourceRoot, SourceRootId};
use base_db::libs::{LibId, LibKind, LibSet};
use base_db::SourceDatabaseExt;
use path_slash::PathExt as _;
use relative_path::RelativePath;
use rustc_hash::FxHashMap;
use serde::{Deserialize, Serialize};
pub use toml::Value as TomlValue;

use crate::db::RootDatabase;

#[derive(Debug, Serialize, Deserialize)]
pub struct Manifest {
    pub project: Project,

    #[serde(default)]
    pub cfg: Cfg,

    #[serde(default)]
    pub dependencies: FxHashMap<String, Dependency>,
}

pub type Cfg = FxHashMap<String, toml::Value>;

#[derive(Debug, Serialize, Deserialize)]
pub struct Project {
    pub name: String,
    pub version: String,

    #[serde(default = "Project::default_src")]
    pub src: PathBuf,

    #[serde(default)]
    pub link: Vec<PathBuf>,

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

impl Project {
    pub fn link_with<'a>(&'a self) -> impl Iterator<Item = &'a PathBuf> {
        self.link.iter()
    }

    fn default_src() -> PathBuf {
        PathBuf::from("src")
    }
}

impl Dependency {
    pub fn get_dir(&self, proj_dir: &Path) -> PathBuf {
        match self {
            | Dependency::Path { path } => proj_dir.join(path),
        }
    }
}

pub fn load_project(
    rdb: &mut RootDatabase,
    libs: &mut LibSet,
    roots: &mut u32,
    files: &mut u32,
    path: &Path,
) -> Result<LibId> {
    let mut root = if *roots == 0 {
        SourceRoot::new_local(Some(path.to_path_buf()))
    } else {
        SourceRoot::new_library(Some(path.to_path_buf()))
    };

    let manifest = Manifest::load(path)?;
    let root_id = SourceRootId(*roots);
    let (lib, exists) = libs.add_lib(
        manifest.project.name.clone(),
        manifest.project.output,
        root_id,
        manifest.project.link_with().map(|l| l.display().to_string()).collect(),
    );

    if exists {
        return Ok(lib);
    }

    *roots += 1;

    let src_dir = path.join(&manifest.project.src);

    load_dir(rdb, &mut root, root_id, lib, files, path, &src_dir)?;

    for dep in manifest.dep_dirs(path) {
        let dep = load_project(rdb, libs, roots, files, &dep)?;

        libs.add_dep(lib, dep)?;
    }

    rdb.set_lib_source_root(lib, root_id);
    rdb.set_source_root(root_id, root.into());

    Ok(lib)
}

pub fn load_normal(
    rdb: &mut RootDatabase,
    libs: &mut LibSet,
    roots: &mut u32,
    files: &mut u32,
    path: &Path,
    kind: LibKind,
) -> Result<LibId> {
    let name = path.file_stem().unwrap().to_str().unwrap();
    let mut root = SourceRoot::new_local(Some(path.to_path_buf()));
    let root_id = SourceRootId(*roots);
    let (lib, _) = libs.add_lib(name, kind, root_id, Vec::new());

    *roots += 1;

    load_dir(rdb, &mut root, root_id, lib, files, path, path)?;

    rdb.set_lib_source_root(lib, root_id);
    rdb.set_source_root(root_id, root.into());

    Ok(lib)
}

fn load_dir(
    rdb: &mut RootDatabase,
    root: &mut SourceRoot,
    root_id: SourceRootId,
    lib: LibId,
    files: &mut u32,
    project: &Path,
    path: &Path,
) -> Result<()> {
    for entry in path.read_dir()? {
        let entry = entry?;
        let meta = entry.metadata()?;

        if meta.is_file() {
            load_file(rdb, root, root_id, lib, files, project, &entry.path())?;
        } else {
            load_dir(rdb, root, root_id, lib, files, project, &entry.path())?;
        }
    }

    Ok(())
}

fn load_file(
    rdb: &mut RootDatabase,
    root: &mut SourceRoot,
    root_id: SourceRootId,
    lib: LibId,
    files: &mut u32,
    project: &Path,
    path: &Path,
) -> Result<()> {
    let text = read_to_string(path).with_context(|| format!("Failed to load source file from {}", path.display()))?;
    let file_path = path.strip_prefix(project).ok().and_then(|p| p.to_slash()).unwrap();
    let file_path = RelativePath::from_path(&file_path).unwrap();
    let file_id = FileId(*files);

    rdb.set_file_text(file_id, text.into());
    rdb.set_file_source_root(file_id, root_id);
    rdb.set_file_lib(file_id, lib);
    root.insert_file(file_id, file_path);
    *files += 1;

    Ok(())
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
