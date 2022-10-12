use std::fs;
use std::path::PathBuf;

use anyhow::{Context, Result};
use base_db::libs::LibKind;
use cfg::{CfgOptions, CfgValue};
use paths::{AbsPath, AbsPathBuf};
use rustc_hash::FxHashMap;
use serde::{Deserialize, Serialize};
pub use toml::Value as TomlValue;
use vfs::{FileId, VfsPath, VirtualFileSystem};

use crate::{Package, Workspace};

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct Manifest {
    pub project: Project,

    #[serde(default)]
    pub dependencies: FxHashMap<String, Dependency>,
}

pub type Cfg = FxHashMap<String, toml::Value>;

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
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

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[serde(untagged)]
pub enum Dependency {
    Path {
        path: PathBuf,

        #[serde(default)]
        cfg: Cfg,
    },
}

impl Manifest {
    pub const FILE_NAME: &'static str = "shadow.toml";

    pub fn parse(text: &str) -> Result<Self> {
        toml::from_str(text).map_err(Into::into)
    }

    pub fn dep_dirs<'a>(&'a self, proj_dir: &'a AbsPath) -> impl Iterator<Item = AbsPathBuf> + 'a {
        self.dependencies.values().map(move |d| d.get_dir(proj_dir))
    }

    pub fn dep_cfg_opts<'a>(&'a self) -> impl Iterator<Item = &'a Cfg> + 'a {
        self.dependencies.values().map(|d| d.get_cfg())
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
    pub fn get_dir(&self, proj_dir: &AbsPath) -> AbsPathBuf {
        match self {
            | Dependency::Path { path, .. } => proj_dir.join(path),
        }
    }

    pub fn get_cfg(&self) -> &Cfg {
        match self {
            | Dependency::Path { cfg, .. } => cfg,
        }
    }
}

pub(crate) fn load_project(
    workspace: &mut Workspace,
    vfs: &mut VirtualFileSystem,
    cfg: &CfgOptions,
    path: &AbsPath,
) -> Result<Package> {
    let manifest_file = load_file(vfs, path.join(Manifest::FILE_NAME))?;
    let manifest_text = String::from_utf8_lossy(vfs.file_content(manifest_file).unwrap());
    let manifest = Manifest::parse(&manifest_text).with_context(|| "failed to parse manifest")?;

    load_manifest(workspace, vfs, &manifest, cfg, path)
}

fn load_manifest(
    workspace: &mut Workspace,
    vfs: &mut VirtualFileSystem,
    manifest: &Manifest,
    cfg: &CfgOptions,
    path: &AbsPath,
) -> Result<Package> {
    if let Some(package) = workspace.package_for_name(&manifest.project.name) {
        return Ok(package);
    }

    let src_dir = path.join(&manifest.project.src);
    let first_file = load_dir(vfs, &src_dir)?.ok_or_else(|| anyhow::anyhow!("empty source folder"))?;
    let package = workspace.alloc_package(manifest, path.join(Manifest::FILE_NAME), first_file);

    for (dep, cfg_opts) in manifest.dep_dirs(path).zip(manifest.dep_cfg_opts()) {
        let dep = load_project(workspace, vfs, cfg, &dep)?;
        let cfg_opts = parse_cfg(cfg_opts).ok_or(anyhow::anyhow!("invalid cfg in manifest"))?;

        workspace.add_dependency(package, crate::Dependency { package: dep, cfg_opts });
    }

    Ok(package)
}

fn load_dir(vfs: &mut VirtualFileSystem, path: &AbsPath) -> Result<Option<FileId>> {
    let mut first_file = None;

    for entry in path
        .as_ref()
        .read_dir()
        .with_context(|| format!("failed to read directory: {}", path.display()))?
    {
        let entry = entry?;
        let meta = entry.metadata()?;
        let path = AbsPathBuf::try_from(entry.path()).unwrap();

        if meta.is_file() {
            if path.extension().and_then(|o| o.to_str()) == Some("shade") {
                first_file.get_or_insert(load_file(vfs, path)?);
            }
        } else {
            load_dir(vfs, &path)?;
        }
    }

    Ok(first_file)
}

pub(crate) fn load_file(vfs: &mut VirtualFileSystem, path: AbsPathBuf) -> Result<FileId> {
    let text = fs::read_to_string(&path).with_context(|| anyhow::anyhow!("failed to load file: {}", path.display()))?;
    let (file, _) = vfs.set_file_content(
        VfsPath::from(path.normalize()),
        Some(text.into_bytes().into_boxed_slice()),
    );

    Ok(file)
}

pub fn parse_cfg(cfg: &Cfg) -> Option<CfgOptions> {
    let mut opts = CfgOptions::default();

    for (key, value) in cfg {
        match value {
            | TomlValue::Boolean(true) => opts.enable(key),
            | TomlValue::Integer(i) => opts.set(key, CfgValue::Int(*i as i128)),
            | TomlValue::String(s) => opts.set(key, CfgValue::String(s.into())),
            | _ => return None,
        }
    }

    Some(opts)
}

mod lib_kind {
    use base_db::libs::LibKind;
    use serde::de::{Deserialize, Deserializer, Error, Unexpected};
    use serde::ser::{Serialize, Serializer};

    pub fn serialize<S: Serializer>(lib_kind: &LibKind, serializer: S) -> Result<S::Ok, S::Error> {
        let s = match lib_kind {
            | LibKind::Executable => "executable",
            | LibKind::Static => "static",
            | LibKind::Dynamic => "dynamic",
        };

        s.serialize(serializer)
    }

    pub fn deserialize<'de, D: Deserializer<'de>>(deserializer: D) -> Result<LibKind, D::Error> {
        let s = <&str>::deserialize(deserializer)?;

        match s {
            | "executable" => Ok(LibKind::Executable),
            | "static" => Ok(LibKind::Static),
            | "dynamic" => Ok(LibKind::Dynamic),
            | _ => Err(Error::invalid_value(
                Unexpected::Str(s),
                &"dynamic, static or executable",
            )),
        }
    }
}
