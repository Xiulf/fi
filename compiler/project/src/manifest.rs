use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::str::FromStr;

use anyhow::Context;
use paths::AbsPathBuf;
use semver::{Version, VersionReq};
use serde::{Deserialize, Serialize};
use vfs::{Db, File, VirtualFileSystem};

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Manifest {
    pub project: Project,
    #[serde(default)]
    pub dependencies: HashMap<String, Dependency>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Project {
    pub name: String,
    pub version: Version,
    #[serde(rename = "type", default)]
    pub type_: ProjectType,
    #[serde(rename = "src-dir", default = "Manifest::default_src_dir")]
    pub src_dir: PathBuf,
}

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum ProjectType {
    #[serde(rename = "exe", alias = "executable")]
    Executable,
    #[default]
    #[serde(rename = "dylib", alias = "dynamiclib")]
    DynamicLib,
    #[serde(rename = "lib", alias = "staticlib")]
    StaticLib,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(untagged)]
pub enum Dependency {
    Normal { version: VersionReq },
    Local { path: PathBuf },
}

impl Manifest {
    pub const FILE_NAME: &'static str = "fi.toml";

    pub(crate) fn default_src_dir() -> PathBuf {
        PathBuf::from("src")
    }

    pub fn load(path: impl AsRef<Path>) -> anyhow::Result<Self> {
        let text = std::fs::read_to_string(path.as_ref()).with_context(|| path.as_ref().display().to_string())?;
        toml::from_str(&text).map_err(Into::into)
    }
}

impl Into<base_db::libs::LibKind> for ProjectType {
    fn into(self) -> base_db::libs::LibKind {
        match self {
            | Self::Executable => base_db::libs::LibKind::Executable,
            | Self::DynamicLib => base_db::libs::LibKind::DynamicLib,
            | Self::StaticLib => base_db::libs::LibKind::StaticLib,
        }
    }
}

pub(crate) fn load_files(vfs: &mut VirtualFileSystem, db: &mut dyn Db, path: AbsPathBuf) -> anyhow::Result<()> {
    if path.as_path().as_ref().is_file()
        && path.extension().and_then(|p| p.to_str()) == Some(File::SOURCE_FILE_EXTENSION)
    {
        tracing::trace!("load {}", path.display());
        let content = std::fs::read_to_string(&path).with_context(|| path.display().to_string())?;
        let content = content.into_boxed_str();
        let path = path.into();

        vfs.set_file_content(db, path, Some(content));
        Ok(())
    } else {
        for entry in std::fs::read_dir(path)? {
            let path = AbsPathBuf::assert(entry?.path());
            load_files(vfs, db, path)?;
        }

        Ok(())
    }
}

pub(crate) fn parse_dependency(dep: &str) -> anyhow::Result<(&str, Dependency)> {
    let (base, mut cfg) = dep.split_once('[').unwrap_or((dep, ""));
    if !cfg.is_empty() {
        cfg = &cfg[..cfg.len()];
    }
    let _ = cfg;

    if Path::new(base).exists() {
        Ok(("", Dependency::Local {
            path: PathBuf::from(base),
        }))
    } else {
        let (s, version) = base.split_once('@').unwrap_or((base, ""));
        let version = if version.is_empty() {
            VersionReq::default()
        } else {
            VersionReq::from_str(version)?
        };

        Ok((s, Dependency::Normal { version }))
    }
}
