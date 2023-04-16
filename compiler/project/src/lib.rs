use std::collections::HashMap;
use std::path::PathBuf;

use semver::{Version, VersionReq};
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Manfiest {
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
    #[serde(rename = "src-dir", default = "default_src_dir")]
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

fn default_src_dir() -> PathBuf {
    PathBuf::from("src")
}
