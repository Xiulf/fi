pub use codegen::OutputType;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::PathBuf;
use target_lexicon::Triple;

#[derive(Debug)]
pub struct Opts {
    pub project_dir: PathBuf,
    pub target: Triple,
    pub out_type: OutputType,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Manifest {
    pub package: Package,
    #[serde(default)]
    pub dependencies: HashMap<String, Dependency>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Package {
    pub name: String,
    pub version: Option<String>,
    pub authors: Option<Vec<String>>,
    pub src_dir: Option<PathBuf>,
    #[serde(skip)]
    pub target_dir: PathBuf,
    #[serde(default = "Triple::host")]
    #[serde(deserialize_with = "deser_triple")]
    #[serde(serialize_with = "ser_triple")]
    pub target: Triple,
}

#[derive(Debug, Serialize, Deserialize)]
#[serde(untagged)]
pub enum Dependency {
    Path { path: PathBuf },
}

fn deser_triple<'de, D: serde::Deserializer<'de>>(d: D) -> Result<Triple, D::Error> {
    use std::str::FromStr;
    let string = <&'_ str>::deserialize(d)?;

    Triple::from_str(&string).map_err(serde::de::Error::custom)
}

fn ser_triple<S: serde::Serializer>(triple: &Triple, s: S) -> Result<S::Ok, S::Error> {
    triple.to_string().serialize(s)
}
