use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::{Path, PathBuf};
pub use target_lexicon::Triple;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Manifest {
    pub package: Package,
    #[serde(default)]
    pub dependencies: HashMap<String, Dependency>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Package {
    pub name: String,
    pub version: String,
    pub authors: Option<Vec<String>>,
    pub src_dir: Option<PathBuf>,
    #[serde(skip)]
    pub target_dir: PathBuf,
    #[serde(default = "Triple::host")]
    #[serde(deserialize_with = "deser_triple")]
    #[serde(serialize_with = "ser_triple")]
    pub target: Triple,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
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

impl Manifest {
    pub fn load(diags: &dyn diagnostics::Diagnostics, files: &mut crate::Files, dir: &Path) -> Self {
        let manifest_path = format!("{}/shadow.toml", dir.display());
        let manifest_src = match std::fs::read_to_string(&manifest_path) {
            | Ok(s) => s,
            | Err(_) => {
                diags.error(format!("no shadow.toml file found in {}", dir.display())).finish();
                diags.print_and_exit();
            },
        };

        let manifest_file = files.add(manifest_path, manifest_src.into());
        let mut manifest: Manifest = match toml::from_str(&files.source(manifest_file)) {
            | Ok(m) => m,
            | Err(e) => {
                let span = if let Some((line, col)) = e.line_col() {
                    let span = files.line_span(manifest_file, line as u32).unwrap();
                    let index = span.start() + codespan::ByteOffset::from(col as i64);

                    codespan::Span::new(index, index)
                } else {
                    codespan::Span::default()
                };

                diags
                    .error("invalid shadow.toml file")
                    .with_label(diagnostics::Label::primary(manifest_file, span).with_message(e.to_string()))
                    .finish();

                diags.print_and_exit();
            },
        };

        match &mut manifest.package.src_dir {
            | Some(entry) => {
                if entry.is_relative() {
                    *entry = format!("{}/{}", dir.display(), entry.display()).into();
                }
            },
            | None => {
                let path = format!("{}/src", dir.display());

                manifest.package.src_dir = Some(path.into());
            },
        }

        manifest.package.target_dir = format!("{}/target", dir.display()).into();
        manifest
    }
}
