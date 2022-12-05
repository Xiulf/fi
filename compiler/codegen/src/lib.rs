pub mod assembly;
pub mod backend;
pub mod db;
pub mod linker;

use std::str::FromStr;
use std::sync::Arc;

use target_lexicon::Triple;
use tempfile::NamedTempFile;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CompilerTarget {
    Javascript,
    Native(Triple),
}

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
pub enum Optimization {
    #[default]
    None,
}

pub(crate) fn build_assembly(db: &dyn db::CodegenDatabase, lib: hir::Lib) -> Arc<assembly::Assembly> {
    let mut objects = Vec::new();

    for module in lib.modules(db.upcast()) {
        if module.is_virtual(db.upcast()) {
            continue;
        }

        objects.push(db.codegen_module(module));
    }

    Arc::new(assembly::Assembly::new(lib, objects))
}

pub(crate) fn codegen_module(db: &dyn db::CodegenDatabase, module: hir::Module) -> Arc<assembly::ObjectFile> {
    let mut file = NamedTempFile::new().unwrap();
    let backend = db.backend();

    backend.invoke(db, module, &mut file);

    Arc::new(assembly::ObjectFile::new(file))
}

impl Default for CompilerTarget {
    fn default() -> Self {
        Self::Native(Triple::host())
    }
}

impl FromStr for CompilerTarget {
    type Err = target_lexicon::ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            | "js" | "javascript" => Ok(Self::Javascript),
            | _ => Triple::from_str(s).map(Self::Native),
        }
    }
}
