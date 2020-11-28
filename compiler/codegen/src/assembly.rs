use crate::CodegenDatabase;
use std::path::Path;
use std::sync::Arc;
use tempfile::NamedTempFile;

#[derive(Debug)]
pub struct Assembly {
    file: NamedTempFile,
}

impl PartialEq for Assembly {
    fn eq(&self, other: &Self) -> bool {
        self.path().eq(other.path())
    }
}

impl Eq for Assembly {}

impl Assembly {
    pub const EXTENSION: &'static str = "shadelib";

    pub fn path(&self) -> &Path {
        self.file.path()
    }

    pub fn copy_to<P: AsRef<Path>>(&self, dest: P) -> std::io::Result<()> {
        std::fs::copy(self.path(), dest).map(|_| ())
    }
}

pub fn build_assembly(
    db: &dyn CodegenDatabase,
    lib: source::LibId,
    module: mir::ir::ModuleId,
) -> Arc<Assembly> {
    let file = NamedTempFile::new().expect("could not create temp file");

    Arc::new(Assembly { file })
}
