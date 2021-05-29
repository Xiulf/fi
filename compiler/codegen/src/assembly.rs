use crate::db::CodegenDatabase;
use std::io::Write;
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

impl Eq for Assembly {
}

impl Assembly {
    pub fn extension(db: &dyn CodegenDatabase) -> &'static str {
        match db.target_triple().operating_system {
            | target_lexicon::OperatingSystem::Windows => "dll",
            | target_lexicon::OperatingSystem::MacOSX { .. } => "dylib",
            | target_lexicon::OperatingSystem::Wasi => "wasm",
            | _ => "so",
        }
    }

    pub fn path(&self) -> &Path {
        self.file.path()
    }

    pub fn copy_to(&self, dest: impl AsRef<Path>) -> std::io::Result<()> {
        std::fs::copy(self.path(), dest).map(|_| ())
    }
}

pub(crate) fn build_assembly(db: &dyn CodegenDatabase, module: hir::Module) -> Arc<Assembly> {
    let object = crate::ModuleCtx::with_mcx(db, |mcx| mcx.build(module));
    let mut object_file = NamedTempFile::new().unwrap();

    object_file.write(object.emit().unwrap().as_slice()).unwrap();

    let mut linker = crate::linker::create();
    let file = NamedTempFile::new().unwrap();

    linker.add_object(object_file.path());
    linker.build_shared_object(file.path());
    linker.run();

    Arc::new(Assembly { file })
}
