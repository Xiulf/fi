use std::path::Path;
use std::sync::Arc;
use tempfile::NamedTempFile;

#[derive(Debug)]
pub struct Assembly {
    file: NamedTempFile,
}

pub struct ObjectFile {
    target: target_lexicon::Triple,
    obj_file: NamedTempFile,
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

impl ObjectFile {
    pub fn into_shared_object(self, out_path: &Path) -> Result<(), anyhow::Error> {
        unimplemented!();
    }
}

extern "Rust" {
    fn __codegen_backend(
        db: &dyn crate::CodegenDatabase,
        lib: source::LibId,
        mir: Arc<mir::ir::Module>,
    ) -> ObjectFile;
}

pub fn build_assembly(
    db: &dyn crate::CodegenDatabase,
    lib: source::LibId,
    module: mir::ir::ModuleId,
) -> Arc<Assembly> {
    let file = NamedTempFile::new().unwrap();
    let mir = db.module_mir(lib, module);
    let obj_file = unsafe { __codegen_backend(db, lib, mir) };

    obj_file.into_shared_object(file.path()).unwrap();

    Arc::new(Assembly { file })
}

#[macro_export]
macro_rules! define_codegen_backend {
    ($backend:ident) => {
        #[no_mangle]
        pub fn __codegen_backend(
            db: &dyn $crate::CodegenDatabase,
            lib: ::source::LibId,
            mir: ::std::sync::Arc<::mir::ir::Module>,
        ) -> $crate::assembly::ObjectFile {
            let backend = $backend::new();
            let mcx = $crate::ModuleCtx::new(db, lib, mir, backend);

            mcx.build()
        }
    };
}
