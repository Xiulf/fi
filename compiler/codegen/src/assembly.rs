use std::path::{Path, PathBuf};
use std::sync::Arc;
use tempfile::NamedTempFile;

#[derive(Debug, PartialEq, Eq)]
pub struct Assembly {
    path: PathBuf,
}

pub struct ObjectFile {
    obj_file: NamedTempFile,
}

impl Assembly {
    pub fn path(&self) -> &Path {
        &self.path
    }
}

impl ObjectFile {
    pub fn new() -> Self {
        ObjectFile {
            obj_file: NamedTempFile::new().unwrap(),
        }
    }

    pub fn write(&mut self, bytes: &[u8]) {
        use std::io::Write;
        self.obj_file.write(bytes).unwrap();
    }

    pub fn path(&self) -> &Path {
        self.obj_file.path()
    }
}

#[link(name = "codegen_cranelift")]
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
    let mir = db.module_mir(lib, module);
    // let obj_file = invoke_backend(db, lib, mir);
    let obj_file = unsafe { __codegen_backend(db, lib, mir) };
    let mut linker = linker::get_linker(&db.target(lib));
    let tree = db.module_tree(lib);
    let data = tree.data(module);
    let out_type = db.link_type(lib, module);
    let extension = linker::extension(out_type, &db.target(lib));
    let out_filename: PathBuf = format!(
        "{}/{}.{}",
        db.manifest(lib).package.target_dir.display(),
        data.name,
        extension.to_string_lossy(),
    )
    .into();

    linker.add_object(obj_file.path());

    for &dep in &data.children {
        let data = tree.get(dep);
        let asm = db.assembly(lib, data.id);

        match db.link_type(lib, data.id) {
            linker::LinkOutputType::Exe => unreachable!(),
            linker::LinkOutputType::Lib => linker.link_staticlib(asm.path()),
            linker::LinkOutputType::Dylib => linker.link_dylib(asm.path()),
        }
    }

    linker.set_output_type(out_type, &out_filename);
    linker.output_filename(&out_filename);
    linker.finalize();
    linker.cmd().status().unwrap();

    Arc::new(Assembly { path: out_filename })
}

fn invoke_backend(
    db: &dyn crate::CodegenDatabase,
    lib: source::LibId,
    mir: Arc<mir::ir::Module>,
) -> ObjectFile {
    let library = libloading::Library::new(format!(
        "target/release/libcodegen_cranelift.so",
        // env!("CARGO_TARGET_DIR")
    ))
    .unwrap();

    unsafe {
        let f: libloading::Symbol<
            fn(&dyn crate::CodegenDatabase, source::LibId, Arc<mir::ir::Module>) -> ObjectFile,
        > = library.get("__codegen_backend".as_bytes()).unwrap();

        f(db, lib, mir)
    }
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
