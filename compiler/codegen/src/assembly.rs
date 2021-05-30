use crate::db::CodegenDatabase;
use base_db::libs::LibKind;
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
    pub fn extension(db: &dyn CodegenDatabase, lib: hir::Lib) -> &'static str {
        match db.libs()[lib.into()].kind {
            | LibKind::Dynamic => match db.target_triple().operating_system {
                | target_lexicon::OperatingSystem::Windows => "dll",
                | target_lexicon::OperatingSystem::MacOSX { .. } => "dylib",
                | target_lexicon::OperatingSystem::Wasi => "wasm",
                | _ => "so",
            },
            | LibKind::Static => match db.target_triple().operating_system {
                | target_lexicon::OperatingSystem::Windows => "lib",
                | target_lexicon::OperatingSystem::Wasi => "wasm",
                | _ => "a",
            },
            | LibKind::Executable => match db.target_triple().operating_system {
                | target_lexicon::OperatingSystem::Windows => "exe",
                | target_lexicon::OperatingSystem::Wasi => "wasm",
                | _ => "",
            },
        }
    }

    pub fn path(&self) -> &Path {
        self.file.path()
    }

    pub fn copy_to(&self, dest: impl AsRef<Path>) -> std::io::Result<()> {
        std::fs::copy(self.path(), dest).map(|_| ())
    }
}

pub(crate) fn build_assembly(db: &dyn CodegenDatabase, lib: hir::Lib) -> Arc<Assembly> {
    let object = crate::ModuleCtx::with_mcx(db, |mcx| mcx.build(lib));
    let mut file = NamedTempFile::new().unwrap();

    file.write(object.emit().unwrap().as_slice()).unwrap();

    // let mut linker = crate::linker::create();
    // let file = NamedTempFile::new().unwrap();
    //
    // linker.args(&["-rpath", "."]);
    // linker.add_object(object_file.path());
    //
    // for dep in lib.dependencies(db.upcast()) {
    //     let _ = db.lib_assembly(dep.lib);
    //     let name = dep.lib.name(db.upcast()).to_string();
    //
    //     match db.libs()[dep.lib.into()].kind {
    //         | LibKind::Dynamic => linker.add_shared_object(&name),
    //         | LibKind::Static => linker.add_static_lib(&name),
    //         | LibKind::Executable => panic!("cannot link with an executable"),
    //     }
    // }
    //
    // match db.libs()[lib.into()].kind {
    //     | LibKind::Dynamic => linker.build_shared_object(file.path()),
    //     | LibKind::Static => linker.build_static_lib(file.path()),
    //     | LibKind::Executable => linker.build_executable(file.path()),
    // }
    //
    // eprintln!("{:?}", linker.cmd());
    //
    // linker.run();

    Arc::new(Assembly { file })
}
