use crate::db::CodegenDatabase;
use base_db::libs::LibKind;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use tempfile::NamedTempFile;

#[derive(Debug)]
pub struct Assembly {
    lib: hir::Lib,
    file: NamedTempFile,
}

impl PartialEq for Assembly {
    fn eq(&self, other: &Self) -> bool {
        self.tmp_path().eq(other.tmp_path())
    }
}

impl Eq for Assembly {
}

impl Assembly {
    pub fn tmp_path(&self) -> &Path {
        self.file.path()
    }

    pub fn path(&self, db: &dyn CodegenDatabase, target_dir: &Path) -> PathBuf {
        target_dir
            .join(format!("{}{}", self.prefix(db), self.lib.name(db.upcast())))
            .with_extension(self.extension(db))
    }

    pub fn link(&self, db: &dyn CodegenDatabase, deps: impl Iterator<Item = hir::Lib>, target_dir: &Path) {
        let mut linker = crate::linker::create();
        let out = self.path(db, target_dir);

        linker.add_object(self.tmp_path());
        linker.rpath(target_dir);
        linker.arg("-L");
        linker.arg(target_dir);

        add_deps(&mut *linker, db.upcast(), deps.collect());

        fn add_deps(linker: &mut dyn crate::linker::Linker, db: &dyn hir::db::HirDatabase, deps: Vec<hir::Lib>) {
            for dep in deps {
                add_deps(linker, db, dep.dependencies(db).into_iter().map(|d| d.lib).collect());

                let name = dep.name(db).to_string();

                match db.libs()[dep.into()].kind {
                    | LibKind::Dynamic => linker.add_shared_object(&name),
                    | LibKind::Static => linker.add_static_lib(&name),
                    | LibKind::Executable => panic!("cannot link with an executable"),
                }
            }
        }

        match db.libs()[self.lib.into()].kind {
            | LibKind::Dynamic => linker.build_shared_object(&out),
            | LibKind::Static => linker.build_static_lib(&out),
            | LibKind::Executable => linker.build_executable(&out),
        }

        linker.run();
    }

    fn extension(&self, db: &dyn CodegenDatabase) -> &'static str {
        match db.libs()[self.lib.into()].kind {
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

    fn prefix(&self, db: &dyn CodegenDatabase) -> &'static str {
        match db.libs()[self.lib.into()].kind {
            | LibKind::Dynamic | LibKind::Static => match db.target_triple().operating_system {
                | target_lexicon::OperatingSystem::Wasi => "",
                | _ => "lib",
            },
            | LibKind::Executable => "",
        }
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

    Arc::new(Assembly { lib, file })
}
