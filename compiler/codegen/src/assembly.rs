use crate::db::CodegenDatabase;
use base_db::libs::LibKind;
use std::{
    path::{Path, PathBuf},
    sync::Arc,
};
use tempfile::NamedTempFile;

#[derive(Debug)]
pub struct ObjectFile {
    file: NamedTempFile,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Assembly {
    lib: hir::Lib,
    objects: Vec<Arc<ObjectFile>>,
}

impl PartialEq for ObjectFile {
    fn eq(&self, other: &Self) -> bool {
        self.file.path().eq(other.file.path())
    }
}

impl Eq for ObjectFile {
}

impl ObjectFile {
    pub fn new(file: NamedTempFile) -> Self {
        Self { file }
    }

    pub fn tmp_path(&self) -> &Path {
        self.file.path()
    }
}

impl Assembly {
    pub fn new(lib: hir::Lib, objects: Vec<Arc<ObjectFile>>) -> Self {
        Self { lib, objects }
    }

    pub fn path(&self, db: &dyn CodegenDatabase, target_dir: &Path) -> PathBuf {
        target_dir
            .join(format!("{}{}", self.prefix(db), self.lib.name(db.upcast())))
            .with_extension(self.extension(db))
    }

    pub fn link<'a>(
        &self,
        db: &dyn CodegenDatabase,
        deps: impl Iterator<Item = hir::Lib>,
        target_dir: &Path,
    ) -> PathBuf {
        let mut linker = crate::linker::create();
        let out = self.path(db, target_dir);

        for obj in self.objects.iter() {
            linker.add_object(obj.tmp_path());
        }

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
        out
    }

    fn extension(&self, db: &dyn CodegenDatabase) -> &'static str {
        match db.libs()[self.lib.into()].kind {
            | LibKind::Dynamic => match db.triple().operating_system {
                | target_lexicon::OperatingSystem::Windows => "dll",
                | target_lexicon::OperatingSystem::MacOSX { .. } => "dylib",
                | target_lexicon::OperatingSystem::Wasi => "wasm",
                | _ => "so",
            },
            | LibKind::Static => match db.triple().operating_system {
                | target_lexicon::OperatingSystem::Windows => "lib",
                | target_lexicon::OperatingSystem::Wasi => "wasm",
                | _ => "a",
            },
            | LibKind::Executable => match db.triple().operating_system {
                | target_lexicon::OperatingSystem::Windows => "exe",
                | target_lexicon::OperatingSystem::Wasi => "wasm",
                | _ => "",
            },
        }
    }

    fn prefix(&self, db: &dyn CodegenDatabase) -> &'static str {
        match db.libs()[self.lib.into()].kind {
            | LibKind::Dynamic | LibKind::Static => match db.triple().operating_system {
                | target_lexicon::OperatingSystem::Wasi => "",
                | _ => "lib",
            },
            | LibKind::Executable => "",
        }
    }
}
