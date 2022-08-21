use std::path::{Path, PathBuf};
use std::sync::Arc;

// use base_db::libs::LibKind;
use tempfile::NamedTempFile;

use crate::db::CodegenDatabase;
use crate::CompilerTarget;

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

    pub fn dummy(lib: hir::Lib) -> Self {
        Self {
            lib,
            objects: Vec::new(),
        }
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
        project_dir: &Path,
        target_dir: &Path,
    ) -> PathBuf {
        let mut linker = crate::linker::create(db.target());
        let out = self.path(db, target_dir);

        linker.runtime_path(target_dir);

        add_deps(&mut *linker, db.upcast(), deps.collect());

        fn add_deps(linker: &mut dyn crate::linker::Linker, db: &dyn hir::db::HirDatabase, deps: Vec<hir::Lib>) {
            for dep in deps {
                add_deps(linker, db, dep.dependencies(db).into_iter().map(|d| d.lib).collect());

                // let dir = db.target_dir(dep.into());
                // let name = dir.join(dep.name(db).to_string());
                // let name = name.to_str().unwrap();
                let name = dep.name(db).to_string();

                linker.add_object(db.libs()[dep.into()].kind, &name);
            }
        }

        for link in &db.libs()[self.lib.into()].links {
            let mut path = std::borrow::Cow::Borrowed(Path::new(link));

            if path.is_relative() {
                path = std::borrow::Cow::Owned(project_dir.join(path));
            }

            linker.add_object(base_db::libs::LibKind::Dynamic, path.to_str().unwrap());
        }

        for obj in self.objects.iter() {
            linker.add_module(obj.tmp_path());
        }

        linker.out_kind(db.libs()[self.lib.into()].kind);
        linker.build(&out);
        linker.run().unwrap();

        out
    }

    fn extension(&self, db: &dyn CodegenDatabase) -> &'static str {
        match db.target() {
            | CompilerTarget::Javascript => "js",
        }
        // match db.libs()[self.lib.into()].kind {
        //     | LibKind::Dynamic => match db.triple().operating_system {
        //         | target_lexicon::OperatingSystem::Windows => "dll",
        //         | target_lexicon::OperatingSystem::MacOSX { .. } => "dylib",
        //         | target_lexicon::OperatingSystem::Wasi => "wasm",
        //         | _ => "so",
        //     },
        //     | LibKind::Static => match db.triple().operating_system {
        //         | target_lexicon::OperatingSystem::Windows => "lib",
        //         | target_lexicon::OperatingSystem::Wasi => "wasm",
        //         | _ => "a",
        //     },
        //     | LibKind::Executable => match db.triple().operating_system {
        //         | target_lexicon::OperatingSystem::Windows => "exe",
        //         | target_lexicon::OperatingSystem::Wasi => "wasm",
        //         | _ => "",
        //     },
        // }
    }

    fn prefix(&self, db: &dyn CodegenDatabase) -> &'static str {
        match db.target() {
            | CompilerTarget::Javascript => "",
        }
        // match db.libs()[self.lib.into()].kind {
        //     | LibKind::Dynamic | LibKind::Static => match db.triple().operating_system {
        //         | target_lexicon::OperatingSystem::Wasi => "",
        //         | _ => "lib",
        //     },
        //     | LibKind::Executable => "",
        // }
    }
}
