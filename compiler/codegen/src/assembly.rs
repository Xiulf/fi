use std::path::Path;

use base_db::libs::LibKind;
use base_db::Error;
use triomphe::Arc;

use crate::linker::Linker;
use crate::Db;

#[derive(Debug, PartialEq, Eq)]
pub struct Assembly {
    lib: hir::id::LibId,
    objects: Vec<Arc<ObjectFile>>,
}

#[derive(Debug)]
pub struct ObjectFile {
    path: tempfile::TempPath,
}

impl Assembly {
    pub fn new(lib: hir::id::LibId, objects: Vec<Arc<ObjectFile>>) -> Self {
        Self { lib, objects }
    }

    pub fn path(&self, db: &dyn Db) -> std::path::PathBuf {
        let prefix = match self.lib.kind(db) {
            | LibKind::Executable => "",
            | _ => db.target().dll_prefix,
        };

        db.target_dir()
            .join(format!("{}{}", prefix, self.lib.name(db)))
            .with_extension(self.extension(db))
    }

    pub fn link(&self, db: &dyn Db, deps: &[Arc<Assembly>]) -> std::path::PathBuf {
        let mut linker = crate::linker::create(db.target());
        let out = self.path(db);

        for obj in self.objects.iter() {
            linker.add_module(obj.path());
        }

        add_exports(db, &mut *linker, self.lib.into());
        linker.runtime_path(db.target_dir());
        linker.add_path(db.target_dir());

        for dep in deps {
            let kind = dep.lib.kind(db);
            let name = dep.lib.name(db);
            let path = dep.path(db);

            linker.add_lib(kind, name, &path);
        }

        if db.target().is_windows() {
            linker.subsystem("console");
            linker.add_module(Path::new("kernel32.lib"));
            linker.add_module(Path::new("libvcruntime.lib"));
            linker.add_module(Path::new("msvcrt.lib"));
            linker.add_module(Path::new("ucrt.lib"));
        }

        linker.out_kind(self.lib.kind(db), &out);
        linker.build(&out);

        tracing::debug!("{:?}", linker);

        if let Err(e) = linker.run() {
            Error::throw(format!("error while linking:\n{}", e));
        }

        out
    }

    fn extension(&self, db: &dyn Db) -> &'static str {
        let kind = self.lib.kind(db);
        match db.target().triple.operating_system {
            | target_lexicon::OperatingSystem::Windows => match kind {
                | LibKind::Executable => "exe",
                | LibKind::DynamicLib => "dll",
                | LibKind::StaticLib => "lib",
            },
            | target_lexicon::OperatingSystem::MacOSX { .. } => match kind {
                | LibKind::Executable => "",
                | LibKind::DynamicLib => "dylib",
                | LibKind::StaticLib => "a",
            },
            | target_lexicon::OperatingSystem::Wasi => "wasm",
            | _ => match kind {
                | LibKind::Executable => "",
                | LibKind::DynamicLib => "so",
                | LibKind::StaticLib => "a",
            },
        }
    }
}

impl ObjectFile {
    pub fn new(path: tempfile::TempPath) -> Self {
        Self { path }
    }

    #[inline]
    pub fn path(&self) -> &std::path::Path {
        self.path.as_ref()
    }
}

impl PartialEq for ObjectFile {
    fn eq(&self, other: &Self) -> bool {
        self.path() == other.path()
    }
}

impl Eq for ObjectFile {
}

fn add_exports(db: &dyn Db, linker: &mut dyn Linker, lib: hir::Lib) {
    for module in lib.modules(db) {
        for export in module.exports(db) {
            match export {
                | hir::Item::Value(it) if it.has_body(db) => {
                    if it.type_vars(db).is_empty() {
                        linker.add_export(&it.link_name(db));
                    }
                },
                | _ => {},
            }
        }
    }
}
