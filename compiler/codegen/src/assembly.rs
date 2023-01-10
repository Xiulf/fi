use std::path::Path;
use std::sync::Arc;

use base_db::libs::LibKind;
use base_db::target::CompilerTarget;
use paths::{AbsPath, AbsPathBuf};
use project::Workspace;
use tempfile::NamedTempFile;

use crate::db::CodegenDatabase;
use crate::linker::Linker;

#[derive(Debug)]
pub struct ObjectFile {
    path: tempfile::TempPath,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Assembly {
    lib: hir::Lib,
    objects: Vec<Arc<ObjectFile>>,
}

impl PartialEq for ObjectFile {
    fn eq(&self, other: &Self) -> bool {
        self.tmp_path().eq(other.tmp_path())
    }
}

impl Eq for ObjectFile {
}

impl ObjectFile {
    pub fn new(file: NamedTempFile) -> Self {
        let path = file.into_temp_path();
        Self { path }
    }

    #[inline(always)]
    pub fn tmp_path(&self) -> &Path {
        self.path.as_ref()
    }
}

fn is_local_link(path: &Path) -> bool {
    let mut components = path.components();

    if let Some(std::path::Component::CurDir) = components.next() {
        return true;
    }

    components.count() > 0 && path.is_relative()
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

    pub fn path(&self, db: &dyn CodegenDatabase, target_dir: &AbsPath) -> AbsPathBuf {
        target_dir
            .join(format!("{}{}", self.prefix(db), self.lib.name(db.upcast())))
            .with_extension(self.extension(db))
    }

    pub fn link(&self, db: &dyn CodegenDatabase, ws: &Workspace, target_dir: &AbsPath) -> AbsPathBuf {
        let mut linker = crate::linker::create(db.target());
        let out = self.path(db, target_dir);
        let libs = db.libs();
        let lib = &libs[self.lib.into()];
        let pkg = ws.find_file_package(lib.root_file).unwrap();
        let pkg_root = match pkg {
            | Some(pkg) => ws[pkg].manifest_path.parent().unwrap(),
            | None => ws.root_dir(),
        };

        for obj in self.objects.iter() {
            linker.add_module(obj.tmp_path());
        }

        add_exports(db, &mut *linker, self.lib);
        linker.runtime_path(target_dir.as_ref());
        linker.add_path(target_dir.as_ref());

        add_deps(
            &mut *linker,
            db.upcast(),
            lib.deps.iter().map(|&id| id.into()).collect(),
            target_dir,
        );

        fn add_deps(linker: &mut dyn Linker, db: &dyn hir::db::HirDatabase, deps: Vec<hir::Lib>, target_dir: &AbsPath) {
            for dep in deps {
                add_deps(
                    linker,
                    db,
                    dep.dependencies(db).into_iter().map(|d| d.lib).collect(),
                    target_dir,
                );

                let name = dep.name(db).to_string();
                linker.add_lib(db.libs()[dep.into()].kind, &name, target_dir.as_ref());
            }
        }

        for link in &db.libs()[self.lib.into()].links {
            let mut path = std::borrow::Cow::Borrowed(Path::new(link));

            if is_local_link(&path) {
                path = std::borrow::Cow::Owned(pkg_root.join(path).into());
            }

            linker.add_lib(
                base_db::libs::LibKind::Dynamic,
                path.to_str().unwrap(),
                target_dir.as_ref(),
            );
        }

        if db.target().is_windows() {
            linker.subsystem("console");
            linker.add_module(Path::new("ucrt.lib"));
        }

        linker.out_kind(db.libs()[self.lib.into()].kind, out.as_ref());
        linker.build(out.as_ref());

        tracing::debug!("{:?}", linker);

        if let Err(e) = linker.run() {
            base_db::Error::throw(format!("error while linking:\n{}", e));
        }

        out
    }

    fn extension(&self, db: &dyn CodegenDatabase) -> &'static str {
        match db.target() {
            | CompilerTarget::Javascript => "js",
            | CompilerTarget::Native(triple) => match db.libs()[self.lib.into()].kind {
                | LibKind::Executable => match triple.operating_system {
                    | target_lexicon::OperatingSystem::Windows => "exe",
                    | target_lexicon::OperatingSystem::Wasi => "wasm",
                    | _ => "",
                },
                | LibKind::Dynamic => match triple.operating_system {
                    | target_lexicon::OperatingSystem::Windows => "dll",
                    | target_lexicon::OperatingSystem::MacOSX { .. } => "dylib",
                    | target_lexicon::OperatingSystem::Wasi => "wasm",
                    | _ => "so",
                },
                | LibKind::Static => match triple.operating_system {
                    | target_lexicon::OperatingSystem::Windows => "lib",
                    | target_lexicon::OperatingSystem::Wasi => "wasm",
                    | _ => "a",
                },
            },
        }
    }

    fn prefix(&self, db: &dyn CodegenDatabase) -> &'static str {
        match db.target() {
            | CompilerTarget::Javascript => "",
            | CompilerTarget::Native(triple) => match db.libs()[self.lib.into()].kind {
                | LibKind::Executable => "",
                | LibKind::Dynamic | LibKind::Static => match triple.operating_system {
                    | target_lexicon::OperatingSystem::Windows => "",
                    | target_lexicon::OperatingSystem::Wasi => "",
                    | _ => "lib",
                },
            },
        }
    }
}

fn add_exports(db: &dyn CodegenDatabase, linker: &mut dyn Linker, lib: hir::Lib) {
    let db = db.upcast();

    for module in lib.modules(db) {
        for export in module.exports(db) {
            match export {
                | hir::ModuleDef::Func(it) if it.has_body(db) => {
                    // for now ignore generic function
                    if !it.is_generic(db) {
                        add_export(linker, it.link_name(db));
                    }
                },
                | hir::ModuleDef::Static(it) if it.has_body(db) => {
                    add_export(linker, it.link_name(db));
                },
                | _ => {},
            }
        }
    }
}

fn add_export(linker: &mut dyn Linker, (symbol, mangle): (hir::Name, bool)) {
    let mut symbol = symbol.to_string();

    if mangle {
        symbol = mangling::mangle(symbol.as_bytes());
    }

    linker.add_export(&symbol);
}
