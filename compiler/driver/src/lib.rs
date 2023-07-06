use std::collections::HashSet;
use std::io;

use base_db::libs::LibId;
pub use opts::Options;
use paths::AbsPathBuf;
use project::Package;
use triomphe::Arc;

mod db;
mod diagnostics;
pub mod opts;

#[derive(Default)]
pub struct Driver {
    db: db::Database,
    vfs: vfs::VirtualFileSystem,
    libs: base_db::libs::LibSet,
    packages: project::Packages,
    source_roots: Vec<base_db::input::SourceRoot>,
}

impl Driver {
    pub fn new(options: Options) -> Self {
        Self {
            db: db::Database::new(options),
            vfs: Default::default(),
            libs: Default::default(),
            packages: Default::default(),
            source_roots: Default::default(),
        }
    }

    pub fn load_project(&mut self, path: AbsPathBuf) -> anyhow::Result<Package> {
        self.packages.load_project(&mut self.vfs, &mut self.db, path)
    }

    pub fn load_files(
        &mut self,
        files: Vec<AbsPathBuf>,
        name: String,
        type_: project::manifest::ProjectType,
        dependencies: Vec<String>,
        search_dir: AbsPathBuf,
    ) -> anyhow::Result<Package> {
        self.packages.load_files(
            &mut self.vfs,
            &mut self.db,
            files,
            name,
            type_,
            dependencies,
            search_dir,
        )
    }

    pub fn finish_loading(&mut self) -> anyhow::Result<()> {
        self.source_roots = self.packages.to_source_roots(&self.vfs, &self.db);
        self.libs = self.packages.to_lib_set(&mut self.db, &self.source_roots)?;
        Ok(())
    }

    pub fn db(&self) -> &db::Database {
        &self.db
    }

    pub fn package_for_lib(&self, lib: LibId) -> Option<Package> {
        let source_root = lib.source_root(&self.db);
        let idx = self.source_roots.iter().position(|&s| s == source_root)?;

        Some(Package::new(idx))
    }

    pub fn libs_for_package(&self, package: Package) -> Vec<LibId> {
        let db = &self.db as &dyn base_db::Db;
        let source_root = self.source_roots[package.index()];

        self.libs
            .iter()
            .filter(|lib| lib.source_root(db) == source_root)
            .collect()
    }

    pub fn build(&self, lib: LibId) -> Arc<codegen::assembly::Assembly> {
        let start = time::Instant::now();
        let asm = self.build_rec(lib, &mut HashSet::default());
        let duration = Self::print_duration(start.elapsed());

        eprintln!("   \x1B[1;32m\x1B[1mFinished\x1B[0m in {}", duration);
        asm
    }

    fn build_rec(&self, lib: LibId, done: &mut HashSet<LibId>) -> Arc<codegen::assembly::Assembly> {
        if !done.insert(lib) {
            return codegen::codegen_lib(&self.db, lib);
        }

        let deps = lib
            .deps(&self.db)
            .iter()
            .map(|&l| self.build_rec(l, done))
            .collect::<Vec<_>>();

        let pkg = self.package_for_lib(lib).unwrap();
        eprintln!(
            "  \x1B[1;32m\x1B[1mCompiling\x1B[0m {} v{} ({})",
            lib.name(&self.db),
            self.packages[pkg].version,
            self.packages[pkg].root_dir.display(),
        );

        let asm = codegen::codegen_lib(&self.db, lib);

        asm.link(&self.db, &deps);
        asm
    }

    fn print_duration(duration: time::Duration) -> String {
        if duration.whole_minutes() > 0 {
            format!(
                "{}m {}.{:0>2}s",
                duration.whole_minutes(),
                duration.whole_seconds(),
                duration.subsec_milliseconds()
            )
        } else if duration.whole_seconds() > 0 || duration.subsec_milliseconds() >= 100 {
            let m = ((duration.subsec_milliseconds() as f32) / 10.0).round() as i16;
            format!("{}.{:0>2}s", duration.whole_seconds(), m)
        } else if duration.whole_milliseconds() > 0 {
            format!("{}ms", duration.whole_milliseconds())
        } else {
            format!("{}ns", duration.whole_nanoseconds())
        }
    }

    pub fn debug(&self, lib: LibId) {
        // for &dep in lib.deps(&self.db) {
        //     self.debug(dep);
        // }

        let def_map = hir_def::def_map::query(&self.db, lib);

        // eprintln!("{:#?}", lib.debug_all(&self.db));
        tracing::debug!("\n{}", def_map.debug(&self.db));
        use hir_def::display::HirDisplay;
        let lib = hir::Lib::from(lib);

        for module in lib.modules(&self.db) {
            for item in module.items(&self.db) {
                if let hir::Item::Value(value) = item {
                    let def = mir::lower::value_mir(&self.db, value.id());
                    tracing::debug!("{}", def.display(&self.db));
                    if let Some(body) = def.body(&self.db) {
                        tracing::debug!("\n{}", body.display(&self.db));
                    }
                }
            }

            for impl_ in module.impls(&self.db) {
                for item in impl_.items(&self.db) {
                    let def = mir::lower::value_mir(&self.db, item.id());
                    tracing::debug!("{}", def.display(&self.db));
                    if let Some(body) = def.body(&self.db) {
                        tracing::debug!("\n{}", body.display(&self.db));
                    }
                }
            }
        }
    }
}
