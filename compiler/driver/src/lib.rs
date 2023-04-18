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

    pub fn libs_for_package(&self, package: Package) -> Vec<LibId> {
        let db = &self.db as &dyn base_db::Db;
        let source_root = self.source_roots[package.index()];

        self.libs
            .iter()
            .filter(|lib| lib.source_root(db) == source_root)
            .collect()
    }

    pub fn build(&self, lib: LibId) -> Arc<codegen::assembly::Assembly> {
        let deps = lib.deps(&self.db).iter().map(|&l| self.build(l)).collect::<Vec<_>>();
        let asm = codegen::codegen_lib(&self.db, lib);

        asm.link(&self.db, &deps);
        asm
    }

    pub fn debug(&self, lib: LibId) {
        for &dep in lib.deps(&self.db) {
            self.debug(dep);
        }

        // let def_map = hir_def::def_map::query(&self.db, lib);

        // eprintln!("{:#?}", lib.debug_all(&self.db));
        // tracing::debug!("\n{}", def_map.debug(&self.db));
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
