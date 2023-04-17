use std::io;

use base_db::libs::LibId;
pub use opts::Options;
use paths::AbsPathBuf;
use project::Package;

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
        root_dir: AbsPathBuf,
    ) -> anyhow::Result<Package> {
        self.packages
            .load_files(&mut self.vfs, &mut self.db, files, name, type_, dependencies, root_dir)
    }

    pub fn finish_loading(&mut self) {
        self.source_roots = self.packages.to_source_roots(&self.vfs, &self.db);
        self.libs = self.packages.to_lib_set(&self.db, &self.source_roots);
    }

    pub fn libs_for_package(&self, package: Package) -> Vec<LibId> {
        let db = &self.db as &dyn base_db::Db;
        let source_root = self.source_roots[package.index()];

        self.libs
            .iter()
            .filter(|lib| lib.source_root(db) == source_root)
            .collect()
    }

    // pub fn load_file(&mut self, path: impl AsRef<Path>) -> io::Result<vfs::File> {
    //     let path = paths::AbsPathBuf::assert(path.as_ref().canonicalize()?);
    //     let content = std::fs::read_to_string(&path)?;
    //     let (file, _) = self
    //         .vfs
    //         .set_file_content(&mut self.db, path.into(), Some(content.into_boxed_str()));

    //     Ok(file)
    // }

    // pub fn load_files(&mut self, files: Vec<PathBuf>) -> io::Result<vfs::FileSet> {
    //     let mut set = vfs::FileSet::default();

    //     for path in files {
    //         let file = self.load_file(&path)?;
    //         let path = file.path(&self.db).clone();
    //         set.insert(file, path);
    //     }

    //     Ok(set)
    // }

    // pub fn create_lib(&mut self, file_set: vfs::FileSet, name: &str) -> LibId {
    //     let source_root = SourceRoot::new(&self.db, file_set);

    //     self.libs.add_lib(&self.db, name, LibKind::Executable, source_root)
    // }

    pub fn build(&self, lib: LibId) {
        let asm = codegen::codegen_lib(&self.db, lib);
        asm.link(&self.db);
    }

    pub fn debug(&self, lib: LibId) {
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
