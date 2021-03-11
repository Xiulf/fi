pub mod db;
pub mod diagnostics;
pub mod manifest;

use base_db::input::{FileId, SourceRoot, SourceRootId};
use base_db::libs::{LibId, LibSet};
use base_db::SourceDatabase;
use base_db::SourceDatabaseExt;
use hir::db::DefDatabase;
use syntax::ast::{self, AstNode, NameOwner};

pub struct Opts<'a> {
    pub input: &'a str,
}

#[derive(Default)]
pub struct Driver {
    pub db: db::RootDatabase,
    libs: LibSet,
    lib_count: u32,
    file_count: u32,
}

impl Driver {
    pub fn init(opts: Opts) -> Option<Self> {
        let mut driver = Driver::default();

        driver.load(opts.input)?;

        Some(driver)
    }

    pub fn interactive() -> (Self, LibId, FileId, FileId, FileId) {
        let mut driver = Driver::default();
        let mut root = SourceRoot::new_local();
        let root_id = SourceRootId(0);
        let root_file = FileId(0);
        let type_file = FileId(1);
        let resolve_file = FileId(2);
        let (lib, _) = driver.libs.add_lib("<interactive>", root_id, root_file);

        root.insert_file(root_file, "<interactive>");
        root.insert_file(type_file, "<type>");
        root.insert_file(resolve_file, "<resolve>");

        driver.db.set_libs(driver.libs.clone().into());
        driver.db.set_source_root(root_id, root.into());
        driver.db.set_file_source_root(root_file, root_id);
        driver.db.set_file_text(root_file, Default::default());
        driver.lib_count = 1;
        driver.file_count = 3;

        (driver, lib, root_file, type_file, resolve_file)
    }

    pub fn load(&mut self, input: &str) -> Option<LibId> {
        let path = std::path::PathBuf::from(input);

        match manifest::load_project(
            &mut self.db,
            &mut self.libs,
            &mut self.lib_count,
            &mut self.file_count,
            &path,
        ) {
            | Ok(lib) => {
                self.db.set_libs(self.libs.clone().into());
                println!("loaded {}", self.libs[lib].name);

                Some(lib)
            },
            | Err(e) => {
                eprintln!("{}", e);
                None
            },
        }
    }

    pub fn add_dep(&mut self, lib: LibId, dep: LibId) {
        self.libs.add_dep(lib, dep).unwrap();
        self.db.set_libs(self.libs.clone().into());
    }

    pub fn build(&self) {
        let start = std::time::Instant::now();

        for lib in self.db.libs().toposort() {
            let lib_data = &self.db.libs()[lib];

            println!("  \x1B[1;32m\x1B[1mCompiling\x1B[0m {}", lib_data.name);

            diagnostics::emit_diagnostics(&self.db, lib, &mut std::io::stderr());

            // let def_map = rdb.def_map(lib);
            //
            // def_map.dump(&mut std::io::stdout()).unwrap();
        }

        let elapsed = start.elapsed();

        println!("   \x1B[1;32m\x1B[1mFinished\x1B[0m in {:?}", elapsed);
    }
}
