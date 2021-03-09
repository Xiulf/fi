pub mod db;
pub mod diagnostics;
pub mod manifest;

use base_db::input::SourceRoot;
use base_db::libs::LibSet;
use base_db::SourceDatabase;
use base_db::SourceDatabaseExt;
use hir::db::DefDatabase;
use syntax::ast::{self, AstNode, NameOwner};

pub struct Opts<'a> {
    pub input: &'a str,
}

#[derive(Default)]
pub struct Driver {
    rdb: db::RootDatabase,
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

    pub fn interactive() -> Self {
        let mut driver = Driver::default();
        let mut root = SourceRoot::new_local();
        let root_id = base_db::input::SourceRootId(0);
        let root_file = base_db::input::FileId(0);
        let (lib, _) = driver.libs.add_lib("<interactive>", root_id, root_file);

        root.insert_file(root_file, "<interactive>");

        driver.rdb.set_libs(driver.libs.clone().into());
        driver.rdb.set_source_root(root_id, root.into());
        driver.rdb.set_file_source_root(root_file, root_id);
        driver.lib_count += 1;
        driver.file_count += 1;

        driver
    }

    pub fn db(&self) -> &db::RootDatabase {
        &self.rdb
    }

    pub fn load(&mut self, input: &str) -> Option<()> {
        let path = std::path::PathBuf::from(input);

        match manifest::load_project(
            &mut self.rdb,
            &mut self.libs,
            &mut self.lib_count,
            &mut self.file_count,
            &path,
        ) {
            | Ok(lib) => {
                self.rdb.set_libs(self.libs.clone().into());
                println!("loaded {}", self.libs[lib].name);

                Some(())
            },
            | Err(e) => {
                eprintln!("{}", e);
                None
            },
        }
    }

    pub fn build(&self) {
        let start = std::time::Instant::now();

        for lib in self.rdb.libs().toposort() {
            let lib_data = &self.rdb.libs()[lib];

            println!("  \x1B[1;32m\x1B[1mCompiling\x1B[0m {}", lib_data.name);

            diagnostics::emit_diagnostics(&self.rdb, lib, &mut std::io::stderr());

            // let def_map = rdb.def_map(lib);
            //
            // def_map.dump(&mut std::io::stdout()).unwrap();
        }

        let elapsed = start.elapsed();

        println!("   \x1B[1;32m\x1B[1mFinished\x1B[0m in {:?}", elapsed);
    }
}
