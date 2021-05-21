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
    pub fn init(opts: Opts) -> Option<(Self, LibId)> {
        let mut driver = Driver::default();
        let lib = driver.load(opts.input, false)?;

        Some((driver, lib))
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
        driver.db.set_file_source_root(type_file, root_id);
        driver.db.set_file_source_root(resolve_file, root_id);
        driver
            .db
            .set_file_text(root_file, String::from("module INTERACTIVE").into());
        driver.db.set_file_text(type_file, Default::default());
        driver.db.set_file_text(resolve_file, Default::default());
        driver.db.set_file_lib(root_file, lib);
        driver.db.set_file_lib(type_file, lib);
        driver.db.set_file_lib(resolve_file, lib);
        driver.lib_count = 1;
        driver.file_count = 3;

        (driver, lib, root_file, type_file, resolve_file)
    }

    pub fn load(&mut self, input: &str, interactive: bool) -> Option<LibId> {
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

                if interactive {
                    println!("loaded {}", self.libs[lib].name);
                }

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

    pub fn check(&self) {
        let start = std::time::Instant::now();
        let db = &self.db;

        for lib in hir::Lib::all(db) {
            println!("  \x1B[1;32m\x1B[1mChecking\x1B[0m {}", lib.name(db));

            diagnostics::emit_diagnostics(db, lib, &mut std::io::stderr()).unwrap();
        }

        let elapsed = start.elapsed();

        println!("   \x1B[1;32m\x1B[1mFinished\x1B[0m in {:?}", elapsed);
    }

    pub fn build(&self) {
        let start = std::time::Instant::now();
        let db = &self.db;

        for lib in hir::Lib::all(db) {
            println!("  \x1B[1;32m\x1B[1mCompiling\x1B[0m {}", lib.name(db));

            diagnostics::emit_diagnostics(db, lib, &mut std::io::stderr()).unwrap();
        }

        let elapsed = start.elapsed();

        println!("   \x1B[1;32m\x1B[1mFinished\x1B[0m in {:?}", elapsed);
    }

    pub fn docs(&self, lib: LibId) {
        docs::generate(&self.db, lib.into(), "target".as_ref())
    }
}
