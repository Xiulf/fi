pub mod db;
pub mod diagnostics;
pub mod manifest;

use base_db::input::{FileId, SourceRoot, SourceRootId};
use base_db::libs::{LibId, LibKind, LibSet};
use base_db::SourceDatabase;
use base_db::SourceDatabaseExt;
use codegen::db::CodegenDatabase;
use mir::db::MirDatabase;
use rustc_hash::FxHashSet;
use std::path::PathBuf;
use std::sync::Arc;

#[derive(Default)]
pub struct Opts<'a> {
    pub input: &'a str,
    pub target: Option<&'a str>,
    pub output: Option<LibKind>,
}

#[derive(Default)]
pub struct Driver {
    pub db: db::RootDatabase,
    target_dir: PathBuf,
    libs: LibSet,
    lib_count: u32,
    file_count: u32,
}

impl Driver {
    pub fn init(opts: Opts) -> Option<(Self, LibId)> {
        let mut driver = Driver::default();
        let lib = driver.load(opts.input)?;

        driver.target_dir = PathBuf::from(opts.input).join("target");
        driver.db.set_target_triple(match opts.target {
            | Some(target) => Arc::new(target.parse().unwrap()),
            | None => Arc::new(mir::target_lexicon::HOST),
        });

        Some((driver, lib))
    }

    pub fn init_no_manifest(opts: Opts) -> Option<(Self, LibId)> {
        let mut driver = Driver::default();
        let path = std::path::Path::new(opts.input);
        let lib = manifest::load_normal(
            &mut driver.db,
            &mut driver.libs,
            &mut driver.lib_count,
            &mut driver.file_count,
            path,
            opts.output.unwrap_or(LibKind::Executable),
        )
        .ok()?;

        driver.target_dir = path.parent().unwrap().join("target");
        driver.db.set_target_triple(match opts.target {
            | Some(target) => Arc::new(target.parse().unwrap()),
            | None => Arc::new(mir::target_lexicon::HOST),
        });

        driver.db.set_libs(driver.libs.clone().into());

        Some((driver, lib))
    }

    pub fn interactive() -> (Self, LibId, FileId) {
        let mut driver = Driver::default();
        let mut root = SourceRoot::new_local();
        let root_id = SourceRootId(0);
        let root_file = FileId(0);
        let (lib, _) = driver
            .libs
            .add_lib("<interactive>", Default::default(), root_id, root_file);

        root.insert_file(root_file, "<interactive>");

        driver.db.set_target_triple(mir::target_lexicon::HOST.into());
        driver.db.set_libs(driver.libs.clone().into());
        driver.db.set_source_root(root_id, root.into());
        driver.db.set_file_source_root(root_file, root_id);
        driver
            .db
            .set_file_text(root_file, String::from("module INTERACTIVE").into());
        driver.db.set_file_lib(root_file, lib);
        driver.lib_count = 1;
        driver.file_count = 1;

        (driver, lib, root_file)
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

    pub fn build(&self) -> bool {
        let start = std::time::Instant::now();
        let db = &self.db;
        let mut errors = 0;

        for lib in hir::Lib::all(db) {
            println!("  \x1B[1;32m\x1B[1mCompiling\x1B[0m {}", lib.name(db));

            if let Ok(e) = diagnostics::emit_diagnostics(db, lib, &mut std::io::stderr()) {
                errors += e;
            }
        }

        if errors == 1 {
            eprintln!("\x1B[1;31mAborting due to previous error\x1B[0m");
            return false;
        } else if errors > 1 {
            eprintln!("\x1B[1;31mAborting due to {} previous errors\x1B[0m", errors);
            return false;
        } else {
            std::fs::create_dir_all(&self.target_dir).unwrap();

            let mut done = FxHashSet::default();

            for lib in hir::Lib::all(db) {
                self.write_assembly(lib, &mut done).unwrap();
            }
        }

        let elapsed = start.elapsed();

        println!("   \x1B[1;32m\x1B[1mFinished\x1B[0m in {:?}", elapsed);

        true
    }

    pub fn run<'a>(&self, lib: LibId, args: impl Iterator<Item = &'a std::ffi::OsStr>) -> bool {
        if self.build() {
            let asm = self.db.lib_assembly(lib.into());
            let path = asm.path(&self.db, &self.target_dir);
            let mut cmd = std::process::Command::new(path);

            cmd.args(args);
            println!("    \x1B[1;32m\x1B[1mRunning\x1B[0m {:?}", cmd);
            cmd.status().unwrap().success()
        } else {
            false
        }
    }

    pub fn docs(&self, lib: LibId) {
        docs::generate(&self.db, lib.into(), &self.target_dir).unwrap();
    }

    fn write_assembly(&self, lib: hir::Lib, done: &mut FxHashSet<hir::Lib>) -> std::io::Result<bool> {
        if done.contains(&lib) {
            return Ok(false);
        }

        let deps = lib.dependencies(&self.db).into_iter().map(|dep| {
            let _ = self.write_assembly(dep.lib, done);
            dep.lib
        });

        let asm = self.db.lib_assembly(lib);

        asm.link(&self.db, deps, &self.target_dir);
        done.insert(lib);

        Ok(true)
    }
}
