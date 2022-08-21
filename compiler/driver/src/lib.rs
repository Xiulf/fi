pub mod db;
pub mod diagnostics;
pub mod manifest;

use std::{fs, io};

use base_db::input::{FileId, SourceRoot, SourceRootId};
use base_db::libs::{LibId, LibKind, LibSet};
use base_db::{SourceDatabase, SourceDatabaseExt};
use codegen::assembly::Assembly;
use codegen::db::CodegenDatabase;
use codegen::CompilerTarget;
use rustc_hash::FxHashSet;

#[derive(Default)]
pub struct Opts<'a> {
    pub input: &'a str,
    pub target: Option<&'a str>,
    pub output: Option<LibKind>,
}

#[derive(Default)]
pub struct Driver {
    pub db: db::RootDatabase,
    lib: LibId,
    libs: LibSet,
    lib_count: u32,
    file_count: u32,
}

impl Driver {
    pub fn init(opts: Opts) -> Option<(Self, LibId)> {
        let mut driver = Driver::default();
        let lib = driver.load(opts.input)?;

        driver.lib = lib;
        driver.db.set_target(match opts.target {
            | Some("javascript") => CompilerTarget::Javascript,
            // | Some(target) => Arc::new(target.parse().unwrap()),
            // | None => Arc::new(target_lexicon::Triple::host()),
            | _ => CompilerTarget::Javascript,
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

        driver.lib = lib;
        driver.db.set_target_dir(lib, path.parent().unwrap().join("target"));
        driver.db.set_target(match opts.target {
            | Some("javascript") => CompilerTarget::Javascript,
            // | Some(target) => Arc::new(target.parse().unwrap()),
            // | None => Arc::new(target_lexicon::Triple::host()),
            | _ => CompilerTarget::Javascript,
        });

        driver.db.set_libs(driver.libs.clone().into());

        Some((driver, lib))
    }

    pub fn interactive() -> (Self, LibId, FileId) {
        let mut driver = Driver::default();
        let mut root = SourceRoot::new_local(None);
        let root_id = SourceRootId(0);
        let root_file = FileId(0);
        let (lib, _) = driver
            .libs
            .add_lib("<interactive>", Default::default(), root_id, Vec::new());

        root.insert_file(root_file, "<interactive>");

        driver.lib = lib;
        driver.db.set_target(CompilerTarget::Javascript);
        driver.db.set_libs(driver.libs.clone().into());
        driver.db.set_source_root(root_id, root.into());
        driver.db.set_lib_source_root(lib, root_id);
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
        let path = std::path::Path::new(input);

        match manifest::load_project(
            &mut self.db,
            &mut self.libs,
            &mut self.lib_count,
            &mut self.file_count,
            path,
        ) {
            | Ok(lib) => {
                for lib in self.libs.iter() {
                    let source_root = self.db.lib_source_root(lib);
                    let source_root = self.db.source_root(source_root);

                    if let Some(dir) = &source_root.dir {
                        let target_dir = dir.join("target");

                        self.db.set_target_dir(lib, target_dir.clone());
                    }
                }

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

    pub fn check(&self) -> io::Result<bool> {
        let start = std::time::Instant::now();
        let db = &self.db;
        let mut last_changed = false;

        for lib in hir::Lib::all(db) {
            let id = lib.into();
            let target_dir = db.target_dir(id);
            let changed = metadata::read_metadata(db, id, &target_dir)
                .map(|m| m.has_changed(db))
                .unwrap_or(false);

            if changed || last_changed {
                eprintln!("  \x1B[1;32m\x1B[1mChecking\x1B[0m {}", lib.name(db));

                if diagnostics::emit_diagnostics(db, lib, &mut io::stderr())? > 0 {
                    return Ok(false);
                }

                metadata::write_metadata(db, id, &target_dir)?;
                last_changed = true;
            }
        }

        let elapsed = start.elapsed();

        eprintln!("   \x1B[1;32m\x1B[1mFinished\x1B[0m in {:?}", elapsed);
        Ok(true)
    }

    pub fn build(&self) -> io::Result<bool> {
        fs::create_dir_all(self.db.target_dir(self.lib))?;
        let start = std::time::Instant::now();
        let db = &self.db;
        let target_dir = db.target_dir(self.lib);
        let mut done = FxHashSet::default();
        let mut last_changed = false;

        for lib in hir::Lib::all(db) {
            let id = lib.into();
            let changed = metadata::read_metadata(db, id, &target_dir)
                .map(|m| m.has_changed(db))
                .unwrap_or(false);
            let changed = changed || !Assembly::dummy(lib).path(db, &target_dir).exists();

            if changed || last_changed {
                eprintln!("  \x1B[1;32m\x1B[1mCompiling\x1B[0m {}", lib.name(db));
                let e = diagnostics::emit_diagnostics(db, lib, &mut io::stderr())?;

                if e == 1 {
                    eprintln!("\x1B[1;31mAborting due to previous error\x1B[0m");
                    return Ok(false);
                } else if e > 1 {
                    eprintln!("\x1B[1;31mAborting due to {} previous errors\x1B[0m", e);
                    return Ok(false);
                }

                self.write_assembly(lib, &mut done)?;
                metadata::write_metadata(db, id, &target_dir)?;
                last_changed = true;
            } else {
                done.insert(lib);
            }
        }

        let elapsed = start.elapsed();

        eprintln!("   \x1B[1;32m\x1B[1mFinished\x1B[0m in {:?}", elapsed);
        Ok(true)
    }

    pub fn run<'a>(&self, _lib: LibId, _args: impl Iterator<Item = &'a std::ffi::OsStr>) -> io::Result<bool> {
        if self.build()? {
            // let asm = self.db.lib_assembly(lib.into());
            // let path = asm.path(&self.db, &self.db.target_dir(lib));
            // let mut cmd = std::process::Command::new(path);

            // cmd.args(args);
            // println!("    \x1B[1;32m\x1B[1mRunning\x1B[0m {:?}", cmd);
            // cmd.status().unwrap().success()
            Ok(true)
        } else {
            Ok(false)
        }
    }

    fn write_assembly(&self, lib: hir::Lib, done: &mut FxHashSet<hir::Lib>) -> io::Result<bool> {
        if done.contains(&lib) {
            return Ok(false);
        }

        let deps = lib.dependencies(&self.db).into_iter().map(|dep| {
            let _ = self.write_assembly(dep.lib, done);
            dep.lib
        });

        let asm = self.db.lib_assembly(lib);
        let source_root = self.db.lib_source_root(lib.into());
        let source_root = self.db.source_root(source_root);
        let path = source_root.dir.as_ref().unwrap();

        asm.link(&self.db, deps, path, &self.db.target_dir(self.lib));
        done.insert(lib);

        Ok(true)
    }
}
