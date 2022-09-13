pub mod db;
pub mod diagnostics;

use std::path::Path;
use std::{fs, io};

use base_db::cfg::CfgOptions;
use base_db::input::{FileId, SourceRootId};
use base_db::libs::{LibId, LibKind, LibSet};
use base_db::manifest::{self, Cfg};
use base_db::{SourceDatabase, SourceDatabaseExt};
use codegen::assembly::Assembly;
use codegen::db::CodegenDatabase;
use codegen::CompilerTarget;
use paths::AbsPathBuf;
use rustc_hash::FxHashSet;
use vfs::VfsPath;

pub struct Opts<'a> {
    pub input: &'a Path,
    pub target: Option<&'a str>,
    pub output: Option<LibKind>,
    pub cfg: Cfg,
}

pub struct Driver {
    pub db: db::RootDatabase,
    target_dir: AbsPathBuf,
    cfg: CfgOptions,
    lib: LibId,
    libs: LibSet,
    vfs: vfs::VirtualFileSystem,
}

impl Default for Opts<'_> {
    fn default() -> Self {
        Self {
            input: Path::new("."),
            target: None,
            output: None,
            cfg: Cfg::default(),
        }
    }
}

impl Default for Driver {
    fn default() -> Self {
        Self {
            db: db::RootDatabase::default(),
            target_dir: AbsPathBuf::assert("".into()),
            cfg: CfgOptions::default(),
            lib: LibId::default(),
            libs: LibSet::default(),
            vfs: vfs::VirtualFileSystem::default(),
        }
    }
}

fn input_dir(input: &Path) -> AbsPathBuf {
    if input.is_absolute() {
        AbsPathBuf::assert(input.into())
    } else {
        AbsPathBuf::assert(std::env::current_dir().unwrap().join(input))
    }
}

impl Driver {
    pub fn init(opts: Opts) -> Option<(Self, LibId)> {
        let mut driver = Driver::default();
        driver.target_dir = input_dir(opts.input).join("target");
        driver.cfg = manifest::parse_cfg(&opts.cfg)?;
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
        let path = input_dir(opts.input);
        let cfg = manifest::parse_cfg(&opts.cfg)?;
        let lib = manifest::load_normal(
            &mut driver.db,
            cfg,
            &mut driver.libs,
            &path,
            opts.output.unwrap_or(LibKind::Executable),
            &mut |db, path| {
                let text = fs::read_to_string(&path)?;
                let (file, _) = driver.vfs.set_file_content(VfsPath::from(path), None);

                db.set_file_text(file, text.into());

                Ok(Some(file))
            },
        )
        .ok()?;

        driver.lib = lib;
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
        let (root_file, _) = driver
            .vfs
            .set_file_content(VfsPath::new_virtual("/<interactive>".into()), None);
        let (lib, _) = driver
            .libs
            .add_lib("<interactive>", Default::default(), Vec::new(), Default::default());

        driver.libs.set_root_file(lib, root_file);
        driver.lib = lib;
        driver.db.set_target(CompilerTarget::Javascript);
        driver.db.set_libs(driver.libs.clone().into());
        driver
            .db
            .set_file_text(root_file, String::from("module INTERACTIVE").into());
        driver.set_source_roots();

        (driver, lib, root_file)
    }

    pub fn load(&mut self, input: &Path) -> Option<LibId> {
        let input = input_dir(input);

        match manifest::load_project(&mut self.db, &self.cfg, &mut self.libs, &input, &mut |db, path| {
            let text = fs::read_to_string(&path)?;
            let (file, _) = self.vfs.set_file_content(VfsPath::from(path), None);

            db.set_file_text(file, text.into());

            Ok(Some(file))
        }) {
            | Ok(lib) => {
                self.db.set_libs(self.libs.clone().into());
                self.set_source_roots();
                Some(lib)
            },
            | Err(e) => {
                eprintln!("{}", e);
                None
            },
        }
    }

    fn set_source_roots(&mut self) {
        for (idx, root) in self.roots.clone().into_iter().enumerate() {
            let root_id = SourceRootId(idx as u32);

            self.db.set_source_root(root_id, root.into());
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
            let changed = metadata::read_metadata(db, id, &self.target_dir)
                .map(|m| m.has_changed(db))
                .unwrap_or(true);

            if changed || last_changed {
                eprintln!("  \x1B[1;32m\x1B[1mChecking\x1B[0m {}", lib.name(db));

                if diagnostics::emit_diagnostics(db, lib, &mut io::stderr())? > 0 {
                    return Ok(false);
                }

                metadata::write_metadata(db, id, &self.target_dir)?;
                last_changed = true;
            }
        }

        let elapsed = start.elapsed();

        eprintln!("   \x1B[1;32m\x1B[1mFinished\x1B[0m in {:?}", elapsed);
        Ok(true)
    }

    pub fn build(&self) -> io::Result<bool> {
        let db = &self.db;
        fs::create_dir_all(&self.target_dir)?;
        let start = std::time::Instant::now();
        let mut done = FxHashSet::default();
        let mut last_changed = false;

        for lib in hir::Lib::all(db) {
            let id = lib.into();
            let changed = metadata::read_metadata(db, id, &self.target_dir)
                .map(|m| m.has_changed(db))
                .unwrap_or(true);
            let changed = changed || !Assembly::dummy(lib).path(db, self.target_dir.as_ref()).exists();

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
                metadata::write_metadata(db, id, &self.target_dir)?;
                last_changed = true;
            } else {
                done.insert(lib);
            }
        }

        let elapsed = start.elapsed();

        eprintln!("   \x1B[1;32m\x1B[1mFinished\x1B[0m in {:?}", elapsed);
        Ok(true)
    }

    pub fn run(&self, _lib: LibId, _args: impl Iterator<Item = impl AsRef<std::ffi::OsStr>>) -> io::Result<bool> {
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
        // let source_root = self.db.libs()[lib.into()].source_root;
        // let path = &self.paths.source_roots[&source_root];

        asm.link(&self.db, deps, Path::new(""), self.target_dir.as_ref());
        done.insert(lib);

        Ok(true)
    }
}
