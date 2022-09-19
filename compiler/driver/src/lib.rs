pub mod db;
pub mod diagnostics;

use std::path::Path;
use std::{fs, io};

use base_db::input::{FileId, SourceRoot, SourceRootId};
use base_db::libs::{LibId, LibKind, LibSet};
use base_db::{SourceDatabase, SourceDatabaseExt};
use cfg::CfgOptions;
use codegen::assembly::Assembly;
use codegen::db::CodegenDatabase;
use codegen::CompilerTarget;
use paths::AbsPathBuf;
use project::manifest::{self, Cfg};
use project::Workspace;
use rustc_hash::FxHashSet;

pub struct InitOpts<'a> {
    pub input: &'a Path,
    pub target: Option<&'a str>,
    pub output: Option<LibKind>,
    pub cfg: Cfg,
}

pub struct InitNoManifestOpts<'a> {
    pub files: Vec<&'a Path>,
    pub name: &'a str,
    pub target: Option<&'a str>,
    pub output: LibKind,
    pub cfg: Cfg,
    pub links: Vec<&'a Path>,
    pub dependencies: Vec<&'a Path>,
}

pub struct Driver {
    pub db: db::RootDatabase,
    workspaces: Vec<Workspace>,
    target_dir: AbsPathBuf,
    cfg: CfgOptions,
    vfs: vfs::VirtualFileSystem,
}

impl Default for InitOpts<'_> {
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
            workspaces: Vec::new(),
            target_dir: AbsPathBuf::assert("/".into()),
            cfg: CfgOptions::default(),
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
    pub fn init(opts: InitOpts) -> anyhow::Result<Self> {
        let mut driver = Driver::default();

        driver.target_dir = input_dir(opts.input).join("target");
        driver.cfg = manifest::parse_cfg(&opts.cfg).ok_or_else(|| anyhow::anyhow!("failed to parse cfg options"))?;
        driver.load(opts.input)?;

        driver.db.set_target(match opts.target {
            | Some("javascript") => CompilerTarget::Javascript,
            // | Some(target) => Arc::new(target.parse().unwrap()),
            // | None => Arc::new(target_lexicon::Triple::host()),
            | _ => CompilerTarget::Javascript,
        });

        Ok(driver)
    }

    pub fn init_without_manifest(opts: InitNoManifestOpts) -> anyhow::Result<Self> {
        let mut driver = Driver::default();
        let current_dir = AbsPathBuf::assert(std::env::current_dir().unwrap());

        driver.target_dir = current_dir.join("target");
        driver.cfg = manifest::parse_cfg(&opts.cfg).ok_or_else(|| anyhow::anyhow!("failed to parse cfg options"))?;

        let files = opts.files.into_iter().map(|p| input_dir(p)).collect();
        let links = opts.links.into_iter().map(|l| l.to_path_buf()).collect();
        let dependencies = opts.dependencies.into_iter().map(|d| input_dir(d)).collect();

        driver.workspaces.push(Workspace::local_files(
            &mut driver.vfs,
            current_dir,
            files,
            opts.name.to_string(),
            opts.output,
            links,
            dependencies,
        )?);

        driver.set_libs();
        driver.set_source_roots();

        driver.db.set_target(match opts.target {
            | Some("javascript") => CompilerTarget::Javascript,
            // | Some(target) => Arc::new(target.parse().unwrap()),
            // | None => Arc::new(target_lexicon::Triple::host()),
            | _ => CompilerTarget::Javascript,
        });

        Ok(driver)
    }

    pub fn interactive() -> (Self, LibId, FileId) {
        // let mut driver = Driver::default();
        // let mut libs = LibSet::default();
        // let (root_file, _) = driver
        //     .vfs
        //     .set_file_content(VfsPath::new_virtual("/<interactive>".into()), None);
        // let lib = libs.add_lib("<interactive>", Default::default(), Vec::new(), Default::default());

        // libs.set_root_file(lib, root_file);
        // driver.lib = lib;
        // driver.db.set_target(CompilerTarget::Javascript);
        // driver.db.set_libs(libs.into());
        // driver
        //     .db
        //     .set_file_text(root_file, String::from("module INTERACTIVE").into());

        // (driver, lib, root_file)
        todo!()
    }

    pub fn load(&mut self, input: &Path) -> anyhow::Result<()> {
        let input = input_dir(input);

        self.workspaces.push(Workspace::load(input, &mut self.vfs)?);
        self.set_libs();
        self.set_source_roots();

        Ok(())
    }

    fn set_libs(&mut self) {
        let mut libs = LibSet::default();

        for ws in &self.workspaces {
            let set = ws.to_libs(&self.cfg);

            libs.extend(set);
        }

        self.db.set_libs(libs.into());
    }

    fn set_source_roots(&mut self) {
        let file_sets = Workspace::file_sets(&self.workspaces, &self.vfs);

        for (idx, file_set) in file_sets.into_iter().enumerate() {
            let root_id = SourceRootId(idx as u32);
            let root = SourceRoot::new(file_set);

            for file_id in root.iter() {
                if let Some(bytes) = self.vfs.file_content(file_id) {
                    let text = String::from_utf8_lossy(bytes);

                    self.db.set_file_text(file_id, text.into());
                }

                self.db.set_file_source_root(file_id, root_id);
            }

            self.db.set_source_root(root_id, root.into());
        }
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

    pub fn run(&self, _args: impl Iterator<Item = impl AsRef<std::ffi::OsStr>>) -> io::Result<bool> {
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
