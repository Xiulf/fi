pub mod db;
pub mod diagnostics;

use std::path::Path;
use std::process::{Command, ExitCode};
use std::{fs, io};

use base_db::input::{FileId, SourceRoot, SourceRootId};
use base_db::libs::{LibId, LibKind, LibSet};
use base_db::target::CompilerTarget;
pub use base_db::target::Optimization;
use base_db::{SourceDatabase, SourceDatabaseExt};
use cfg::{CfgOptions, CfgValue};
use codegen::assembly::Assembly;
use codegen::db::CodegenDatabase;
use paths::AbsPathBuf;
use project::manifest::{self, Cfg};
use project::Workspace;
use rustc_hash::FxHashSet;

pub struct InitOpts<'a> {
    pub input: &'a Path,
    pub target: Option<&'a str>,
    pub output: Option<LibKind>,
    pub optimization: Optimization,
    pub cfg: Cfg,
}

pub struct InitNoManifestOpts<'a> {
    pub files: Vec<&'a Path>,
    pub name: &'a str,
    pub target: Option<&'a str>,
    pub output: LibKind,
    pub optimization: Optimization,
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
            optimization: Optimization::None,
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
    pub fn init(opts: InitOpts) -> anyhow::Result<(Self, usize)> {
        let mut driver = Driver::default();
        let cfg = manifest::parse_cfg(&opts.cfg).ok_or_else(|| anyhow::anyhow!("failed to parse cfg options"))?;
        let target = opts.target.map(|t| t.parse()).unwrap_or(Ok(Default::default()))?;

        driver.init_cfg(&target);
        driver.target_dir = input_dir(opts.input).join("target");
        driver.cfg = driver.cfg.merge(&cfg);
        driver.db.set_target(target);

        let ws = driver.load(opts.input)?;

        Ok((driver, ws))
    }

    pub fn init_without_manifest(opts: InitNoManifestOpts) -> anyhow::Result<(Self, usize)> {
        let mut driver = Driver::default();
        let current_dir = AbsPathBuf::assert(std::env::current_dir().unwrap());
        let cfg = manifest::parse_cfg(&opts.cfg).ok_or_else(|| anyhow::anyhow!("failed to parse cfg options"))?;
        let target = opts.target.map(|t| t.parse()).unwrap_or(Ok(Default::default()))?;

        driver.init_cfg(&target);
        driver.target_dir = current_dir.join("target");
        driver.cfg = driver.cfg.merge(&cfg);

        let files = opts.files.into_iter().map(|p| input_dir(p)).collect();
        let links = opts.links.into_iter().map(|l| l.to_path_buf()).collect();
        let dependencies = opts.dependencies.into_iter().map(|d| input_dir(d)).collect();

        driver.workspaces.push(Workspace::local_files(
            &mut driver.vfs,
            &driver.cfg,
            current_dir,
            files,
            opts.name.to_string(),
            opts.output,
            links,
            dependencies,
        )?);

        driver.set_libs();
        driver.set_source_roots();
        driver.db.set_target(target);

        let ws = driver.workspaces.len() - 1;

        Ok((driver, ws))
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

    pub fn load(&mut self, input: &Path) -> anyhow::Result<usize> {
        let input = input_dir(input);

        self.workspaces.push(Workspace::load(input, &mut self.vfs, &self.cfg)?);
        self.set_libs();
        self.set_source_roots();

        Ok(self.workspaces.len() - 1)
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

    fn init_cfg(&mut self, target: &CompilerTarget) {
        self.cfg.set("target", match target {
            | CompilerTarget::Javascript => CfgValue::String("javascript".into()),
            | CompilerTarget::Native(triple) => CfgValue::String(triple.operating_system.to_string().into()),
        });
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

    pub fn build(&self, ws: usize) -> io::Result<bool> {
        let db = &self.db;
        fs::create_dir_all(&self.target_dir)?;
        let start = std::time::Instant::now();
        let mut done = FxHashSet::default();
        let mut last_changed = false;
        let ws = &self.workspaces[ws];
        let libs = db.libs();

        for lib in hir::Lib::all(db) {
            if ws.find_file_package(libs[lib.into()].root_file).is_none() {
                continue;
            }

            let id = lib.into();
            let changed = metadata::read_metadata(db, id, &self.target_dir)
                .map(|m| m.has_changed(db))
                .unwrap_or(true);
            let changed = changed || fs::metadata(Assembly::dummy(lib).path(db, self.target_dir.as_ref())).is_err();

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

                self.write_assembly(ws, lib, &mut done)?;
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

    pub fn run(&self, ws: usize, args: impl Iterator<Item = impl AsRef<std::ffi::OsStr>>) -> io::Result<ExitCode> {
        if self.build(ws)? {
            let ws = &self.workspaces[ws];
            let libs = self.db.libs();
            let lib = hir::Lib::all(&self.db)
                .into_iter()
                .filter(|&lib| ws.find_file_package(libs[lib.into()].root_file).is_some())
                .find(|&lib| libs[lib.into()].kind == LibKind::Executable)
                .unwrap();

            let asm = Assembly::dummy(lib);
            let path = asm.path(&self.db, &self.target_dir);

            let mut cmd = if self.db.target() == CompilerTarget::Javascript {
                if !program_exists("node") {
                    base_db::Error::throw("node must be insalled to run a javascript targeted program");
                }

                let mut cmd = Command::new("node");

                cmd.arg(path.as_path().as_ref());
                cmd
            } else {
                Command::new(path.as_path().as_ref())
            };

            cmd.args(args);

            print!(
                "    \x1B[1;32m\x1B[1mRunning\x1B[0m `{}",
                cmd.get_program().to_string_lossy()
            );

            for arg in cmd.get_args() {
                print!(" {}", arg.to_string_lossy());
            }

            println!("`");

            cmd.status()
                .map(|s| s.code().map(|c| ExitCode::from(c as u8)).unwrap_or(ExitCode::FAILURE))
        } else {
            Ok(ExitCode::FAILURE)
        }
    }

    fn write_assembly(&self, ws: &Workspace, lib: hir::Lib, done: &mut FxHashSet<hir::Lib>) -> io::Result<bool> {
        if done.contains(&lib) {
            return Ok(false);
        }

        for dep in lib.dependencies(&self.db) {
            self.write_assembly(ws, dep.lib, done)?;
        }

        let asm = self.db.lib_assembly(lib);

        asm.link(&self.db, ws, &self.target_dir);
        done.insert(lib);

        Ok(true)
    }
}

fn program_exists(program: impl AsRef<std::ffi::OsStr>) -> bool {
    match Command::new(program).spawn() {
        | Ok(mut c) => {
            let _ = c.kill();
            true
        },
        | Err(e) if e.kind() == io::ErrorKind::NotFound => false,
        | Err(_) => true,
    }
}
