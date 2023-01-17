use std::collections::HashSet;
use std::io;
use std::path::{Path, PathBuf};
use std::process::Command;

use base_db::libs::LibKind;

use crate::CompilerTarget;

pub fn create(target: CompilerTarget) -> Box<dyn Linker> {
    match target {
        | CompilerTarget::Javascript => Box::new(JsLinker::new()),
        | CompilerTarget::Native(triple) => {
            match triple.operating_system {
                | target_lexicon::OperatingSystem::Windows => create_linker_windows(&triple),
                | target_lexicon::OperatingSystem::MacOSX { .. } => todo!(),
                | target_lexicon::OperatingSystem::Ios => todo!(),
                | target_lexicon::OperatingSystem::Wasi => todo!(),
                // | _ => Box::new(ElfLinker::new()),
                | _ => Box::new(CcLinker::new()),
            }
        },
    }
}

fn create_linker_windows(triple: &target_lexicon::Triple) -> Box<dyn Linker> {
    let link_exe = cc::windows_registry::find_tool(&triple.to_string(), "link.exe");

    match link_exe {
        | Some(msvc) => Box::new(MsvcLinker::new(msvc.to_command())),
        | None => todo!(),
    }
}

#[derive(Debug)]
pub enum LinkError {
    Io(io::Error),
    Linker(String),
}

impl std::fmt::Display for LinkError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            | Self::Linker(e) => write!(f, "{}", e),
            | Self::Io(e) => write!(f, "{}", e),
        }
    }
}

impl std::error::Error for LinkError {
}

impl From<io::Error> for LinkError {
    fn from(value: io::Error) -> Self {
        Self::Io(value)
    }
}

pub trait Linker: std::fmt::Debug {
    fn runtime_path(&mut self, rpath: &Path);
    fn add_path(&mut self, path: &Path);
    fn add_module(&mut self, path: &Path);
    fn add_lib(&mut self, kind: LibKind, lib: &str, path: &Path);
    fn add_export(&mut self, symbol: &str);
    fn subsystem(&mut self, subsystem: &str);
    fn out_kind(&mut self, kind: LibKind, path: &Path);
    fn build(&mut self, out: &Path);
    fn run(&mut self) -> Result<(), LinkError>;
}

#[derive(Debug)]
pub struct ElfLinker {
    args: Vec<String>,
}

impl ElfLinker {
    pub fn new() -> Self {
        Self { args: Vec::new() }
    }

    fn arg(&mut self, arg: impl AsRef<std::ffi::OsStr>) -> &mut Self {
        self.args.push(arg.as_ref().to_str().unwrap().to_string());
        self
    }
}

impl Linker for ElfLinker {
    fn runtime_path(&mut self, rpath: &Path) {
        self.arg("--rpath");
        self.arg(rpath);
    }

    fn add_path(&mut self, path: &Path) {
        self.arg("-L");
        self.arg(path);
    }

    fn add_module(&mut self, path: &Path) {
        self.arg(path);
    }

    fn add_lib(&mut self, kind: LibKind, lib: &str, _: &Path) {
        match kind {
            | LibKind::Dynamic => self.arg("--Bdynamic"),
            | LibKind::Static => self.arg("--Bstatic"),
            | LibKind::Executable => panic!("linking with executable"),
        };

        self.arg("-l");
        self.arg(lib);
    }

    fn add_export(&mut self, _: &str) {
    }

    fn subsystem(&mut self, _: &str) {
    }

    fn out_kind(&mut self, kind: LibKind, _: &Path) {
        match kind {
            | LibKind::Dynamic => self.arg("--shared"),
            | LibKind::Static => self.arg("--static"),
            | LibKind::Executable => self.arg("--no-pie"),
        };
    }

    fn build(&mut self, out: &Path) {
        self.arg("--output");
        self.arg(out);
    }

    fn run(&mut self) -> Result<(), LinkError> {
        todo!();
        // lld_rs::link(lld_rs::LldFlavor::Elf, &self.args)
        //     .ok()
        //     .map_err(LinkError::Lld)
    }
}

#[derive(Debug)]
pub struct CcLinker {
    cmd: Command,
}

impl CcLinker {
    pub fn new() -> Self {
        Self {
            cmd: Command::new("cc"),
        }
    }

    fn arg(&mut self, arg: impl AsRef<std::ffi::OsStr>) -> &mut Self {
        self.cmd.arg(arg);
        self
    }
}

impl Linker for CcLinker {
    fn runtime_path(&mut self, rpath: &Path) {
        self.arg("-Wl,-rpath");
        self.arg(rpath);
    }

    fn add_path(&mut self, path: &Path) {
        self.arg("-L");
        self.arg(path);
    }

    fn add_module(&mut self, path: &Path) {
        self.arg(path);
    }

    fn add_lib(&mut self, kind: LibKind, lib: &str, _: &Path) {
        match kind {
            | LibKind::Dynamic => self.arg("-Bdynamic"),
            | LibKind::Static => self.arg("-Bstatic"),
            | LibKind::Executable => panic!("linking with executable"),
        };

        self.arg("-l");
        self.arg(lib);
    }

    fn add_export(&mut self, _: &str) {
    }

    fn subsystem(&mut self, _: &str) {
    }

    fn out_kind(&mut self, kind: LibKind, _: &Path) {
        match kind {
            | LibKind::Dynamic => self.arg("-shared"),
            | LibKind::Static => self.arg("-static"),
            | LibKind::Executable => self.arg("-no-pie"),
        };
    }

    fn build(&mut self, out: &Path) {
        self.arg("-o");
        self.arg(out);
    }

    fn run(&mut self) -> Result<(), LinkError> {
        let output = self.cmd.output()?;

        if !output.status.success() {
            return Err(LinkError::Linker(String::from_utf8(output.stderr).unwrap()));
        }

        Ok(())
    }
}

#[derive(Debug)]
pub struct MsvcLinker {
    cmd: Command,
}

impl MsvcLinker {
    pub fn new(cmd: Command) -> Self {
        Self { cmd }
    }

    fn arg(&mut self, arg: impl AsRef<std::ffi::OsStr>) -> &mut Self {
        self.cmd.arg(arg);
        self
    }
}

impl Linker for MsvcLinker {
    fn runtime_path(&mut self, _rpath: &Path) {
        // self.arg("--rpath");
        // self.arg(rpath);
    }

    fn add_path(&mut self, path: &Path) {
        self.arg(format!("/LIBPATH:{}", path.display()));
    }

    fn add_module(&mut self, path: &Path) {
        self.arg(path);
    }

    fn add_lib(&mut self, kind: LibKind, lib: &str, path: &Path) {
        match kind {
            | LibKind::Static => {
                self.arg(lib);
            },
            | LibKind::Dynamic => {
                let name = format!("{lib}.dll.lib");
                if path.join(&name).exists() {
                    self.arg(name);
                }
            },
            | LibKind::Executable => panic!("linking with executable"),
        }
    }

    fn add_export(&mut self, symbol: &str) {
        self.arg(format!("/EXPORT:{symbol}"));
    }

    fn subsystem(&mut self, subsystem: &str) {
        self.arg(format!("/SUBSYSTEM:{subsystem}"));

        if subsystem == "windows" {
            self.arg("/ENTRY:mainCRTStartup");
        }
    }

    fn out_kind(&mut self, kind: LibKind, out: &Path) {
        match kind {
            | LibKind::Dynamic => {
                self.arg("/DLL");
                self.arg("/NOENTRY");
                self.arg(format!("/IMPLIB:{}", out.with_extension("dll.lib").display()));
            },
            | LibKind::Static => {
                self.arg("/NOENTRY");
            },
            | LibKind::Executable => {
                self.arg("/ENTRY:main");
            },
        };
    }

    fn build(&mut self, out: &Path) {
        self.arg(format!("/OUT:{}", out.display()));
    }

    fn run(&mut self) -> Result<(), LinkError> {
        let output = self.cmd.output()?;

        if !output.status.success() {
            return Err(LinkError::Linker(String::from_utf8(output.stdout).unwrap()));
        }

        Ok(())
    }
}

#[derive(Debug)]
pub struct JsLinker {
    rpath: PathBuf,
    files: Vec<PathBuf>,
    out_file: PathBuf,
    emit_main: bool,
}

impl JsLinker {
    pub fn new() -> Self {
        Self {
            rpath: PathBuf::new(),
            files: Vec::new(),
            out_file: PathBuf::new(),
            emit_main: false,
        }
    }

    fn add_file(&mut self, path: impl AsRef<Path>) {
        let abs = path.as_ref().canonicalize().unwrap();

        if !self.files.contains(&abs) {
            self.files.push(abs);
        }
    }
}

impl Linker for JsLinker {
    fn runtime_path(&mut self, rpath: &Path) {
        self.rpath = rpath.to_path_buf();
    }

    fn add_path(&mut self, _path: &Path) {
    }

    fn add_module(&mut self, path: &Path) {
        self.add_file(path);
    }

    fn add_lib(&mut self, _kind: LibKind, lib: &str, _: &Path) {
        let mut path = Path::new(lib).to_path_buf().with_extension("js");

        if !path.exists() {
            path = self.rpath.join(path);
        }

        self.add_file(path);
    }

    fn add_export(&mut self, _: &str) {
    }

    fn subsystem(&mut self, _: &str) {
    }

    fn out_kind(&mut self, kind: LibKind, _: &Path) {
        if let LibKind::Executable = kind {
            self.emit_main = true;
        }
    }

    fn build(&mut self, out: &Path) {
        self.out_file = out.to_path_buf();
    }

    fn run(&mut self) -> Result<(), LinkError> {
        use std::io::{BufRead, Write};
        std::fs::create_dir_all(self.out_file.parent().unwrap())?;
        let mut emitted = HashSet::new();
        let mut out = std::fs::File::create(&self.out_file)?;

        for filename in self.files.drain(..) {
            if emitted.contains(&filename) {
                continue;
            }

            writeln!(out, "//- {}", filename.display())?;
            let file = std::fs::File::open(&filename)?;
            let mut file = std::io::BufReader::new(file);
            let mut line = String::new();
            let mut skip = false;

            while let Ok(n) = file.read_line(&mut line) {
                if n == 0 {
                    break;
                }

                if line.starts_with("//- ") {
                    skip = false;

                    if emitted.iter().any(|e| e == Path::new(&line[3..])) {
                        skip = true;
                        line.clear();
                        continue;
                    } else {
                        emitted.insert(Path::new(&line[3..]).to_path_buf());
                    }
                }

                if !skip {
                    out.write_all(line.as_bytes())?;
                }

                line.clear();
            }

            emitted.insert(filename);
        }

        if self.emit_main {
            out.write_all(b"console.log($main());\n")?;
        }

        Ok(())
    }
}
