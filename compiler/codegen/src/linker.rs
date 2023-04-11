use std::io;
use std::path::Path;
use std::process::Command;

use base_db::libs::LibKind;

use crate::target::{LinkerFlavor, Target};

pub fn create(target: &Target) -> Box<dyn Linker> {
    match target.linker_flavor {
        | LinkerFlavor::Msvc => Box::new(MsvcLinker::new(target)),
        | LinkerFlavor::Ld => Box::new(CcLinker::new()),
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

// #[derive(Debug)]
// pub struct ElfLinker {
//     args: Vec<String>,
// }

// impl ElfLinker {
//     pub fn new() -> Self {
//         Self { args: Vec::new() }
//     }

//     fn arg(&mut self, arg: impl AsRef<std::ffi::OsStr>) -> &mut Self {
//         self.args.push(arg.as_ref().to_str().unwrap().to_string());
//         self
//     }
// }

// impl Linker for ElfLinker {
//     fn runtime_path(&mut self, rpath: &Path) {
//         self.arg("--rpath");
//         self.arg(rpath);
//     }

//     fn add_path(&mut self, path: &Path) {
//         self.arg("-L");
//         self.arg(path);
//     }

//     fn add_module(&mut self, path: &Path) {
//         self.arg(path);
//     }

//     fn add_lib(&mut self, kind: LibKind, lib: &str, _: &Path) {
//         match kind {
//             | LibKind::Dynamic => self.arg("--Bdynamic"),
//             | LibKind::Static => self.arg("--Bstatic"),
//             | LibKind::Executable => panic!("linking with executable"),
//         };

//         self.arg("-l");
//         self.arg(lib);
//     }

//     fn add_export(&mut self, _: &str) {
//     }

//     fn subsystem(&mut self, _: &str) {
//     }

//     fn out_kind(&mut self, kind: LibKind, _: &Path) {
//         match kind {
//             | LibKind::Dynamic => self.arg("--shared"),
//             | LibKind::Static => self.arg("--static"),
//             | LibKind::Executable => self.arg("--no-pie"),
//         };
//     }

//     fn build(&mut self, out: &Path) {
//         self.arg("--output");
//         self.arg(out);
//     }

//     fn run(&mut self) -> Result<(), LinkError> {
//         todo!();
//         // lld_rs::link(lld_rs::LldFlavor::Elf, &self.args)
//         //     .ok()
//         //     .map_err(LinkError::Lld)
//     }
// }

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
            | LibKind::DynamicLib => self.arg("-Bdynamic"),
            | LibKind::StaticLib => self.arg("-Bstatic"),
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
            | LibKind::DynamicLib => self.arg("-shared"),
            | LibKind::StaticLib => self.arg("-static"),
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
    pub fn new(target: &Target) -> Self {
        let link_exe = cc::windows_registry::find_tool(&target.triple.to_string(), "link.exe").unwrap();

        Self {
            cmd: link_exe.to_command(),
        }
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
            | LibKind::StaticLib => {
                self.arg(lib);
            },
            | LibKind::DynamicLib => {
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
            | LibKind::DynamicLib => {
                self.arg("/DLL");
                self.arg("/NOENTRY");
                self.arg(format!("/IMPLIB:{}", out.with_extension("dll.lib").display()));
            },
            | LibKind::StaticLib => {
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
