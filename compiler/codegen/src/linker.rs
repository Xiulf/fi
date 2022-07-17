use std::io;
use std::path::{Path, PathBuf};
use std::process::Command;

use base_db::libs::LibKind;

use crate::CompilerTarget;

pub fn create(target: CompilerTarget) -> Box<dyn Linker> {
    match target {
        | CompilerTarget::Javascript => Box::new(JsLinker::new()),
    }
    // Box::new(CcLinker::new())
}

pub trait Linker {
    fn runtime_path(&mut self, rpath: &Path);
    fn add_module(&mut self, path: &Path);
    fn add_object(&mut self, kind: LibKind, path: &Path);
    fn out_kind(&mut self, kind: LibKind);
    fn build(&mut self, out: &Path);
    fn run(&mut self) -> io::Result<()>;
}

pub struct LdLinker {
    cmd: Command,
}

impl LdLinker {
    pub fn new() -> Self {
        Self {
            cmd: Command::new("ld"),
        }
    }
}

impl Linker for LdLinker {
    fn runtime_path(&mut self, rpath: &Path) {
        self.cmd.arg("-rpath");
        self.cmd.arg(rpath);
    }

    fn add_module(&mut self, path: &Path) {
        self.cmd.arg(path);
    }

    fn add_object(&mut self, kind: LibKind, path: &Path) {
        match kind {
            | LibKind::Dynamic => self.cmd.arg("-Bdynamic"),
            | LibKind::Static => self.cmd.arg("-Bstatic"),
            | LibKind::Executable => panic!("linking with executable"),
        };

        self.cmd.arg("-l");
        self.cmd.arg(path);
    }

    fn out_kind(&mut self, kind: LibKind) {
        match kind {
            | LibKind::Dynamic => self.cmd.arg("-shared"),
            | LibKind::Static => self.cmd.arg("-static"),
            | LibKind::Executable => self.cmd.arg("-no-pie"),
        };
    }

    fn build(&mut self, out: &Path) {
        self.cmd.arg("-o");
        self.cmd.arg(out);
    }

    fn run(&mut self) -> io::Result<()> {
        self.cmd.status().map(|_| ())
    }
}

pub struct CcLinker {
    cmd: Command,
}

impl CcLinker {
    pub fn new() -> Self {
        Self {
            cmd: Command::new("cc"),
        }
    }
}

impl Linker for CcLinker {
    fn runtime_path(&mut self, rpath: &Path) {
        self.cmd.arg("-Wl,-rpath");
        self.cmd.arg(rpath);
    }

    fn add_module(&mut self, path: &Path) {
        self.cmd.arg(path);
    }

    fn add_object(&mut self, kind: LibKind, path: &Path) {
        match kind {
            | LibKind::Dynamic => self.cmd.arg("-Wl,-Bdynamic"),
            | LibKind::Static => self.cmd.arg("-Wl,-Bstatic"),
            | LibKind::Executable => panic!("linking with executable"),
        };

        self.cmd.arg("-l");
        self.cmd.arg(path);
    }

    fn out_kind(&mut self, kind: LibKind) {
        match kind {
            | LibKind::Dynamic => self.cmd.arg("-shared"),
            | LibKind::Static => self.cmd.arg("-static"),
            | LibKind::Executable => self.cmd.arg("-no-pie"),
        };
    }

    fn build(&mut self, out: &Path) {
        self.cmd.arg("-o");
        self.cmd.arg(out);
    }

    fn run(&mut self) -> io::Result<()> {
        self.cmd.status().map(|_| ())
    }
}

pub struct JsLinker {
    files: Vec<PathBuf>,
    out_file: PathBuf,
}

impl JsLinker {
    pub fn new() -> Self {
        Self {
            files: Vec::new(),
            out_file: PathBuf::new(),
        }
    }
}

impl Linker for JsLinker {
    fn runtime_path(&mut self, _rpath: &Path) {
    }

    fn add_module(&mut self, path: &Path) {
        self.files.push(path.to_path_buf());
    }

    fn add_object(&mut self, _kind: LibKind, path: &Path) {
        self.files.push(path.to_path_buf());
    }

    fn out_kind(&mut self, _kind: LibKind) {
    }

    fn build(&mut self, out: &Path) {
        self.out_file = out.to_path_buf();
    }

    fn run(&mut self) -> io::Result<()> {
        std::fs::create_dir_all(self.out_file.parent().unwrap())?;
        let mut out = std::fs::File::create(&self.out_file)?;
        let mut buf = [0u8; 1024];

        for file in self.files.drain(..) {
            let mut file = std::fs::File::open(file)?;
            let mut read = io::Read::read(&mut file, &mut buf)?;

            while read > 0 {
                io::Write::write_all(&mut out, &buf[..read])?;
                read = io::Read::read(&mut file, &mut buf)?;
            }
        }

        Ok(())
    }
}
