use std::ffi::{OsStr, OsString};
use std::path::Path;
use std::process::Command;

pub fn create() -> Box<dyn Linker> {
    Box::new(LdLinker::new())
}

pub trait Linker {
    fn cmd(&mut self) -> &mut Command;

    fn add_object(&mut self, path: &Path);

    fn build_shared_object(&mut self, out: &Path);

    fn run(&mut self) {
        self.cmd().status().unwrap();
    }
}

impl dyn Linker + '_ {
    pub fn arg(&mut self, arg: impl AsRef<OsStr>) {
        self.cmd().arg(arg);
    }

    pub fn args(&mut self, args: impl IntoIterator<Item = impl AsRef<OsStr>>) {
        self.cmd().args(args);
    }
}

pub struct LdLinker {
    cmd: Command,
}

impl LdLinker {
    pub fn new() -> Self {
        LdLinker {
            cmd: Command::new("ld"),
        }
    }
}

impl Linker for LdLinker {
    fn cmd(&mut self) -> &mut Command {
        &mut self.cmd
    }

    fn add_object(&mut self, path: &Path) {
        self.cmd.arg(path);
    }

    fn build_shared_object(&mut self, out: &Path) {
        self.cmd.arg("--shared");
        self.cmd.arg("-o");
        self.cmd.arg(out);
    }
}
