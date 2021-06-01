use std::ffi::{OsStr, OsString};
use std::path::Path;
use std::process::Command;

pub fn create() -> Box<dyn Linker> {
    Box::new(CcLinker::new())
}

pub trait Linker {
    fn cmd(&mut self) -> &mut Command;

    fn rpath(&mut self, rpath: &Path);

    fn add_object(&mut self, path: &Path);
    fn add_shared_object(&mut self, name: &str);
    fn add_static_lib(&mut self, name: &str);

    fn build_shared_object(&mut self, out: &Path);
    fn build_static_lib(&mut self, out: &Path);
    fn build_executable(&mut self, out: &Path);

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

    fn rpath(&mut self, rpath: &Path) {
        self.cmd.arg("-rpath");
        self.cmd.arg(rpath);
    }

    fn add_object(&mut self, path: &Path) {
        self.cmd.arg(path);
    }

    fn add_shared_object(&mut self, name: &str) {
        self.cmd.arg("-Bdynamic");
        self.cmd.arg("-l");
        self.cmd.arg(name);
    }

    fn add_static_lib(&mut self, name: &str) {
        self.cmd.arg("-Bstatic");
        self.cmd.arg("-l");
        self.cmd.arg(name);
    }

    fn build_shared_object(&mut self, out: &Path) {
        self.cmd.arg("-shared");
        self.cmd.arg("-o");
        self.cmd.arg(out);
    }

    fn build_static_lib(&mut self, out: &Path) {
        self.cmd.arg("-static");
        self.cmd.arg("-o");
        self.cmd.arg(out);
    }

    fn build_executable(&mut self, out: &Path) {
        self.cmd.arg("-no-pie");
        self.cmd.arg("-o");
        self.cmd.arg(out);
    }
}

pub struct CcLinker {
    cmd: Command,
}

impl CcLinker {
    pub fn new() -> Self {
        CcLinker {
            cmd: Command::new("cc"),
        }
    }
}

impl Linker for CcLinker {
    fn cmd(&mut self) -> &mut Command {
        &mut self.cmd
    }

    fn rpath(&mut self, rpath: &Path) {
        self.cmd.arg("-Wl,-rpath");
        self.cmd.arg(rpath);
    }

    fn add_object(&mut self, path: &Path) {
        self.cmd.arg(path);
    }

    fn add_shared_object(&mut self, name: &str) {
        self.cmd.arg("-Wl,-Bdynamic");
        self.cmd.arg("-l");
        self.cmd.arg(name);
    }

    fn add_static_lib(&mut self, name: &str) {
        self.cmd.arg("-Wl,-Bstatic");
        self.cmd.arg("-l");
        self.cmd.arg(name);
    }

    fn build_shared_object(&mut self, out: &Path) {
        self.cmd.arg("-shared");
        self.cmd.arg("-o");
        self.cmd.arg(out);
    }

    fn build_static_lib(&mut self, out: &Path) {
        self.cmd.arg("-static");
        self.cmd.arg("-o");
        self.cmd.arg(out);
    }

    fn build_executable(&mut self, out: &Path) {
        self.cmd.arg("-no-pie");
        self.cmd.arg("-o");
        self.cmd.arg(out);
    }
}
