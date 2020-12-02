use crate::LinkOutputType;
use std::ffi::{OsStr, OsString};
use std::path::Path;
use std::process::Command;

pub trait Linker {
    fn cmd(&mut self) -> &mut Command;
    fn set_output_type(&mut self, output_type: LinkOutputType, out_filename: &Path);
    fn link_dylib(&mut self, path: &Path);
    fn link_staticlib(&mut self, path: &Path);
    fn output_filename(&mut self, path: &Path);
    fn add_object(&mut self, path: &Path);
    fn finalize(&mut self);

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

pub struct GccLinker {
    cmd: Command,
    hinted_static: bool,
    is_ld: bool,
}

impl GccLinker {
    pub fn new(cmd: Command, is_ld: bool) -> Self {
        GccLinker {
            cmd,
            is_ld,
            hinted_static: false,
        }
    }

    fn linker_arg(&mut self, arg: impl AsRef<OsStr>) -> &mut Self {
        if !self.is_ld {
            let mut os = OsString::from("-Wl,");
            os.push(arg.as_ref());
            self.cmd.arg(os);
        } else {
            self.cmd.arg(arg);
        }

        self
    }

    fn hint_static(&mut self) {
        if !self.hinted_static {
            self.linker_arg("-Bstatic");
            self.hinted_static = true;
        }
    }

    fn hint_dynamic(&mut self) {
        if self.hinted_static {
            self.linker_arg("-Bdynamic");
            self.hinted_static = false;
        }
    }

    fn build_dylib(&mut self, _out_filename: &Path) {
        self.cmd.arg("-shared");
    }
}

impl Linker for GccLinker {
    fn cmd(&mut self) -> &mut Command {
        &mut self.cmd
    }

    fn set_output_type(&mut self, output_type: LinkOutputType, out_filename: &Path) {
        match output_type {
            LinkOutputType::Exe => {
                self.cmd.arg("-pie");
            }
            LinkOutputType::Dylib => {
                self.build_dylib(out_filename);
            }
            LinkOutputType::Lib => {
                self.cmd.arg("-static");
                self.build_dylib(out_filename);
            }
        }
    }

    fn link_dylib(&mut self, path: &Path) {
        self.hint_dynamic();
        self.cmd.arg(format!("-l:{}", path.display()));
    }

    fn link_staticlib(&mut self, path: &Path) {
        self.hint_static();
        self.cmd.arg(format!("-l:{}", path.display()));
    }

    fn output_filename(&mut self, path: &Path) {
        self.cmd.arg("-o").arg(path);
    }

    fn add_object(&mut self, path: &Path) {
        self.cmd.arg(path);
    }

    fn finalize(&mut self) {
        self.hint_dynamic();
    }
}
