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
    fn add_object(&mut self, kind: LibKind, path: &str);
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

    fn add_object(&mut self, kind: LibKind, path: &str) {
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

    fn add_object(&mut self, kind: LibKind, path: &str) {
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

    fn add_module(&mut self, path: &Path) {
        self.add_file(path);
    }

    fn add_object(&mut self, _kind: LibKind, path: &str) {
        let mut path = Path::new(path).to_path_buf().with_extension("js");

        if !path.exists() {
            path = self.rpath.join(path);
        }

        self.add_file(path);
    }

    fn out_kind(&mut self, kind: LibKind) {
        if let LibKind::Executable = kind {
            self.emit_main = true;
        }
    }

    fn build(&mut self, out: &Path) {
        self.out_file = out.to_path_buf();
    }

    fn run(&mut self) -> io::Result<()> {
        use std::io::{BufRead, Write};
        std::fs::create_dir_all(self.out_file.parent().unwrap())?;
        let mut emitted = Vec::new();
        let mut out = std::fs::File::create(&self.out_file)?;

        for filename in self.files.drain(..) {
            if emitted.contains(&filename) {
                continue;
            }

            writeln!(out, "// {}", filename.display())?;
            let file = std::fs::File::open(&filename)?;
            let mut file = std::io::BufReader::new(file);
            let mut line = String::new();
            let mut skip = false;

            while let Ok(n) = file.read_line(&mut line) {
                if n == 0 {
                    break;
                }

                if line.starts_with("// ") {
                    skip = false;

                    if emitted.iter().any(|e| e == Path::new(&line[3..])) {
                        skip = true;
                        line.clear();
                        continue;
                    } else {
                        emitted.push(Path::new(&line[3..]).to_path_buf());
                    }
                }

                if !skip {
                    out.write_all(line.as_bytes())?;
                }

                line.clear();
            }

            emitted.push(filename);
        }

        if self.emit_main {
            out.write_all(b"$main();\n")?;
        }

        Ok(())
    }
}
