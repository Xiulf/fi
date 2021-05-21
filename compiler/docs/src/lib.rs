pub mod db;
pub mod elems;

use hir::db::HirDatabase;
use html_builder::{Document, Html5, Node};
use relative_path::{RelativePath, RelativePathBuf};
use rustc_hash::FxHashSet;
use std::fmt::Write as _;
use std::path::{Path, PathBuf};
use std::{fs, io};

pub fn generate(db: &dyn HirDatabase, lib: hir::Lib, target_dir: &Path) {
    let cwd = std::env::current_dir().unwrap();
    let target_dir = target_dir.join("docs");
    let target_dir = target_dir.strip_prefix(cwd).unwrap_or(&target_dir);

    Generator {
        db,
        visited: FxHashSet::default(),
        target_dir: RelativePathBuf::from_path(target_dir).unwrap(),
    }
    .generate_lib(lib)
    .unwrap();
}

struct Generator<'a> {
    db: &'a dyn HirDatabase,
    visited: FxHashSet<hir::Lib>,
    target_dir: RelativePathBuf,
}

impl Generator<'_> {
    fn generate_lib(&self, lib: hir::Lib) -> io::Result<()> {
        let dir = self.target_dir.join(lib.name(self.db).to_string());
        let module = lib.root_module(self.db);

        fs::create_dir_all(dir.to_path("."))?;
        self.generate_module(module, &dir)?;

        for dep in lib.dependencies(self.db) {
            self.generate_lib(dep.lib)?;
        }

        Ok(())
    }

    fn generate_module(&self, module: hir::Module, dir: &RelativePath) -> io::Result<RelativePathBuf> {
        let name = module.name(self.db);
        let dir = dir.join(name.to_string());
        let decls = module.declarations(self.db);
        let mut doc = Document::new();
        let mut html = doc.html().attr("lang='en'");

        write!(html.head().title(), "module {}", name);

        let mut body = html.body();
        let mut main = body.main();
        let mut modules = main.section().attr("class='modules'");
        // let mut funcs = main.section().attr("class='funcs'");
        // let mut statics = main.section().attr("class='statics'");
        // let mut consts = main.section().attr("class='consts'");
        // let mut types = main.section().attr("class='types'");
        // let mut aliases = main.section().attr("class='aliases'");
        // let mut classes = main.section().attr("class='classes'");

        for def in &decls {
            if let hir::ModuleDef::Module(it) = *def {
                if !it.is_virtual(self.db) {
                    fs::create_dir_all(dir.to_path("."))?;

                    let name2 = it.name(self.db);
                    let path = self.generate_module(it, &dir)?;
                    let path = dir.relative(path);
                    let mut module = modules.a().attr(&format!("href = '{}/{}'", name, path.display()));

                    write!(module, "{}", name2);
                }
            }
        }

        let mut fixities = main.section().attr("class='fixities'");

        write!(fixities.h3(), "Infix Operators");

        for def in &decls {
            if let hir::ModuleDef::Fixity(it) = *def {
                fs::create_dir_all(dir.to_path("."))?;

                let name2 = it.name(self.db);
                let path = self.generate_fixity(it, &dir)?;
                let path = dir.relative(path);
                let mut fixity = fixities.a().attr(&format!("href = '{}/{}'", name, path.display()));

                write!(fixity, "{}", name2);
            }
        }

        let dir = dir.with_extension("html");

        fs::write(dir.to_path("."), doc.build())?;

        Ok(dir)
    }

    fn generate_fixity(&self, fixity: hir::Fixity, dir: &RelativePath) -> io::Result<RelativePathBuf> {
        let name = fixity.name(self.db);
        let dir = dir.join(name.to_string()).with_extension("html");
        let mut doc = Document::new();
        let mut html = doc.html().attr("lang='en'");

        write!(html.head().title(), "infix operator {}", name);

        fs::write(dir.to_path("."), doc.build())?;

        Ok(dir)
    }
}
