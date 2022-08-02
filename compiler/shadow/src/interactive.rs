use base_db::input::{FileId, LineCol};
use base_db::libs::LibId;
use base_db::SourceDatabaseExt as _;
use driver::Driver;
use hir::semantics::Semantics;
use hir::HirDisplay;
use markup::{Markup, MarkupRenderer, Styles};
use repl::{ReadLine, Repl};
use syntax::ast::AstNode;

pub fn run() {
    let (driver, lib, file) = Driver::interactive();

    Interactive {
        repl: Repl::new((), ()),
        imports: Vec::new(),
        lines: vec![String::new()],
        driver,
        lib,
        file,
    }
    .run();
}

struct Interactive {
    repl: Repl<(), ()>,
    driver: Driver,
    lib: LibId,
    file: FileId,

    imports: Vec<String>,
    lines: Vec<String>,
}

impl Interactive {
    fn run(&mut self) {
        loop {
            match self.repl.read_line("> ") {
                | Ok(ReadLine::Line(text)) => {
                    let mut words = text.split(char::is_whitespace);

                    match words.next() {
                        | Some(".exit") => break,
                        | Some(".clear") => {
                            self.repl.clear().unwrap();
                        },
                        | Some(".reset") => {
                            self.imports.clear();
                            self.lines = vec![String::new()];
                        },
                        | Some(".code") => {
                            println!("{}", self.text());
                        },
                        | Some(".load") => self.load(words.as_str()),
                        | Some(".import") => self.add_import(words.as_str()),
                        | Some(".let") => self.add_let(words.as_str()),
                        | Some(".r" | ".resolve") => self.resolve(words.as_str()),
                        | Some(".t" | ".type") => self.type_of(words.as_str()),
                        | Some(".k" | ".kind") => self.kind_of(words.as_str()),
                        | Some(_) => self.eval(&text),
                        | None => {},
                    }
                },
                | Ok(ReadLine::Exit) => break,
                | Err(e) => {
                    eprintln!("{}", e);
                    std::process::exit(1);
                },
            }
        }
    }

    fn load(&mut self, text: &str) {
        if let Some(lib) = self.driver.load(text) {
            self.driver.add_dep(self.lib, lib);
        }
    }

    fn add_import(&mut self, text: &str) {
        self.imports.push(text.to_string());
    }

    fn add_let(&mut self, text: &str) {
        *self.lines.last_mut().unwrap() = format!("let {}", text);
        self.lines.push(String::new());
    }

    fn resolve(&mut self, text: &str) {
        if text.is_empty() {
            return;
        }

        *self.lines.last_mut().unwrap() = format!("let _ = _ `{}` _", text);
        self.driver.db.set_file_text(self.file, self.text().into());

        let sema = Semantics::new(&self.db);
        let parsed = sema.parse(self.file);
        let offset = self.offset(15);

        if let Some(path) = sema.find_node_at_offset::<syntax::ast::Path>(parsed.syntax(), offset) {
            let resolution = sema.resolve_path(&path);

            if let Some(resolution) = resolution {
                let markup = format_resolution(&self.db, resolution);

                TermRenderer.render_markup(&markup);
                println!();
            } else {
                println!("not found");
            }
        } else {
            println!("invalid path");
        }
    }

    fn type_of(&mut self, text: &str) {
        if text.is_empty() {
            return;
        }

        *self.lines.last_mut().unwrap() = format!("let _ = {}", text);
        self.driver.db.set_file_text(self.file, self.text().into());

        let sema = Semantics::new(&self.db);
        let parsed = sema.parse(self.file);
        let offset = self.offset(12);

        if let Some(mut expr) = sema.find_node_at_offset::<syntax::ast::Expr>(parsed.syntax(), offset) {
            while let Some(parent) = expr.parent() {
                expr = parent;
            }

            let ty = sema.type_of_expr(&expr).unwrap();

            println!("{}", ty.display(&self.db));
        } else {
            println!("invalid expression");
        }
    }

    fn kind_of(&mut self, text: &str) {
        if text.is_empty() {
            return;
        }

        *self.lines.last_mut().unwrap() = format!("let _ = _ :: {}", text);
        self.driver.db.set_file_text(self.file, self.text().into());

        let sema = Semantics::new(&self.db);
        let parsed = sema.parse(self.file);
        let offset = self.offset(17);

        if let Some(mut ty) = sema.find_node_at_offset::<syntax::ast::Type>(parsed.syntax(), offset) {
            while let Some(parent) = ty.parent() {
                ty = parent;
            }

            let kind = sema.kind_of(&ty).unwrap();

            println!("{}", kind.display(&self.db));
        } else {
            println!("invalid type");
        }
    }

    fn eval(&mut self, text: &str) {
        if text.is_empty() {
            return;
        }

        *self.lines.last_mut().unwrap() = text.to_string();
        self.driver.db.set_file_text(self.file, self.text().into());
    }

    fn text(&self) -> String {
        let mut text = String::from("module INTERACTIVE =");

        for import in &self.imports {
            text.push_str("\nimport ");
            text.push_str(import);
        }

        text.push_str("\nmain =");

        for line in &self.lines {
            text.push_str("\n    ");
            text.push_str(line);
        }

        text
    }

    fn offset(&self, col: u32) -> syntax::TextSize {
        let line_index = self.db.line_index(self.file);

        line_index.offset(LineCol {
            line: self.imports.len() as u32 + self.lines.len() as u32 + 1,
            col,
        })
    }
}

impl std::ops::Deref for Interactive {
    type Target = Driver;

    fn deref(&self) -> &Self::Target {
        &self.driver
    }
}

fn format_resolution(db: &dyn hir::db::HirDatabase, resolution: hir::PathResolution) -> Markup {
    match resolution {
        | hir::PathResolution::Local(local) => {
            let name = local.name(db);

            Markup::new()
                .text("let ", Styles::BOLD)
                .text(name.to_string(), Styles::NONE)
        },
        | hir::PathResolution::TypeVar(type_var) => {
            let name = type_var.name(db);

            Markup::new()
                .text("forall ", Styles::BOLD)
                .text(name.to_string(), Styles::NONE)
        },
        | hir::PathResolution::Def(def) => {
            let name = def.name(db);
            let m = Markup::new();
            let m = match def {
                | hir::ModuleDef::Module(_) => {
                    return m
                        .text("module ", Styles::BOLD)
                        .text(name.to_string(), Styles::UNDERLINE)
                },
                | hir::ModuleDef::Fixity(f) => match f.kind(db) {
                    | hir::FixityKind::Infix { assoc, .. } => match assoc {
                        | hir::Assoc::Left => m.text("infixl ", Styles::BOLD),
                        | hir::Assoc::Right => m.text("infixr ", Styles::BOLD),
                        | hir::Assoc::None => m.text("infix ", Styles::BOLD),
                    },
                    | hir::FixityKind::Postfix => m.text("postfix ", Styles::BOLD),
                    | hir::FixityKind::Prefix => m.text("prefix ", Styles::BOLD),
                },
                | hir::ModuleDef::Func(_) => m.text("function ", Styles::BOLD),
                | hir::ModuleDef::Static(_) => m.text("static ", Styles::BOLD),
                | hir::ModuleDef::Const(_) => m.text("const ", Styles::BOLD),
                | hir::ModuleDef::TypeAlias(_) => m.text("type ", Styles::BOLD),
                | hir::ModuleDef::TypeCtor(_) => m.text("type ", Styles::BOLD),
                | hir::ModuleDef::Ctor(_) => m.text("ctor ", Styles::BOLD),
                | hir::ModuleDef::Class(_) => m.text("class ", Styles::BOLD),
            };

            let module = def.module(db).unwrap().name(db);

            m.text(module.to_string(), Styles::UNDERLINE)
                .text(".", Styles::NONE)
                .text(name.to_string(), Styles::UNDERLINE)
        },
    }
}

struct TermRenderer;

impl MarkupRenderer for TermRenderer {
    fn render_newline(&mut self) {
        println!();
    }

    fn render_text(&mut self, text: &String, styles: Styles) {
        if styles.bold() {
            print!("\x1B[1m");
        }

        if styles.italic() {
            print!("\x1B[3m");
        }

        if styles.underline() {
            print!("\x1B[4m");
        }

        print!("{}\x1B[0m", text);
    }

    fn render_header(&mut self, text: &String, _level: u8) {
        print!("\x1B[1m{}\x1B[0m", text);
    }

    fn render_line(&mut self) {
        print!("----------");
    }
}
