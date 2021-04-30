use base_db::input::FileId;
use base_db::libs::LibId;
use base_db::SourceDatabase as _;
use base_db::SourceDatabaseExt as _;
use driver::Driver;
use hir::db::DefDatabase as _;
use markup::{Markup, MarkupRenderer, Styles};
use repl::{ReadLine, Repl};

pub fn run() {
    let (driver, lib, main_file, type_file, resolve_file) = Driver::interactive();

    Interactive {
        repl: Repl::new((), ()),
        main_str: String::new(),
        type_str: String::new(),
        resolve_str: String::new(),
        driver,
        lib,
        main_file,
        type_file,
        resolve_file,
    }
    .run();
}

struct Interactive {
    repl: Repl<(), ()>,
    driver: Driver,
    lib: LibId,
    main_file: FileId,
    type_file: FileId,
    resolve_file: FileId,

    main_str: String,
    type_str: String,
    resolve_str: String,
}

impl Interactive {
    fn run(&mut self) {
        loop {
            match self.repl.read_line("> ") {
                | Ok(ReadLine::Line(text)) => {
                    let mut words = text.split(char::is_whitespace);

                    match words.next() {
                        | Some(".exit") => break,
                        | Some(".clear") => self.repl.clear().unwrap(),
                        | Some(".load") => self.load(words.as_str()),
                        | Some(".r" | ".resolve") => self.resolve(words.as_str()),
                        | Some(".t" | ".type") => self.type_(words.as_str()),
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

    fn resolve(&mut self, text: &str) {
        self.resolve_str.clear();
        self.resolve_str.push_str(text);
        self.driver
            .db
            .set_file_text(self.resolve_file, self.resolve_str.clone().into());

        let sema = hir::semantics::Semantics::new(&self.db);
        let parsed = sema.parse_path(self.resolve_file);
        let resolution = sema.resolve_path_in(&parsed, self.main_file);

        if let Some(resolution) = resolution {
            let markup = format_resolution(&self.db, resolution);

            TermRenderer.render_markup(&markup);
            println!();
        } else {
            println!("not found");
        }
    }

    fn type_(&mut self, text: &str) {
    }

    fn eval(&mut self, text: &str) {
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
        | hir::PathResolution::Def(def) => {
            let name = def.name(db);

            Markup::new().text(name.to_string(), Styles::UNDERLINE)
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
