use base_db::input::FileId;
use base_db::libs::LibId;
use base_db::SourceDatabase as _;
use base_db::SourceDatabaseExt as _;
use driver::Driver;
use hir::db::DefDatabase as _;
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
        let _ = self.driver.load(text);
    }

    fn resolve(&mut self, text: &str) {
        self.resolve_str.clear();
        self.resolve_str.push_str(text);
        self.driver
            .db
            .set_file_text(self.resolve_file, self.resolve_str.clone().into());

        let parsed = self.driver.db.parse_path(self.resolve_file);
        let path = hir::path::ModPath::lower(parsed.tree());
        let def_map = self.driver.db.def_map(self.lib);
        let (res, _) = def_map.resolve_path(&self.driver.db, def_map.root(), &path);

        println!("{:?}", res);
    }

    fn type_(&mut self, text: &str) {
    }

    fn eval(&mut self, text: &str) {
    }
}
