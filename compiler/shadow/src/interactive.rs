use driver::Driver;
use repl::{ReadLine, Repl};

pub fn run() {
    let mut driver = Driver::interactive();
    let mut repl = Repl::new((), ());

    loop {
        match repl.read_line("> ") {
            | Ok(ReadLine::Line(text)) => {
                let mut words = text.split(char::is_whitespace);

                match words.next() {
                    | Some(".exit") => break,
                    | Some(".clear") => repl.clear().unwrap(),
                    | Some(".load") => load(&mut driver, words.as_str()),
                    | Some(".r" | ".resolve") => resolve(&mut driver, words.as_str()),
                    | Some(".t" | ".type") => type_(&mut driver, words.as_str()),
                    | Some(_) => eval(&mut driver, &text),
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

fn load(driver: &mut Driver, text: &str) {
    let _ = driver.load(text);
}

fn resolve(driver: &mut Driver, text: &str) {
}

fn type_(driver: &mut Driver, text: &str) {
}

fn eval(driver: &mut Driver, text: &str) {
}
