#![feature(or_patterns)]
#![feature(str_split_as_str)]

mod interactive;

use clap::clap_app;
use driver::{Driver, Opts};

fn main() {
    let matches = clap_app!(shadow =>
        (@setting ArgRequiredElseHelp)
        (@setting VersionlessSubcommands)
        (@arg interactive: -i --interactive "starts the compiler in interactive mode")
        (@subcommand build =>
            (@arg input: +takes_value default_value("."))
        )
        (@subcommand run =>
            (@setting TrailingVarArg)
            (@arg input: +takes_value default_value("."))
            (@arg args: ...)
        )
    )
    .get_matches();

    std::panic::set_hook(Box::new(|info| {
        let loc = info.location().unwrap();
        let msg = match info.payload().downcast_ref::<&'static str>() {
            | Some(s) => *s,
            | None => match info.payload().downcast_ref::<String>() {
                | Some(s) => &s[..],
                | None => "Box<Any>",
            },
        };

        eprintln!("\x1B[31mInternal Compiler Error\x1B[0m: '{}' at {}", msg, loc);
    }));

    if let Some(matches) = matches.subcommand_matches("build") {
        let input = matches.value_of("input").unwrap();

        if let Some(driver) = Driver::init(Opts { input }) {
            driver.build();
        }
    } else if let Some(matches) = matches.subcommand_matches("run") {
        let input = matches.value_of("input").unwrap();

        if let Some(driver) = Driver::init(Opts { input }) {
            driver.build();
        }
    } else {
        interactive::run();
    }
}
