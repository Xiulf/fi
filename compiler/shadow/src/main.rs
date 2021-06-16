#![feature(or_patterns)]
#![feature(str_split_as_str)]

mod interactive;

use clap::clap_app;
use driver::{Driver, Opts};

fn main() {
    let matches = clap_app!(shadow =>
        (@setting VersionlessSubcommands)
        (@subcommand check =>
            (@arg input: +takes_value default_value("."))
        )
        (@subcommand build =>
            (@arg input: +takes_value default_value("."))
        )
        (@subcommand run =>
            (@setting TrailingVarArg)
            (@arg input: +takes_value default_value("."))
            (@arg args: ...)
        )
        (@subcommand docs =>
            (@arg input: +takes_value default_value("."))
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

    if let Some(matches) = matches.subcommand_matches("check") {
        let input = matches.value_of("input").unwrap();

        if let Some((driver, _)) = Driver::init(Opts { input }) {
            driver.check();
        }
    } else if let Some(matches) = matches.subcommand_matches("build") {
        let input = matches.value_of("input").unwrap();

        if let Some((driver, _)) = Driver::init(Opts { input }) {
            driver.build();
        }
    } else if let Some(matches) = matches.subcommand_matches("run") {
        let input = matches.value_of("input").unwrap();

        if let Some((driver, lib)) = Driver::init(Opts { input }) {
            let status = if let Some(args) = matches.values_of_os("args") {
                driver.run(lib, args.into_iter())
            } else {
                driver.run(lib, std::iter::empty())
            };

            if status {
                std::process::exit(0);
            } else {
                std::process::exit(1);
            }
        }
    } else if let Some(matches) = matches.subcommand_matches("docs") {
        let input = matches.value_of("input").unwrap();

        if let Some((driver, lib)) = Driver::init(Opts { input }) {
            driver.docs(lib);
        }
    } else {
        interactive::run();
    }
}
