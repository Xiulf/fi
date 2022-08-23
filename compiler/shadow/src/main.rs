#![feature(str_split_as_str)]

mod interactive;

use std::io;

use base_db::libs::LibKind;
use clap::clap_app;
use driver::manifest::{Cfg, TomlValue};
use driver::{Driver, Opts};

fn main() -> io::Result<()> {
    let matches = clap_app!(shadow =>
        (@setting VersionlessSubcommands)
        (@arg file: +takes_value)
        (@arg target: --target +takes_value)
        (@arg output: --output +takes_value)
        (@arg cfg: --cfg +takes_value +global +multiple)
        (@subcommand check =>
            (@arg input: +takes_value default_value("."))
        )
        (@subcommand build =>
            (@arg target: --target +takes_value)
            (@arg input: +takes_value default_value("."))
        )
        (@subcommand run =>
            (@setting TrailingVarArg)
            (@arg target: --target +takes_value)
            (@arg input: +takes_value default_value("."))
            (@arg args: ...)
        )
    )
    .get_matches();

    env_logger::builder()
        .filter_level(log::LevelFilter::Debug)
        .filter_module("salsa", log::LevelFilter::Off)
        .format_timestamp(None)
        .init();

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

    let cfg: Cfg = matches
        .values_of("cfg")
        .map(|v| v.filter_map(parse_cfg).collect())
        .unwrap_or_default();

    if let Some(matches) = matches.subcommand_matches("check") {
        let input = matches.value_of("input").unwrap();

        if let Some((driver, _)) = Driver::init(Opts {
            input,
            cfg,
            ..Opts::default()
        }) {
            driver.check()?;
        }
    } else if let Some(matches) = matches.subcommand_matches("build") {
        let input = matches.value_of("input").unwrap();
        let target = matches.value_of("target");

        if let Some((driver, _)) = Driver::init(Opts {
            input,
            target,
            cfg,
            ..Opts::default()
        }) {
            driver.build()?;
        }
    } else if let Some(matches) = matches.subcommand_matches("run") {
        let input = matches.value_of("input").unwrap();
        let target = matches.value_of("target");

        if let Some((driver, lib)) = Driver::init(Opts {
            input,
            target,
            cfg,
            ..Opts::default()
        }) {
            let status = if let Some(args) = matches.values_of_os("args") {
                driver.run(lib, args.into_iter())
            } else {
                driver.run(lib, std::iter::empty())
            }?;

            if status {
                std::process::exit(0);
            } else {
                std::process::exit(1);
            }
        }
    } else if let Some(input) = matches.value_of("file") {
        let target = matches.value_of("target");
        let output = matches.value_of("output").map(|o| match o {
            | "dynamic" => LibKind::Dynamic,
            | "static" => LibKind::Static,
            | "executable" => LibKind::Executable,
            | _ => panic!("invalid output kind '{}'", o),
        });

        if let Some((driver, _)) = Driver::init_no_manifest(Opts {
            input,
            target,
            output,
            cfg,
        }) {
            driver.build()?;
        }
    } else {
        interactive::run();
    }

    Ok(())
}

fn parse_cfg(s: &str) -> Option<(String, TomlValue)> {
    if let Some((key, value)) = s.split_once('=') {
        let value = if let Ok(i) = value.parse::<i64>() {
            TomlValue::Integer(i)
        } else if let Ok(f) = value.parse::<f64>() {
            TomlValue::Float(f)
        } else {
            TomlValue::String(value.into())
        };

        Some((key.into(), value))
    } else {
        Some((s.into(), TomlValue::Boolean(true)))
    }
}
