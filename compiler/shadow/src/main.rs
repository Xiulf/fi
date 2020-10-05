use clap::{App, AppSettings, Arg, SubCommand};

fn main() {
    let app = App::new(clap::crate_name!())
        .version(clap::crate_version!())
        .author(clap::crate_authors!())
        .about(clap::crate_description!())
        .setting(AppSettings::SubcommandRequired)
        .subcommand(
            SubCommand::with_name("build")
                .arg(
                    Arg::with_name("input")
                        .takes_value(true)
                        .required(false)
                        .default_value("."),
                )
                .arg(
                    Arg::with_name("target")
                        .long("target")
                        .takes_value(true)
                        .required(false),
                )
                .arg(
                    Arg::with_name("output-type")
                        .long("output-type")
                        .takes_value(true)
                        .possible_values(&["bin", "dylib", "lib"])
                        .default_value("bin"),
                ),
        )
        .subcommand(
            SubCommand::with_name("run")
                .setting(AppSettings::TrailingVarArg)
                .arg(
                    Arg::with_name("input")
                        .takes_value(true)
                        .required(false)
                        .default_value("."),
                )
                .arg(
                    Arg::with_name("target")
                        .long("target")
                        .takes_value(true)
                        .required(false),
                )
                .arg(Arg::with_name("bin-args").multiple(true)),
        );

    let matches = app.get_matches();

    if let Some(matches) = matches.subcommand_matches("build") {
        let project_dir: std::path::PathBuf = matches.value_of("input").unwrap().into();
        let opts = driver::Opts {
            target: match matches.value_of("target") {
                Some(s) => s.parse().unwrap(),
                None => target_lexicon::HOST,
            },
            out_type: match matches.value_of("output-type").unwrap() {
                "bin" => driver::OutputType::Bin,
                "dylib" => driver::OutputType::DyLib,
                "lib" => driver::OutputType::Lib,
                _ => unreachable!(),
            },
            project_dir,
        };

        driver::build(opts);
    } else if let Some(matches) = matches.subcommand_matches("run") {
        let project_dir: std::path::PathBuf = matches.value_of("input").unwrap().into();
        let opts = driver::Opts {
            target: match matches.value_of("target") {
                Some(s) => s.parse().unwrap(),
                None => target_lexicon::HOST,
            },
            out_type: driver::OutputType::Bin,
            project_dir,
        };

        let files = driver::build(opts);

        println!(
            "  \x1B[1m\x1B[32mRunning\x1B[0m {} {}({})",
            files.manifest.package.name,
            if let Some(v) = &files.manifest.package.version {
                format!("v{} ", v)
            } else {
                String::new()
            },
            files.bin.display()
        );

        let mut cmd = std::process::Command::new(files.bin);

        for arg in matches.values_of("bin-args").unwrap() {
            cmd.arg(arg);
        }

        let status = cmd.status().unwrap();

        std::process::exit(status.code().unwrap_or(0));
    }
}
