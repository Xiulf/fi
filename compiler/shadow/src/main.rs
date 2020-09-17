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
    }
}
