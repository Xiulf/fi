use clap::{App, AppSettings, Arg, SubCommand};

fn main() {
    let app = App::new(clap::crate_name!())
        .version(clap::crate_version!())
        .author(clap::crate_authors!())
        .about(clap::crate_description!())
        .setting(AppSettings::SubcommandRequired)
        .subcommand(
            SubCommand::with_name("build")
                .arg(Arg::with_name("input").takes_value(true).required(true))
                .arg(
                    Arg::with_name("target")
                        .long("target")
                        .takes_value(true)
                        .required(false),
                )
                .arg(
                    Arg::with_name("target_dir")
                        .long("ouptut")
                        .takes_value(true)
                        .required(false),
                ),
        );

    let matches = app.get_matches();

    if let Some(matches) = matches.subcommand_matches("build") {
        let entry: std::path::PathBuf = matches.value_of("input").unwrap().into();
        let opts = driver::Opts {
            target: match matches.value_of("target") {
                Some(s) => s.parse().unwrap(),
                None => target_lexicon::HOST,
            },
            target_dir: match matches.value_of("target_dir") {
                Some(dir) => dir.into(),
                None => {
                    if entry.parent().unwrap().file_name().unwrap() == "src" {
                        entry.parent().unwrap().parent().unwrap().join("target")
                    } else {
                        entry.parent().unwrap().join("target")
                    }
                }
            },
            entry,
        };

        driver::build(opts);
    }
}
