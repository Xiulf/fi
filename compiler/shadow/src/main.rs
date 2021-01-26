use clap::{App, AppSettings, Arg, SubCommand};

fn main() {
    let matches = App::new(clap::crate_name!())
        .version(clap::crate_version!())
        .author(clap::crate_authors!())
        .about(clap::crate_description!())
        .setting(AppSettings::SubcommandRequired)
        .subcommand(SubCommand::with_name("build").arg(Arg::with_name("input").takes_value(true).required(false).default_value(".")))
        .subcommand(SubCommand::with_name("run").arg(Arg::with_name("input").takes_value(true).required(false).default_value(".")))
        .subcommand(SubCommand::with_name("new").arg(Arg::with_name("name").required(true).takes_value(true)))
        .get_matches();

    if let Some(matches) = matches.subcommand_matches("build") {
        let project_dir = matches.value_of("input").unwrap().into();
        let opts = driver::Opts {
            compiler_version: clap::crate_version!().to_string(),
            project_dir,
        };

        driver::build(opts);
    } else if let Some(matches) = matches.subcommand_matches("run") {
        let project_dir = matches.value_of("input").unwrap().into();
        let opts = driver::Opts {
            compiler_version: clap::crate_version!().to_string(),
            project_dir,
        };

        driver::run(opts);
    } else if let Some(matches) = matches.subcommand_matches("new") {
        let name = matches.value_of("name").unwrap();
        let manifest = source::opts::Manifest {
            package: source::opts::Package {
                name: name.into(),
                version: "0.1.0".into(),
                authors: None,
                src_dir: None,
                target_dir: Default::default(),
                target: Default::default(),
            },
            dependencies: Default::default(),
        };

        std::fs::create_dir(format!("./{}", name)).unwrap();
        std::fs::create_dir(format!("./{}/src", name)).unwrap();

        let manifest = toml::to_string_pretty(&manifest).unwrap();

        std::fs::write(format!("./{}/shadow.toml", name), manifest).unwrap();
    } else {
        unreachable!();
    }
}
