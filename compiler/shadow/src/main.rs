#![feature(str_split_as_str)]

mod interactive;

use std::io;
use std::path::{Path, PathBuf};

use base_db::libs::{LibId, LibKind};
use clap::{Args, Parser, Subcommand};
use driver::manifest::{Cfg, TomlValue};
use driver::{Driver, Opts};
use tracing::{debug, Level};

#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
#[clap(propagate_version = true)]
struct Cli {
    #[clap(flatten)]
    args: CliArgs,

    #[clap(subcommand)]
    command: Option<Commands>,
}

#[derive(Args, Debug)]
struct CliArgs {
    #[clap(value_hint = clap::ValueHint::FilePath)]
    file: Option<PathBuf>,

    #[clap(long)]
    target: Option<String>,

    #[clap(long, value_parser = parse_output)]
    output: Option<LibKind>,

    #[clap(long, global = true, value_parser = parse_cfg)]
    cfg: Vec<(String, TomlValue)>,

    #[clap(short = 'v', long, global = true, action = clap::ArgAction::Count)]
    verbose: u8,
}

#[derive(Subcommand, Debug, Clone)]
enum BasicCommands {
    Check(CheckArgs),
    Build(BuildArgs),

    #[clap(trailing_var_arg = true)]
    Run(RunArgs),
}

#[derive(Subcommand, Debug)]
enum Commands {
    #[clap(flatten)]
    Basic(BasicCommands),
    Watch(WatchArgs),
    Lsp(LspArgs),
}

#[derive(Args, Debug, Clone)]
struct CheckArgs {
    #[clap(default_value = ".", value_hint = clap::ValueHint::DirPath)]
    input: PathBuf,
}

#[derive(Args, Debug, Clone)]
struct BuildArgs {
    #[clap(default_value = ".", value_hint = clap::ValueHint::DirPath)]
    input: PathBuf,
}

#[derive(Args, Debug, Clone)]
struct RunArgs {
    #[clap(default_value = ".", value_hint = clap::ValueHint::DirPath)]
    input: PathBuf,
    args: Vec<String>,
}

#[derive(Args, Debug)]
struct WatchArgs {
    #[clap(subcommand)]
    command: BasicCommands,
}

#[derive(Args, Debug)]
struct LspArgs {
    #[clap(default_value = ".", value_hint = clap::ValueHint::DirPath)]
    input: PathBuf,
}

fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();

    tracing_subscriber::fmt()
        .without_time()
        .with_max_level(match cli.args.verbose {
            | 0 => Level::WARN,
            | 1 => Level::INFO,
            | 2 => Level::DEBUG,
            | _ => Level::TRACE,
        })
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

    run_cli(cli)
}

fn run_cli(cli: Cli) -> anyhow::Result<()> {
    let Cli { args, command } = cli;

    match command {
        | Some(command) => match command {
            | Commands::Basic(command) => match setup_basic(args, &command) {
                | Some((driver, lib)) => run_basic(&driver, lib, &command).map(|_| ()).map_err(Into::into),
                | None => Ok(()),
            },
            | Commands::Watch(watch) => run_watch(args, watch),
            | Commands::Lsp(lsp) => run_lsp(args, lsp),
        },
        | None => match args.file.clone() {
            | Some(file) => run_file(args, file).map_err(Into::into),
            | None => {
                interactive::run();
                Ok(())
            },
        },
    }
}

fn setup_basic(cli: CliArgs, command: &BasicCommands) -> Option<(Driver, LibId)> {
    let cfg: Cfg = cli.cfg.into_iter().collect();
    let input = match &command {
        | BasicCommands::Check(args) => &args.input,
        | BasicCommands::Build(args) => &args.input,
        | BasicCommands::Run(args) => &args.input,
    };

    Driver::init(Opts {
        target: cli.target.as_deref(),
        input,
        cfg,
        ..Opts::default()
    })
}

fn run_basic(driver: &Driver, lib: LibId, command: &BasicCommands) -> io::Result<bool> {
    match command {
        | BasicCommands::Check(_) => driver.check(),
        | BasicCommands::Build(_) => driver.build(),
        | BasicCommands::Run(args) => driver.run(lib, args.args.iter()),
    }
}

fn run_file(cli: CliArgs, input: PathBuf) -> io::Result<()> {
    let cfg: Cfg = cli.cfg.into_iter().collect();

    if let Some((driver, _)) = Driver::init_no_manifest(Opts {
        input: &input,
        target: cli.target.as_deref(),
        output: cli.output,
        cfg,
    }) {
        driver.build()?;
    }

    Ok(())
}

fn run_lsp(_cli: CliArgs, _args: LspArgs) -> anyhow::Result<()> {
    language_server::run()
}

fn run_watch(cli: CliArgs, args: WatchArgs) -> anyhow::Result<()> {
    use notify::event::{CreateKind, EventKind, RemoveKind};
    use notify::Watcher;

    if let Some((_driver, _lib)) = setup_basic(cli, &args.command) {
        let input = match &args.command {
            | BasicCommands::Check(args) => &args.input,
            | BasicCommands::Build(args) => &args.input,
            | BasicCommands::Run(args) => &args.input,
        };

        let (tx, rx) = std::sync::mpsc::channel();
        let mut watcher = notify::recommended_watcher(tx)?;

        watcher.watch(Path::new(input), notify::RecursiveMode::Recursive)?;

        for res in rx {
            if let Ok(event) = res {
                match event.kind {
                    | EventKind::Create(CreateKind::File) => {
                        debug!("added: {:?}", event.paths);
                    },
                    | EventKind::Remove(RemoveKind::File) => {
                        debug!("removed: {:?}", event.paths);
                    },
                    | EventKind::Modify(_) => {
                        debug!("modified: {:?}", event.paths);
                    },
                    | _ => continue,
                }
            }
        }
    }

    Ok(())
}

fn parse_output(s: &str) -> Result<LibKind, String> {
    match s {
        | "dynamic" => Ok(LibKind::Dynamic),
        | "static" => Ok(LibKind::Static),
        | "executable" => Ok(LibKind::Executable),
        | _ => Err(format!("invalid output kind '{}'", s)),
    }
}

fn parse_cfg(s: &str) -> Result<(String, TomlValue), String> {
    if let Some((key, value)) = s.split_once('=') {
        let value = if let Ok(i) = value.parse::<i64>() {
            TomlValue::Integer(i)
        } else if let Ok(f) = value.parse::<f64>() {
            TomlValue::Float(f)
        } else {
            TomlValue::String(value.into())
        };

        Ok((key.into(), value))
    } else {
        Ok((s.into(), TomlValue::Boolean(true)))
    }
}
