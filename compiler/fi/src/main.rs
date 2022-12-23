#![feature(str_split_as_str)]

mod interactive;

use std::io;
use std::path::{Path, PathBuf};
use std::process::ExitCode;

use base_db::libs::LibKind;
use base_db::{Error, ICE};
use clap::{Args, Parser, Subcommand};
use driver::{Driver, InitNoManifestOpts, InitOpts, Optimization};
use project::manifest::{Cfg, TomlValue};
use tracing::{debug, Level};
use tracing_subscriber::EnvFilter;

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
    #[clap(requires = "name", value_hint = clap::ValueHint::FilePath)]
    files: Vec<PathBuf>,

    #[clap(long, short, requires = "files")]
    name: Option<String>,

    #[clap(long, requires = "files")]
    target: Option<String>,

    #[clap(long, requires = "files", value_parser = parse_output)]
    output: Option<LibKind>,

    #[clap(short = 'O', value_parser = parse_optimization)]
    optimization: Option<Optimization>,

    #[clap(long = "link", short, requires = "files")]
    links: Vec<PathBuf>,

    #[clap(long = "dep", short, requires = "files")]
    dependencies: Vec<PathBuf>,

    #[clap(long, global = true, value_parser = parse_cfg)]
    cfg: Vec<(String, TomlValue)>,

    #[clap(short = 'v', long, global = true, action = clap::ArgAction::Count)]
    verbose: u8,

    #[clap(long = "log-filter", global = true)]
    log_filter: Option<String>,
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

    #[clap(last = true)]
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

fn main() -> anyhow::Result<ExitCode> {
    let cli = Cli::parse();
    let filter = cli
        .args
        .log_filter
        .as_ref()
        .map(|s| EnvFilter::from(s))
        .unwrap_or_default()
        .add_directive(
            match cli.args.verbose {
                | 0 => Level::WARN,
                | 1 => Level::INFO,
                | 2 => Level::DEBUG,
                | _ => Level::TRACE,
            }
            .into(),
        );

    let _guard = if let Some(Commands::Lsp(_)) = cli.command {
        let tmp_dir = std::env::temp_dir();
        eprintln!("logging to {}", tmp_dir.join("shadow-lsp.log").display());
        let appender = tracing_appender::rolling::never(tmp_dir, "shadow-lsp.log");
        let (non_blocking, guard) = tracing_appender::non_blocking(appender);

        tracing_subscriber::fmt()
            .without_time()
            .with_ansi(false)
            .with_env_filter(filter)
            .with_writer(non_blocking)
            .init();

        Some(guard)
    } else {
        tracing_subscriber::fmt().without_time().with_env_filter(filter).init();
        None
    };

    std::panic::set_hook(Box::new(|info| {
        let loc = info.location().unwrap();

        if let Some(ice) = info.payload().downcast_ref::<ICE>() {
            eprintln!("\x1B[31mInternal Compiler Error:\x1B[0m '{}' at {}", ice.0, loc);
            return;
        }

        if let Some(err) = info.payload().downcast_ref::<Error>() {
            eprintln!("\x1B[31mError:\x1B[0m '{}'", err.0);
            return;
        }

        let msg = match info.payload().downcast_ref::<&'static str>() {
            | Some(s) => *s,
            | None => match info.payload().downcast_ref::<String>() {
                | Some(s) => &s[..],
                | None => "...",
            },
        };

        eprintln!("\x1B[31mInternal Compiler Error:\x1B[0m '{}' at {}", msg, loc);
    }));

    run_cli(cli)
}

fn run_cli(cli: Cli) -> anyhow::Result<ExitCode> {
    let Cli { args, command } = cli;

    match command {
        | Some(command) => match command {
            | Commands::Basic(command) => {
                let (driver, ws) = setup_basic(args, &command)?;
                run_basic(&driver, ws, &command).map_err(Into::into)
            },
            | Commands::Watch(watch) => run_watch(args, watch).map(|_| ExitCode::SUCCESS),
            | Commands::Lsp(lsp) => run_lsp(args, lsp).map(|_| ExitCode::SUCCESS),
        },
        | None if !args.files.is_empty() => run_files(args),
        | None => {
            interactive::run();
            Ok(ExitCode::SUCCESS)
        },
    }
}

fn setup_basic(cli: CliArgs, command: &BasicCommands) -> anyhow::Result<(Driver, usize)> {
    let cfg: Cfg = cli.cfg.into_iter().collect();
    let input = match &command {
        | BasicCommands::Check(args) => &args.input,
        | BasicCommands::Build(args) => &args.input,
        | BasicCommands::Run(args) => &args.input,
    };

    Driver::init(InitOpts {
        target: cli.target.as_deref(),
        input,
        cfg,
        ..InitOpts::default()
    })
}

fn run_basic(driver: &Driver, ws: usize, command: &BasicCommands) -> io::Result<ExitCode> {
    match command {
        | BasicCommands::Check(_) => {
            if driver.check()? {
                Ok(ExitCode::SUCCESS)
            } else {
                Ok(ExitCode::FAILURE)
            }
        },
        | BasicCommands::Build(_) => {
            if driver.build(ws)? {
                Ok(ExitCode::SUCCESS)
            } else {
                Ok(ExitCode::FAILURE)
            }
        },
        | BasicCommands::Run(args) => driver.run(ws, args.args.iter()),
    }
}

fn run_files(cli: CliArgs) -> anyhow::Result<ExitCode> {
    let cfg: Cfg = cli.cfg.into_iter().collect();
    let name = cli.name.unwrap();
    let output = cli.output.unwrap_or_default();
    let optimization = cli.optimization.unwrap_or_default();
    let (driver, ws) = Driver::init_without_manifest(InitNoManifestOpts {
        name: &name,
        target: cli.target.as_deref(),
        files: cli.files.iter().map(|p| p.as_path()).collect(),
        links: cli.links.iter().map(|p| p.as_path()).collect(),
        dependencies: cli.dependencies.iter().map(|p| p.as_path()).collect(),
        optimization,
        output,
        cfg,
    })?;

    if driver.build(ws)? {
        Ok(ExitCode::SUCCESS)
    } else {
        Ok(ExitCode::FAILURE)
    }
}

fn run_lsp(_cli: CliArgs, _args: LspArgs) -> anyhow::Result<()> {
    language_server::run()
}

fn run_watch(cli: CliArgs, args: WatchArgs) -> anyhow::Result<()> {
    use notify::event::{CreateKind, EventKind, RemoveKind};
    use notify::Watcher;

    if let Ok(_driver) = setup_basic(cli, &args.command) {
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

fn parse_optimization(s: &str) -> Result<Optimization, String> {
    match s.to_lowercase().as_str() {
        | "0" | "none" => Ok(Optimization::None),
        | _ => Err(format!("`{}` isn't a valid optimization", s)),
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
