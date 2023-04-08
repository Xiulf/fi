#![feature(iterator_try_collect)]

use std::fmt::Write;
use std::path::PathBuf;

use base_db::libs::LibId;
use clap::{Args, Parser, Subcommand};
use driver::Driver;
use tracing::metadata::LevelFilter;
use tracing_subscriber::EnvFilter;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Cli {
    #[command(flatten)]
    args: CliArgs,

    #[command(subcommand)]
    command: Command,
}

#[derive(Args, Debug)]
struct CliArgs {}

#[derive(Subcommand, Debug)]
enum Command {
    #[command(flatten)]
    Basic(BasicCommand),
}

#[derive(Subcommand, Debug)]
enum BasicCommand {
    Check(CheckArgs),
    Build(BuildArgs),
    Run(RunArgs),
}

#[derive(Args, Debug, Clone)]
struct BasicCommandArgs {
    #[arg(required = true, value_hint = clap::ValueHint::FilePath)]
    files: Vec<PathBuf>,

    #[arg(long = "lib-name", required_unless_present("output"))]
    lib_name: Option<String>,

    #[arg(short = 'o', value_hint = clap::ValueHint::FilePath)]
    output: Option<PathBuf>,

    #[arg(short = 'O', long = "opt-level", default_value_t, value_parser = optimization_level_parser)]
    optimization_level: OptLevel,
}

#[derive(Args, Debug)]
struct CheckArgs {
    #[command(flatten)]
    basic: BasicCommandArgs,
}

#[derive(Args, Debug)]
struct BuildArgs {
    #[command(flatten)]
    basic: BasicCommandArgs,
}

#[derive(Args, Debug)]
struct RunArgs {
    #[command(flatten)]
    basic: BasicCommandArgs,
}

#[derive(Default, Debug, Clone)]
#[repr(u32)]
enum OptLevel {
    None,
    Less,
    #[default]
    Default,
    Aggressive,
}

fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();

    tracing_subscriber::fmt()
        .with_env_filter(
            EnvFilter::builder()
                .with_default_directive(LevelFilter::WARN.into())
                .with_env_var("FI_LOG")
                .from_env_lossy(),
        )
        .without_time()
        .with_line_number(true)
        .init();

    std::panic::set_hook(Box::new(|info| {
        let loc = info.location().unwrap();

        // if let Some(ice) = info.payload().downcast_ref::<ICE>() {
        //     eprintln!("\x1B[31mInternal Compiler Error:\x1B[0m '{}' at {}", ice.0, loc);
        //     return;
        // }

        // if let Some(err) = info.payload().downcast_ref::<Error>() {
        //     eprintln!("\x1B[31mError:\x1B[0m '{}'", err.0);
        //     return;
        // }

        let msg = match info.payload().downcast_ref::<&'static str>() {
            | Some(s) => *s,
            | None => match info.payload().downcast_ref::<String>() {
                | Some(s) => &s[..],
                | None => "...",
            },
        };

        eprintln!("\x1B[31mInternal Compiler Error:\x1B[0m '{}' at {}", msg, loc);
    }));
    match cli.command {
        | Command::Basic(cmd) => basic(cli.args, cmd),
    }
}

fn basic(_cli: CliArgs, cmd: BasicCommand) -> anyhow::Result<()> {
    let args = match &cmd {
        | BasicCommand::Check(args) => args.basic.clone(),
        | BasicCommand::Build(args) => args.basic.clone(),
        | BasicCommand::Run(args) => args.basic.clone(),
    };

    let options = driver::Options {
        optimization_level: args.optimization_level.into(),
        ..Default::default()
    };

    let mut driver = Driver::new(options);
    let mut files = Vec::with_capacity(args.files.len());

    for file in args.files {
        let str = file.as_os_str().to_str().unwrap();
        let mut glob = glob::glob(str)?;

        files.append(&mut glob.try_collect()?);
    }

    let files = driver.load_files(files)?;
    let lib_name = args.lib_name.as_deref().unwrap_or("");
    let lib = driver.create_lib(files, lib_name);

    match cmd {
        | BasicCommand::Check(args) => check(args, driver, lib),
        | BasicCommand::Build(args) => build(args, driver, lib),
        | BasicCommand::Run(args) => run(args, driver, lib),
    }
}

fn check(_args: CheckArgs, driver: Driver, lib: LibId) -> anyhow::Result<()> {
    if driver.report_diagnostics(lib)? {
        driver.debug(lib);
    }

    Ok(())
}

fn build(_args: BuildArgs, driver: Driver, lib: LibId) -> anyhow::Result<()> {
    if driver.report_diagnostics(lib)? {
        driver.debug(lib);
        driver.build(lib);
    }

    Ok(())
}

fn run(_args: RunArgs, _driver: Driver, _lib: LibId) -> anyhow::Result<()> {
    Ok(())
}

impl Into<driver::opts::OptimizationLevel> for OptLevel {
    fn into(self) -> driver::opts::OptimizationLevel {
        unsafe { std::mem::transmute(self) }
    }
}

impl std::fmt::Display for OptLevel {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            | Self::None => f.write_char('0'),
            | Self::Less => f.write_char('1'),
            | Self::Default => f.write_char('2'),
            | Self::Aggressive => f.write_char('3'),
        }
    }
}

fn optimization_level_parser(str: &str) -> Result<OptLevel, String> {
    match str {
        | "0" => Ok(OptLevel::None),
        | "1" => Ok(OptLevel::Less),
        | "2" => Ok(OptLevel::Default),
        | "3" => Ok(OptLevel::Aggressive),
        | _ => Err(format!("invalid optimization level")),
    }
}
