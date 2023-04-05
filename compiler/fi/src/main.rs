#![feature(iterator_try_collect)]

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
    #[arg(value_hint = clap::ValueHint::FilePath)]
    files: Vec<PathBuf>,

    #[arg(short = 'o', value_hint = clap::ValueHint::FilePath)]
    output: Option<PathBuf>,
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

    let mut driver = Driver::default();
    let mut files = Vec::with_capacity(args.files.len());

    for file in args.files {
        let str = file.as_os_str().to_str().unwrap();
        let mut glob = glob::glob(str)?;

        files.append(&mut glob.try_collect()?);
    }

    let files = driver.load_files(files)?;
    let lib = driver.create_lib(files);

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
    }

    driver.build(lib);
    Ok(())
}

fn run(_args: RunArgs, _driver: Driver, _lib: LibId) -> anyhow::Result<()> {
    Ok(())
}
