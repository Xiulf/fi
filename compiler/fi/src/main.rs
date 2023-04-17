#![feature(iterator_try_collect)]

use std::fmt::Write;
use std::path::PathBuf;

use base_db::libs::LibId;
use clap::error::ErrorKind;
use clap::{Args, CommandFactory, Parser, Subcommand};
use driver::Driver;
use paths::AbsPathBuf;
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
    #[arg(value_hint = clap::ValueHint::AnyPath)]
    files: Vec<PathBuf>,

    #[arg(long = "lib-name")]
    lib_name: Option<String>,

    #[arg(short = 'o', value_hint = clap::ValueHint::FilePath)]
    output: Option<PathBuf>,

    #[arg(short = 'D', long = "dependency", alias = "dep")]
    dependencies: Vec<String>,

    #[arg(short = 'O', long = "opt-level", default_value_t, value_parser = optimization_level_parser)]
    optimization_level: OptLevel,
}

#[derive(Debug, Clone)]
enum ProjectArgs {
    Files {
        files: Vec<PathBuf>,
        lib_name: Option<String>,
        output: Option<PathBuf>,
        dependencies: Vec<String>,
    },
    Project {
        dir: PathBuf,
    },
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

#[derive(Default, Debug, Clone, Copy)]
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

fn verify_basic_args(args: &BasicCommandArgs) -> anyhow::Result<ProjectArgs> {
    match (&args.output, &args.lib_name) {
        | (None, None) if args.files.len() == 1 && !args.files[0].join("fi.toml").exists() => {
            let mut cmd = Cli::command();
            cmd.error(
                ErrorKind::MissingRequiredArgument,
                "when compiling individual files, either 'lib-name' or 'output' must be present",
            )
            .exit();
        },
        | (None, None) if args.files.len() <= 1 => {
            let dir = args
                .files
                .get(0)
                .cloned()
                .map(Ok)
                .unwrap_or_else(|| std::env::current_dir())?;

            Ok(ProjectArgs::Project { dir })
        },
        | (_, _) => {
            let mut files = Vec::with_capacity(args.files.len());

            for file in &args.files {
                let str = file.as_os_str().to_str().unwrap();
                let mut glob = glob::glob(str)?;

                files.append(&mut glob.try_collect()?);
            }

            Ok(ProjectArgs::Files {
                files,
                lib_name: args.lib_name.clone(),
                output: args.output.clone(),
                dependencies: args.dependencies.clone(),
            })
        },
    }
}

fn basic(_cli: CliArgs, cmd: BasicCommand) -> anyhow::Result<()> {
    let args = match &cmd {
        | BasicCommand::Check(args) => &args.basic,
        | BasicCommand::Build(args) => &args.basic,
        | BasicCommand::Run(args) => &args.basic,
    };

    let options = driver::Options {
        optimization_level: args.optimization_level.into(),
        ..Default::default()
    };

    let mut driver = Driver::new(options);
    let project_args = verify_basic_args(args)?;
    let package = match project_args {
        | ProjectArgs::Files {
            files,
            lib_name,
            output,
            dependencies,
        } => {
            let files = files
                .into_iter()
                .map(|f| Ok::<_, std::io::Error>(AbsPathBuf::assert(f.canonicalize()?)))
                .try_collect()?;
            let lib_name = lib_name
                .as_deref()
                .or_else(|| output.as_ref()?.file_stem()?.to_str())
                .unwrap();
            let root_dir = AbsPathBuf::assert(std::env::current_dir()?);

            driver.load_files(files, lib_name.to_string(), Default::default(), dependencies, root_dir)?
        },
        | ProjectArgs::Project { dir } => {
            let dir = AbsPathBuf::assert(dir.canonicalize()?);
            driver.load_project(dir)?
        },
    };

    driver.finish_loading()?;

    let lib = driver.libs_for_package(package)[0];

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
