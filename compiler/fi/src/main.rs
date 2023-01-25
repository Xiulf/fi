use std::path::PathBuf;

use clap::Parser;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Cli {
    #[arg(value_hint = clap::ValueHint::FilePath)]
    files: Vec<PathBuf>,

    #[arg(short = 'o', value_hint = clap::ValueHint::FilePath)]
    output: Option<PathBuf>,
}

fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();
    let mut driver = driver::Driver::default();
    let files = driver.load_files(cli.files)?;
    let lib = driver.create_lib(files);

    driver.report_diagnostics(lib)?;
    driver.debug(lib);

    Ok(())
}
