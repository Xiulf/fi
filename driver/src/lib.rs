use std::path::PathBuf;

#[derive(Default, Debug)]
pub struct Opts {
    pub entry: PathBuf,
    pub target_dir: PathBuf,
}

pub fn build(opts: Opts) {
    let reporter = diagnostics::Reporter::default();
    let source = std::fs::read_to_string(&opts.entry).unwrap();
    let file = diagnostics::FileId::new(&opts.entry, source);
    let package = syntax::parse(&reporter, file);

    println!("{:#?}", package);
}
