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

    reporter.report(true);

    let package = hir::convert::convert(&reporter, &package);

    reporter.report(true);

    check::with_tcx(&reporter, &package, |tcx| {
        for (id, item) in &package.items {
            let ty = tcx.type_of(id);

            println!("{}: {};", item.name, ty);
        }
    });
}
