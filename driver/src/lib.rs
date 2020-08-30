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

    // println!("{}", package);

    let hir = hir::convert::convert(&reporter, &package);

    reporter.report(true);

    // println!("{}", hir);

    check::with_tcx(&reporter, &hir, |tcx| {
        let mir = mir::convert::convert(&tcx, &hir);

        println!("{}", mir);

        // codegen::compile(&tcx, &mir, format!("{}/main", opts.target_dir.display()));
    });
}
