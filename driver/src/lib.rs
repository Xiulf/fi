use std::path::PathBuf;

#[derive(Default, Debug)]
pub struct Opts {
    pub entry: PathBuf,
    pub target: target_lexicon::Triple,
    pub target_dir: PathBuf,
}

pub fn build(opts: Opts) {
    let reporter = diagnostics::Reporter::default();
    let source = std::fs::read_to_string(&opts.entry).unwrap();
    let file = diagnostics::FileId::new(&opts.entry, source);
    let package = syntax::parse(&reporter, file);

    reporter.report(true);

    // println!("{}", package);

    let (hir, module_structure) = hir::convert::convert(&reporter, &package);

    reporter.report(true);

    // println!("{}", hir);
    // println!("{:#?}", module_structure);

    check::with_tcx(&reporter, &hir, &module_structure, &opts.target, |tcx| {
        let mir = mir::convert::convert(&tcx, &hir);

        let range = tcx.lang_items.range().unwrap();
        let path = tcx.get_full_name(&range);

        println!("{}", path);

        // println!("{}", mir);

        codegen::compile(
            &tcx,
            &mir,
            &opts.target,
            format!("{}/main", opts.target_dir.display()),
        );
    });
}
