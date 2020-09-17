pub mod opts;

use diagnostics::{Diagnostic, FileId, Position, Severity, Span};
pub use opts::*;

pub fn build(opts: Opts) {
    let reporter = diagnostics::Reporter::default();
    let manifest_path = format!("{}/shadow.toml", opts.project_dir.display());
    let manifest_src = match std::fs::read_to_string(&manifest_path) {
        Ok(s) => s,
        Err(_) => {
            reporter.add(Diagnostic::new(
                Severity::Error,
                0017,
                "no shadow.toml file found",
            ));
            reporter.report(true);
            unreachable!();
        }
    };

    let manifest_file = FileId::new(manifest_path, manifest_src);
    let mut manifest: Manifest = match toml::from_str(&manifest_file.source) {
        Ok(m) => m,
        Err(e) => {
            let span = if let Some((line, col)) = e.line_col() {
                let pos = Position {
                    offset: 0,
                    line,
                    col,
                };

                Span {
                    file: manifest_file,
                    start: pos,
                    end: pos,
                }
            } else {
                Span::empty(manifest_file)
            };

            reporter.add(
                Diagnostic::new(Severity::Error, 0018, "invalid shadow.toml file").label(
                    Severity::Error,
                    span,
                    e.to_string(),
                ),
            );
            reporter.report(true);
            unreachable!();
        }
    };

    match &mut manifest.package.entry {
        Some(entry) => {
            if entry.is_relative() {
                *entry = format!("{}/{}", opts.project_dir.display(), entry.display()).into();
            }

            entry.set_extension("shade");
        }
        None => {
            let path = format!(
                "{}/src/{}.shade",
                opts.project_dir.display(),
                manifest.package.name
            );

            manifest.package.entry = Some(path.into());
        }
    }

    manifest.package.target_dir = format!("{}/target", opts.project_dir.display()).into();

    let source = std::fs::read_to_string(manifest.package.entry.as_ref().unwrap()).unwrap();
    let file = diagnostics::FileId::new(manifest.package.entry.as_ref().unwrap(), source);
    let package = syntax::parse(&reporter, file);

    reporter.report(true);

    // println!("{}", package);

    let (hir, module_structure) = hir::convert::convert(&reporter, &package);

    reporter.report(true);

    // println!("{}", hir);
    // println!("{:#?}", module_structure);

    check::with_tcx(&reporter, &hir, &module_structure, &opts.target, |tcx| {
        let mir = mir::convert::convert(&tcx, &hir);

        // println!("{}", mir);

        tcx.store_type_map(format!(
            "{}/{}.shade-types",
            manifest.package.target_dir.display(),
            manifest.package.name
        ));

        codegen::compile(
            &tcx,
            &mir,
            &opts.target,
            opts.out_type,
            format!(
                "{}/{}",
                manifest.package.target_dir.display(),
                manifest.package.name
            ),
        );
    });
}
