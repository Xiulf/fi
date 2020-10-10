pub mod opts;

use diagnostics::{Diagnostic, FileId, Position, Reporter, Severity, Span};
pub use opts::*;
use std::path::{Path, PathBuf};

pub struct BuildFiles {
    pub manifest: Manifest,
    pub bin: PathBuf,
    pub modules: Vec<PathBuf>,
    pub exports: PathBuf,
    pub typemap: PathBuf,
}

pub fn build(mut opts: Opts) -> BuildFiles {
    opts.project_dir = normalize(opts.project_dir);

    let reporter = Reporter::default();
    let manifest = get_manifest(&reporter, &opts.project_dir);
    let exports_dir = format!(
        "{}/{}.shade-exports",
        manifest.package.target_dir.display(),
        manifest.package.name
    )
    .into();

    let types_dir = format!(
        "{}/{}.shade-types",
        manifest.package.target_dir.display(),
        manifest.package.name
    )
    .into();

    let mut bin_dir = PathBuf::from(format!(
        "{}/{}",
        manifest.package.target_dir.display(),
        manifest.package.name
    ));

    bin_dir.set_extension(opts.out_type.extension(&opts.target));

    let dep_files = build_deps(&opts, &manifest);

    println!(
        "\x1B[1m\x1B[32mCompiling\x1B[0m {} {}({})",
        manifest.package.name,
        if let Some(v) = &manifest.package.version {
            format!("v{} ", v)
        } else {
            String::new()
        },
        opts.project_dir.display()
    );

    let package = parse_all(&reporter, manifest.package.src_dir.as_ref().unwrap());

    reporter.report(true);

    // println!("{}", package);

    let (hir, meta) = hir::convert::convert(
        &reporter,
        &package,
        &manifest.package.name,
        dep_files
            .iter()
            .flat_map(|d| d.modules.iter().map(|d| d.as_path())),
        dep_files.iter().map(|d| d.exports.as_ref()),
    );

    let exports = hir.collect_exports();

    reporter.report(true);
    exports.store(&exports_dir);

    let meta_files = meta
        .iter()
        .map(|meta| {
            let path = PathBuf::from(format!(
                "{}/{}.shade-module",
                manifest.package.target_dir.display(),
                meta.name,
            ));

            meta.store(&path);

            path
        })
        .collect();

    // println!("{}", hir);
    // println!("{:#?}", module_structure);

    check::with_tcx(
        &reporter,
        &hir,
        &opts.target,
        dep_files.iter().map(|d| d.typemap.as_ref()),
        |tcx| {
            let mir = mir::convert::convert(&tcx, &hir);

            println!("{}", mir);

            tcx.store_type_map(&types_dir);

            codegen::compile(
                &tcx,
                &mir,
                &opts.target,
                opts.out_type,
                &bin_dir,
                dep_files.iter().map(|d| d.bin.as_ref()),
            );
        },
    );

    BuildFiles {
        manifest,
        bin: bin_dir,
        modules: meta_files,
        exports: exports_dir,
        typemap: types_dir,
    }
}

pub fn get_manifest(reporter: &Reporter, project_dir: &Path) -> Manifest {
    let manifest_path = format!("{}/shadow.toml", project_dir.display());
    let manifest_src = match std::fs::read_to_string(&manifest_path) {
        Ok(s) => s,
        Err(_) => {
            reporter.add(Diagnostic::new(
                Severity::Error,
                0017,
                format!("no shadow.toml file found in {}", manifest_path),
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

    match &mut manifest.package.src_dir {
        Some(entry) => {
            if entry.is_relative() {
                *entry = format!("{}/{}", project_dir.display(), entry.display()).into();
            }
        }
        None => {
            let path = format!("{}/src", project_dir.display(),);

            manifest.package.src_dir = Some(path.into());
        }
    }

    manifest.package.target_dir = format!("{}/target", project_dir.display()).into();
    manifest
}

pub fn parse_all(reporter: &Reporter, src_dir: &Path) -> syntax::ast::Package {
    let mut package = syntax::ast::Package {
        modules: Vec::new(),
    };

    for entry in src_dir.read_dir().unwrap() {
        let entry = entry.unwrap().path();

        if entry.is_dir() {
            let mut pkg = parse_all(reporter, &entry);

            package.modules.append(&mut pkg.modules);
        } else if entry.extension().and_then(|s| s.to_str()) == Some("shade") {
            let source = std::fs::read_to_string(&entry).unwrap();
            let file = diagnostics::FileId::new(entry, source);
            let mut pkg = syntax::parse(reporter, file);

            package.modules.append(&mut pkg.modules);
        }
    }

    package
}

pub fn build_deps(opts: &Opts, manifest: &Manifest) -> Vec<BuildFiles> {
    manifest
        .dependencies
        .iter()
        .map(|(_, dep)| match dep {
            Dependency::Path { path } => build_dep(opts, path),
        })
        .collect()
}

pub fn build_dep(opts: &Opts, dir: &Path) -> BuildFiles {
    let opts = Opts {
        project_dir: opts.project_dir.join(dir),
        target: opts.target.clone(),
        out_type: OutputType::DyLib,
    };

    build(opts)
}

fn normalize(path: impl AsRef<Path>) -> PathBuf {
    use std::path::Component;
    let mut buf = PathBuf::new();

    for c in path.as_ref().components() {
        match c {
            Component::CurDir => continue,
            Component::ParentDir => {
                buf.pop();
                continue;
            }
            _ => buf.push(c),
        }
    }

    buf
}
