pub mod db;
pub mod metadata;

use relative_path::RelativePath;
use source::SourceDatabase;
use std::path::{Path, PathBuf};

pub struct Opts {
    pub project_dir: PathBuf,
}

pub struct RunResult {
    pub lib: source::LibId,
    pub changed: bool,
    pub db: db::CompilerDatabase,
}

pub fn run(opts: Opts) -> RunResult {
    let mut db = db::CompilerDatabase::default();
    let mut files = source::Files::new();
    let mut lib_files = Vec::new();
    let manifest = source::opts::Manifest::load(&diagnostics::UnsafeReporter::new(&files), &mut files, &opts.project_dir);
    let lib = source::LibId::new(&manifest.package.name, &manifest.package.version);
    let mut libs = vec![lib];
    let mut deps = Vec::new();
    let mut deps_changed = false;

    for (_name, dep) in &manifest.dependencies {
        match dep {
            | source::opts::Dependency::Path { path } => {
                let opts = if path.is_relative() {
                    Opts {
                        project_dir: RelativePath::from_path(&opts.project_dir.join(path)).unwrap().normalize().to_string().into(),
                    }
                } else {
                    Opts { project_dir: path.clone() }
                };

                let res = run(opts);

                deps.push(res.lib);
                deps_changed |= res.changed;
                libs.extend(res.db.libs());
                db.set_manifest(res.lib, res.db.manifest(res.lib));
            },
        }
    }

    let start = std::time::Instant::now();
    let _ = std::fs::create_dir_all(format!("{}/meta/items", manifest.package.target_dir.display()));
    let _ = std::fs::create_dir_all(format!("{}/meta/types", manifest.package.target_dir.display()));

    register_files(&mut db, &mut files, &mut lib_files, lib, manifest.package.src_dir.as_ref().unwrap()).unwrap();

    db.set_manifest(lib, std::sync::Arc::new(manifest));
    db.set_files(std::sync::Arc::new(files));
    db.set_lib_files(lib, std::sync::Arc::new(lib_files));
    db.set_libs(libs);
    db.set_deps(lib, deps);
    db.set_lib(lib);

    if let Some(meta) = db.load_metadata(lib) {
        let none_changed = meta.last_modified.iter().all(|(path, last)| {
            let modified = match path.metadata() {
                | Ok(m) => match m.modified() {
                    | Ok(m) => m,
                    | Err(_) => return false,
                },
                | Err(_) => return false,
            };

            modified == *last
        });

        if none_changed && meta.last_modified.len() == db.lib_files(lib).len() && !deps_changed {
            let elapsed = start.elapsed();

            println!("\x1B[1;32m\x1B[1mCompiled\x1B[0m {} in {:?}", db.manifest(lib).package.name, elapsed);

            return RunResult { lib, db, changed: false };
        }
    }

    // for mdata in db.module_tree(lib).toposort(&db) {
    //     // use typeck::{display::Typed, TypeDatabase};
    //     // let hir = db.module_hir(mdata.file);
    //     //
    //     // for (_, item) in &hir.items {
    //     //     if let hir::ir::ItemKind::Func { body, .. } = &item.kind {
    //     //         let types = db.typecheck(item.id.owner);
    //     //
    //     //         println!("{} :: {}", item.name, Typed(&db, &(), &types.ty));
    //     //         println!("{} {}", item.name, Typed(&db, &types.tys, &hir.bodies[body]));
    //     //     }
    //     // }
    //     db.assembly(lib, mdata.id);
    // }

    db.store_metadata(lib);

    let elapsed = start.elapsed();

    println!("\x1B[1;32m\x1B[1mCompiled\x1B[0m {} in {:?}", db.manifest(lib).package.name, elapsed);

    RunResult { lib, db, changed: true }
}

fn register_files(
    db: &mut impl source::SourceDatabase,
    files: &mut source::Files,
    lib_files: &mut Vec<source::FileId>,
    lib: source::LibId,
    src: impl AsRef<Path>,
) -> std::io::Result<()> {
    for entry in src.as_ref().read_dir()? {
        let path = entry?.path();

        if path.is_dir() {
            register_files(db, files, lib_files, lib, path)?;
        } else {
            let source = std::fs::read_to_string(&path)?;
            let id = files.add(path, source.into());

            db.set_file_lib(id, lib);
            lib_files.push(id);
        }
    }

    Ok(())
}
