use backend::BackendDatabase;
use hir::HirDatabase;
use source::SourceDatabase;
use std::cell::{Cell, RefCell};
use std::path::Path;

#[salsa::database(
    source::SourceDatabaseStorage,
    syntax::SyntaxDatabaseStorage,
    hir::HirDatabaseStorage,
    typeck::TypeDatabaseStorage,
    lower::LowerDatabaseStorage,
    backend::BackendDatabaseStorage
)]
#[derive(Default)]
pub struct CompilerDatabase {
    storage: salsa::Storage<Self>,
    diags: RefCell<Vec<diagnostics::Diagnostic>>,
    lib_ids: u32,
    // infer_ids: Cell<u64>,
}

impl salsa::Database for CompilerDatabase {}

impl CompilerDatabase {
    fn next_lib(&mut self) -> source::LibId {
        let id = self.lib_ids;

        self.lib_ids += 1;
        source::LibId(id)
    }
}

impl typeck::InferDb for CompilerDatabase {
    fn to_ty_db(&self) -> &dyn typeck::TypeDatabase {
        self
    }
}

impl diagnostics::Diagnostics for CompilerDatabase {
    fn report(&self, diag: diagnostics::Diagnostic) {
        self.diags.borrow_mut().push(diag);
    }

    fn report_all(&self, mut diags: Vec<diagnostics::Diagnostic>) {
        self.diags.borrow_mut().append(&mut diags);
    }

    fn has_errors(&self) -> bool {
        self.diags.borrow().iter().any(|d| match d.severity {
            diagnostics::Severity::Bug => true,
            diagnostics::Severity::Error => true,
            _ => false,
        })
    }

    fn print(&self) {
        let mut stream = diagnostics::StandardStream::stderr(diagnostics::ColorChoice::Always);
        let config = diagnostics::Config::default();

        for diag in self.diags.borrow_mut().drain(..) {
            diagnostics::emit(&mut stream, &config, &*self.files(), &diag).unwrap();
        }
    }

    fn print_and_exit(&self) -> ! {
        self.print();
        std::process::exit(0);
    }
}

pub fn run() {
    let mut db = CompilerDatabase::default();
    let mut files = source::Files::new();
    let mut lib_files = Vec::new();
    let lib = db.next_lib();
    let manifest = source::opts::Manifest::load(
        &diagnostics::UnsafeReporter::new(&files),
        &mut files,
        &std::path::PathBuf::from("test"),
    );

    register_files(
        &mut db,
        &mut files,
        &mut lib_files,
        lib,
        manifest.package.src_dir.as_ref().unwrap(),
    )
    .unwrap();

    db.set_manifest(lib, std::sync::Arc::new(manifest));
    db.set_files(std::sync::Arc::new(files));
    db.set_lib_files(lib, std::sync::Arc::new(lib_files));
    db.set_libs(vec![lib]);

    for mdata in db.module_tree(lib).toposort(&db) {
        // db.assembly(lib, mdata.id);
        use typeck::{display::Typed, TypeDatabase};
        let hir = db.module_hir(mdata.file);
        
        for (_, item) in &hir.items {
            if let hir::ir::ItemKind::Func { body, .. } = &item.kind {
                let types = db.typecheck(item.id.owner);
        
                println!("{} :: {}", item.name, Typed(&db, &(), &types.ty));
                println!(
                    "{} {}",
                    item.name,
                    Typed(&db, &types.tys, &hir.bodies[body])
                );
            }
        }
    }
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
