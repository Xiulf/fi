use hir::HirDatabase;
use source::SourceDatabase;
use std::cell::RefCell;
use std::path::Path;

#[salsa::database(
    source::SourceDatabaseStorage,
    syntax::SyntaxDatabaseStorage,
    hir::HirDatabaseStorage
)]
#[derive(Default)]
pub struct CompilerDatabase {
    storage: salsa::Storage<Self>,
    diags: RefCell<Vec<diagnostics::Diagnostic>>,
    lib_ids: u32,
}

impl salsa::Database for CompilerDatabase {}

impl CompilerDatabase {
    fn next_lib(&mut self) -> source::LibId {
        let id = self.lib_ids;

        self.lib_ids += 1;
        source::LibId(id)
    }
}

impl diagnostics::Diagnostics for CompilerDatabase {
    fn report(&self, diag: diagnostics::Diagnostic) {
        self.diags.borrow_mut().push(diag);
    }

    fn report_all(&self, mut diags: Vec<diagnostics::Diagnostic>) {
        self.diags.borrow_mut().append(&mut diags);
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

    register_files(&mut db, &mut files, &mut lib_files, lib, "test/src").unwrap();
    db.set_files(std::sync::Arc::new(files));
    db.set_lib_files(lib, std::sync::Arc::new(lib_files));
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
