use source::SourceDatabase;
use std::cell::RefCell;

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
}

impl salsa::Database for CompilerDatabase {
}

impl typeck::InferDb for CompilerDatabase {
    fn to_ty_db(&self) -> &dyn typeck::TypeDatabase {
        self
    }

    fn to_hir_db(&self) -> &dyn hir::HirDatabase {
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
            | diagnostics::Severity::Bug => true,
            | diagnostics::Severity::Error => true,
            | _ => false,
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
