use codespan::FileId;

pub use codespan_reporting::term::{
    emit,
    termcolor::{ColorChoice, StandardStream},
    Config,
};

pub use codespan_reporting::diagnostic::Severity;
pub type Diagnostic = codespan_reporting::diagnostic::Diagnostic<FileId>;
pub type Label = codespan_reporting::diagnostic::Label<FileId>;

pub trait Diagnostics: ToDiagDb {
    fn report(&self, diag: Diagnostic);
    fn report_all(&self, diags: Vec<Diagnostic>);
    fn has_errors(&self) -> bool;
    fn print(&self);
    fn print_and_exit(&self) -> !;
}

pub trait ToDiagDb {
    fn to_diag_db(&self) -> &dyn Diagnostics;
}

impl<T: Diagnostics> ToDiagDb for T {
    fn to_diag_db(&self) -> &dyn Diagnostics {
        self
    }
}

pub struct DiagnosticBuilder<'rep> {
    rep: &'rep dyn Diagnostics,
    diag: Diagnostic,
    labels: Vec<Label>,
    notes: Vec<String>,
}

pub struct UnsafeReporter<'a, F>
where
    F: codespan_reporting::files::Files<'a, FileId = FileId, Name = String, Source = &'a str>,
{
    diags: std::cell::RefCell<Vec<Diagnostic>>,
    files: *const F,
    _marker: std::marker::PhantomData<&'a ()>,
}

impl dyn Diagnostics + '_ {
    pub fn bug(&self, message: impl Into<String>) -> DiagnosticBuilder<'_> {
        DiagnosticBuilder {
            rep: self,
            diag: Diagnostic::bug().with_message(message),
            labels: Vec::new(),
            notes: Vec::new(),
        }
    }

    pub fn error(&self, message: impl Into<String>) -> DiagnosticBuilder<'_> {
        DiagnosticBuilder {
            rep: self,
            diag: Diagnostic::error().with_message(message),
            labels: Vec::new(),
            notes: Vec::new(),
        }
    }

    pub fn warning(&self, message: impl Into<String>) -> DiagnosticBuilder<'_> {
        DiagnosticBuilder {
            rep: self,
            diag: Diagnostic::warning().with_message(message),
            labels: Vec::new(),
            notes: Vec::new(),
        }
    }

    pub fn note(&self, message: impl Into<String>) -> DiagnosticBuilder<'_> {
        DiagnosticBuilder {
            rep: self,
            diag: Diagnostic::note().with_message(message),
            labels: Vec::new(),
            notes: Vec::new(),
        }
    }

    pub fn help(&self, message: impl Into<String>) -> DiagnosticBuilder<'_> {
        DiagnosticBuilder {
            rep: self,
            diag: Diagnostic::help().with_message(message),
            labels: Vec::new(),
            notes: Vec::new(),
        }
    }
}

impl DiagnosticBuilder<'_> {
    pub fn with_code(self, code: impl Into<String>) -> Self {
        DiagnosticBuilder {
            diag: self.diag.with_code(code),
            ..self
        }
    }

    pub fn with_label(mut self, label: Label) -> Self {
        self.labels.push(label);
        self
    }

    pub fn with_note(mut self, note: impl Into<String>) -> Self {
        self.notes.push(note.into());
        self
    }

    pub fn finish(self) {
        let DiagnosticBuilder {
            rep,
            diag,
            labels,
            notes,
        } = self;

        rep.report(diag.with_labels(labels).with_notes(notes));
    }
}

impl<'a, F> UnsafeReporter<'a, F>
where
    F: codespan_reporting::files::Files<'a, FileId = FileId, Name = String, Source = &'a str>,
{
    pub fn new(files: &F) -> Self {
        UnsafeReporter {
            files,
            diags: Default::default(),
            _marker: std::marker::PhantomData,
        }
    }
}

impl<'a, F> Diagnostics for UnsafeReporter<'a, F>
where
    F: codespan_reporting::files::Files<'a, FileId = FileId, Name = String, Source = &'a str>,
    F: 'a,
{
    fn report(&self, diag: Diagnostic) {
        self.diags.borrow_mut().push(diag);
    }

    fn report_all(&self, mut diags: Vec<Diagnostic>) {
        self.diags.borrow_mut().append(&mut diags);
    }

    fn has_errors(&self) -> bool {
        self.diags.borrow().iter().any(|d| match d.severity {
            Severity::Bug => true,
            Severity::Error => true,
            _ => false,
        })
    }

    fn print(&self) {
        let mut stream = StandardStream::stderr(ColorChoice::Always);
        let config = Config::default();

        for diag in self.diags.borrow_mut().drain(..) {
            emit(&mut stream, &config, unsafe { &*self.files }, &diag).unwrap();
        }
    }

    fn print_and_exit(&self) -> ! {
        self.print();
        std::process::exit(1);
    }
}
