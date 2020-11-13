use codespan::FileId;

pub use codespan_reporting::term::{
    emit,
    termcolor::{ColorChoice, StandardStream},
    Config,
};

pub type Diagnostic = codespan_reporting::diagnostic::Diagnostic<FileId>;
pub type Label = codespan_reporting::diagnostic::Label<FileId>;

pub trait Diagnostics {
    fn report(&self, diag: Diagnostic);
    fn report_all(&self, diags: Vec<Diagnostic>);
    fn print(&self);
    fn print_and_exit(&self) -> !;
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

pub struct DiagnosticBuilder<'rep> {
    rep: &'rep dyn Diagnostics,
    diag: Diagnostic,
    labels: Vec<Label>,
    notes: Vec<String>,
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
