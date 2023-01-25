use salsa::storage::HasJar;
use text_size::TextRange;
use vfs::in_file::InFile;

pub trait Db: vfs::Db + salsa::DbWithJar<Jar> {}

impl<T: ?Sized + vfs::Db + salsa::DbWithJar<Jar>> Db for T {
}

#[salsa::jar(db = Db)]
pub struct Jar(Diagnostics);

#[salsa::accumulator]
pub struct Diagnostics(Diagnostic);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Diagnostic {
    pub title: String,
    pub range: TextRange,
    pub level: Level,
    pub primary_label: Option<PrimaryLabel>,
    pub secondary_labels: Vec<SecondaryLabel>,
    pub notes: Vec<String>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Level {
    Error,
    Warning,
    Info,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PrimaryLabel {
    pub range: TextRange,
    pub message: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SecondaryLabel {
    pub range: InFile<TextRange>,
    pub message: String,
}

pub trait ToDiagnostic {
    type Db<'t>: ?Sized + 't;

    fn to_diagnostic(self, db: &Self::Db<'_>) -> Diagnostic;
}

impl ToDiagnostic for Diagnostic {
    type Db<'t> = dyn Db + 't;

    fn to_diagnostic(self, _: &Self::Db<'_>) -> Diagnostic {
        self
    }
}

impl<T: ToDiagnostic + Clone> ToDiagnostic for &T {
    type Db<'t> = T::Db<'t>;

    fn to_diagnostic(self, db: &Self::Db<'_>) -> Diagnostic {
        self.clone().to_diagnostic(db)
    }
}

impl Diagnostics {
    pub fn emit<'t, Db, T>(db: &'t Db, diag: T)
    where
        Db: HasJar<Jar> + ?Sized + 't,
        T: ToDiagnostic<Db<'t> = Db>,
    {
        Diagnostics::push(db, diag.to_diagnostic(db));
    }
}

impl Diagnostic {
    pub fn new(title: impl ToString, range: TextRange) -> Self {
        Self {
            range,
            title: title.to_string(),
            level: Level::Error,
            primary_label: None,
            secondary_labels: Vec::new(),
            notes: Vec::new(),
        }
    }

    pub fn with_level(self, level: Level) -> Self {
        Self { level, ..self }
    }

    pub fn with_primary_label(self, range: TextRange, msg: impl ToString) -> Self {
        Self {
            primary_label: Some(PrimaryLabel {
                range,
                message: msg.to_string(),
            }),
            ..self
        }
    }

    pub fn with_secondary_label(mut self, range: InFile<TextRange>, msg: impl ToString) -> Self {
        self.secondary_labels.push(SecondaryLabel {
            range,
            message: msg.to_string(),
        });
        self
    }

    pub fn with_note(mut self, note: impl ToString) -> Self {
        self.notes.push(note.to_string());
        self
    }
}
