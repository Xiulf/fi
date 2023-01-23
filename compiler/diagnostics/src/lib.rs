use text_size::TextRange;
use vfs::in_file::InFile;

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
