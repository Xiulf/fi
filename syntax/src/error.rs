use rowan::{TextRange, TextSize};

#[derive(Debug, PartialEq, Eq)]
pub struct SyntaxError {
    pub msg: String,
    pub range: TextRange,
}

impl SyntaxError {
    pub fn new(msg: impl Into<String>, range: TextRange) -> Self {
        SyntaxError { msg: msg.into(), range }
    }

    pub fn new_at_offset(msg: impl Into<String>, offset: TextSize) -> Self {
        SyntaxError {
            msg: msg.into(),
            range: TextRange::empty(offset),
        }
    }

    pub fn range(&self) -> TextRange {
        self.range
    }

    pub fn with_range(self, range: TextRange) -> Self {
        SyntaxError { range, ..self }
    }
}

impl std::fmt::Display for SyntaxError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.msg.fmt(f)
    }
}
