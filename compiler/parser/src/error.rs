use std::collections::BTreeSet;

use text_size::TextRange;

use crate::token::SyntaxKind;

#[derive(Clone, Debug, PartialEq, Eq, serde::Serialize)]
pub enum SyntaxError {
    ParseError(ParseError),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, serde::Serialize)]
pub struct ParseError {
    pub range: TextRange,
    pub found: Option<SyntaxKind>,
    pub expected: BTreeSet<SyntaxKind>,
}

impl ParseError {
    pub fn new(range: TextRange, found: Option<SyntaxKind>, expected: impl IntoIterator<Item = SyntaxKind>) -> Self {
        ParseError {
            range,
            found,
            expected: BTreeSet::from_iter(expected),
        }
    }
}
