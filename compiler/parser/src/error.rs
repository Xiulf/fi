use std::collections::HashSet;
use std::fmt;
use std::ops::Range;

use chumsky::Error;
use text_size::{TextRange, TextSize};

use crate::token::SyntaxKind;

#[derive(Clone, Debug, PartialEq, Eq, serde::Serialize)]
pub enum SyntaxError {
    ParseError(ParseError),
}

impl fmt::Display for SyntaxError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            | Self::ParseError(e) => e.fmt(f),
        }
    }
}

/// A type representing possible reasons for an error.
#[derive(Clone, Debug, PartialEq, Eq, serde::Serialize)]
pub enum ErrorReason {
    /// An unexpected input was found.
    Unexpected,
    /// An unclosed delimiter was found.
    Unclosed {
        /// The span of the unclosed delimiter.
        span: TextRange,
        /// The unclosed delimiter.
        delimiter: SyntaxKind,
    },
    /// An error with a custom message occurred.
    Custom(String),
}

impl fmt::Display for ErrorReason {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        const DEFAULT_DISPLAY_UNEXPECTED: &str = "unexpected input";

        match self {
            | Self::Unexpected => write!(f, "{}", DEFAULT_DISPLAY_UNEXPECTED),
            | Self::Unclosed { span, delimiter } => {
                write!(
                    f,
                    "unclosed delimiter ({}..{}) in {}",
                    usize::from(span.start()),
                    usize::from(span.end()),
                    delimiter
                )
            },
            | Self::Custom(string) => write!(f, "error {}", string),
        }
    }
}

/// A type representing zero, one, or many labels applied to an error
#[derive(Clone, Copy, Debug, PartialEq, serde::Serialize)]
enum ErrorLabel {
    Some(&'static str),
    None,
    Multi,
}

impl ErrorLabel {
    fn merge(self, other: Self) -> Self {
        match (self, other) {
            | (ErrorLabel::Some(a), ErrorLabel::Some(b)) if a == b => ErrorLabel::Some(a),
            | (ErrorLabel::Some(_), ErrorLabel::Some(_)) => ErrorLabel::Multi,
            | (ErrorLabel::Multi, _) => ErrorLabel::Multi,
            | (_, ErrorLabel::Multi) => ErrorLabel::Multi,
            | (ErrorLabel::None, x) => x,
            | (x, ErrorLabel::None) => x,
        }
    }
}

impl From<ErrorLabel> for Option<&'static str> {
    fn from(label: ErrorLabel) -> Self {
        match label {
            | ErrorLabel::Some(s) => Some(s),
            | _ => None,
        }
    }
}

#[derive(Clone, Debug, serde::Serialize)]
pub struct ParseError {
    span: TextRange,
    reason: ErrorReason,
    expected: HashSet<Option<SyntaxKind>>,
    found: Option<SyntaxKind>,
    label: ErrorLabel,
}

impl ParseError {
    /// Create an error with a custom error message.
    pub fn custom<M: ToString>(span: Range<usize>, msg: M) -> Self {
        Self {
            span: TextRange::new(TextSize::from(span.start as u32), TextSize::from(span.end as u32)),
            reason: ErrorReason::Custom(msg.to_string()),
            expected: HashSet::default(),
            found: None,
            label: ErrorLabel::None,
        }
    }

    /// Returns the span that the error occurred at.
    pub fn span(&self) -> TextRange {
        self.span.clone()
    }

    /// Returns an iterator over possible expected patterns.
    pub fn expected(&self) -> impl ExactSizeIterator<Item = &Option<SyntaxKind>> + '_ {
        self.expected.iter()
    }

    /// Returns the input, if any, that was found instead of an expected pattern.
    pub fn found(&self) -> Option<&SyntaxKind> {
        self.found.as_ref()
    }

    /// Returns the reason for the error.
    pub fn reason(&self) -> &ErrorReason {
        &self.reason
    }

    /// Returns the error's label, if any.
    pub fn label(&self) -> Option<&'static str> {
        self.label.into()
    }
}

impl Error<SyntaxKind> for ParseError {
    type Span = Range<usize>;
    type Label = &'static str;

    fn expected_input_found<Iter: IntoIterator<Item = Option<SyntaxKind>>>(
        span: Self::Span,
        expected: Iter,
        found: Option<SyntaxKind>,
    ) -> Self {
        Self {
            span: TextRange::new(TextSize::from(span.start as u32), TextSize::from(span.end as u32)),
            reason: ErrorReason::Unexpected,
            expected: expected.into_iter().collect(),
            found,
            label: ErrorLabel::None,
        }
    }

    fn unclosed_delimiter(
        unclosed_span: Self::Span,
        delimiter: SyntaxKind,
        span: Self::Span,
        expected: SyntaxKind,
        found: Option<SyntaxKind>,
    ) -> Self {
        Self {
            span: TextRange::new(TextSize::from(span.start as u32), TextSize::from(span.end as u32)),
            reason: ErrorReason::Unclosed {
                span: TextRange::new(
                    TextSize::from(unclosed_span.start as u32),
                    TextSize::from(unclosed_span.end as u32),
                ),
                delimiter,
            },
            expected: core::iter::once(Some(expected)).collect(),
            found,
            label: ErrorLabel::None,
        }
    }

    fn with_label(mut self, label: Self::Label) -> Self {
        match self.label {
            | ErrorLabel::Some(_) => {},
            | _ => {
                self.label = ErrorLabel::Some(label);
            },
        }
        self
    }

    fn merge(mut self, other: Self) -> Self {
        // TODO: Assert that `self.span == other.span` here?
        self.reason = match (&self.reason, &other.reason) {
            | (ErrorReason::Unclosed { .. }, _) => self.reason,
            | (_, ErrorReason::Unclosed { .. }) => other.reason,
            | _ => self.reason,
        };
        self.label = self.label.merge(other.label);
        for expected in other.expected {
            self.expected.insert(expected);
        }
        self
    }
}

impl PartialEq for ParseError {
    fn eq(&self, other: &Self) -> bool {
        self.span == other.span && self.found == other.found && self.reason == other.reason && self.label == other.label
    }
}

impl Eq for ParseError {
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // TODO: Take `self.reason` into account

        if let Some(found) = &self.found {
            write!(f, "found {:?}", found.to_string())?;
        } else {
            write!(f, "found end of input")?;
        };

        match self.expected.len() {
            | 0 => {}, //write!(f, " but end of input was expected")?,
            | 1 => write!(f, " but expected {}", match self.expected.iter().next().unwrap() {
                | Some(x) => format!("{:?}", x.to_string()),
                | None => "end of input".to_string(),
            },)?,
            | _ => {
                write!(
                    f,
                    " but expected one of {}",
                    self.expected
                        .iter()
                        .map(|expected| match expected {
                            | Some(x) => format!("{:?}", x.to_string()),
                            | None => "end of input".to_string(),
                        })
                        .collect::<Vec<_>>()
                        .join(", ")
                )?;
            },
        }

        Ok(())
    }
}

impl std::error::Error for ParseError {
}
