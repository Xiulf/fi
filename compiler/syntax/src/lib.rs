#![feature(trait_upcasting)]

pub mod ast;
pub mod parsing;
pub mod ptr;
pub mod syntax_node;

pub use cstree::interning::{new_interner, Rodeo as Interner};
pub use cstree::{TextRange, TextSize};
use diagnostics::{Db, Diagnostic, ToDiagnostic};
use parser::error::{ErrorReason, SyntaxError};
pub use parser::token::SyntaxKind;
pub use syntax_node::*;
use vfs::{File, InFile};

impl ast::SourceFile {
    pub fn parse(db: &dyn Db, file: File, interner: &mut Interner) -> Self {
        use ast::AstNode;
        Self::cast(&parsing::parse_text(db, file, interner)).unwrap()
    }
}

struct SyntaxDiagnostic(SyntaxError, File);

impl ToDiagnostic for SyntaxDiagnostic {
    type Db<'t> = dyn Db + 't;

    fn to_diagnostic(self, _: &Self::Db<'_>) -> Diagnostic {
        let SyntaxDiagnostic(error, file) = self;

        match error {
            | SyntaxError::ParseError(e) => match e.reason() {
                | ErrorReason::Custom(msg) => Diagnostic::new(msg, file, e.span()),
                | ErrorReason::Unexpected => {
                    let msg = format!(
                        "{}, expected {}",
                        if e.found().is_some() {
                            "Unexpected token in input"
                        } else {
                            "Unexpected end of input"
                        },
                        if e.expected().len() == 0 {
                            "something else".to_string()
                        } else {
                            e.expected()
                                .filter(|e| match e {
                                    | Some(e) => !e.is_trivia(),
                                    | None => true,
                                })
                                .map(|expected| match expected {
                                    | Some(expected) => expected.to_string(),
                                    | None => "end of input".to_string(),
                                })
                                .collect::<Vec<_>>()
                                .join(", ")
                        }
                    );

                    Diagnostic::new(msg, file, e.span()).with_primary_label(e.span(), match e.found() {
                        | Some(found) => format!("Unexpected token {found}"),
                        | None => "Unexpected end of input".to_string(),
                    })
                },
                | ErrorReason::Unclosed { span, delimiter } => {
                    Diagnostic::new(format!("Unclosed delimiter {}", delimiter), file, *span)
                        .with_primary_label(*span, format!("Unclosed delimiter {}", delimiter))
                        .with_secondary_label(InFile::new(file, e.span()), match e.found() {
                            | Some(found) => format!("Must be closed before this {found}"),
                            | None => "Must be closed before the end of the input".to_string(),
                        })
                },
            },
        }
    }
}
