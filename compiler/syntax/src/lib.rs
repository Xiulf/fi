#![feature(trait_upcasting)]

pub mod ast;
pub mod parsing;
pub mod ptr;
pub mod syntax_node;

pub use cstree::interning::{new_interner, Rodeo as Interner};
pub use cstree::{TextRange, TextSize};
use diagnostics::{Db, Diagnostic, ToDiagnostic};
use parser::error::SyntaxError;
pub use parser::token::SyntaxKind;
pub use syntax_node::*;
use vfs::File;

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
            | SyntaxError::ParseError(e) => {
                let msg = format!(
                    "{}, expected {}",
                    if e.found.is_some() {
                        "unexpected token in input"
                    } else {
                        "unexpected end of input"
                    },
                    if e.expected.is_empty() {
                        "something else".to_string()
                    } else {
                        e.expected
                            .iter()
                            .filter(|e| !e.is_trivia())
                            .map(|e| e.to_string())
                            .collect::<Vec<_>>()
                            .join(", ")
                    }
                );

                Diagnostic::new(msg, file, e.range).with_primary_label(e.range, match e.found {
                    | Some(found) => format!("unexpected token {found}"),
                    | None => "unexpected end of input".to_string(),
                })
            },
        }
    }
}
