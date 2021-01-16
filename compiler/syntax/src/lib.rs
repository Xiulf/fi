#![feature(once_cell)]

pub mod ast;
pub mod group;
mod parsing;
pub mod symbol;

use std::sync::Arc;

#[salsa::query_group(SyntaxDatabaseStorage)]
pub trait SyntaxDatabase: source::SourceDatabase + diagnostics::Diagnostics {
    fn parse(&self, id: source::FileId) -> Arc<ast::Module>;
}

fn parse(db: &dyn SyntaxDatabase, id: source::FileId) -> Arc<ast::Module> {
    let source = db.file_content(id);
    let mut lexer = parser::lexer::Lexer::new(&source);
    let tokens = match lexer.collect::<Result<Vec<_>, _>>() {
        Ok(tokens) => tokens,
        Err(error) => {
            report_lex_error(db, id, error);
            db.print_and_exit();
        }
    };

    // for tok in &tokens {
    //     println!("{:>16?}: {:?}", tok.kind, lexer.text(tok.span));
    // }
    //
    // println!();

    let tokens = parser::buffer::TokenBuffer::new(id, tokens);
    let buffer = parser::parse::ParseBuffer::new(tokens.begin(&source), codespan::Span::default());

    match buffer.parse::<ast::Module>() {
        Ok(module) => Arc::new(module),
        Err(e) => {
            report_parse_error(db, id, e);
            db.print_and_exit();
        }
    }
}

fn report_lex_error(db: &dyn SyntaxDatabase, file: source::FileId, error: parser::lexer::LexicalError) {
    match error {
        parser::lexer::LexicalError::UnknownChar(idx, ch) => db
            .to_diag_db()
            .error(format!("unknown character {:?}", ch))
            .with_label(diagnostics::Label::primary(file, ast::Span::new(idx, idx)))
            .finish(),
    }
}

fn report_parse_error(db: &dyn SyntaxDatabase, file: source::FileId, error: parser::parse::ParseError) {
    db.to_diag_db()
        .error(error.expected)
        .with_label(diagnostics::Label::primary(file, error.span))
        .finish();
}
