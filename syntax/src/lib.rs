pub mod ast;
mod parsing;
mod printing;
pub mod symbol;

pub fn parse(reporter: &diagnostics::Reporter, file: diagnostics::FileId) -> ast::Package {
    let mut lexer = parser::lexer::Lexer::new(&file.source, file, reporter);
    let tokens = lexer.run();
    let buffer = parser::parse::ParseBuffer::new(
        tokens.begin(),
        reporter,
        (),
        diagnostics::Span::empty(file),
    );

    buffer.parse().unwrap()
}
