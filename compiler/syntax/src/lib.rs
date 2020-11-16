pub mod ast;
pub mod group;
mod parsing;
pub mod symbol;

use std::sync::Arc;

#[salsa::query_group(SyntaxDatabaseStorage)]
pub trait SyntaxDatabase: source::SourceDatabase + diagnostics::Diagnostics {
    #[salsa::invoke(parse_query)]
    fn parse(&self, id: source::FileId) -> Arc<ast::Module>;
}

fn parse_query(db: &dyn SyntaxDatabase, id: source::FileId) -> Arc<ast::Module> {
    let source = db.file_content(id);
    let mut lexer = parser::lexer::Lexer::new(&source, id);
    let tokens = match lexer.run() {
        Ok(tokens) => tokens,
        Err(errors) => {
            db.report_all(errors);
            db.print_and_exit();
        }
    };

    let tokens = parser::layout::fix_layout(&db.files(), tokens);
    // println!("{}", tokens);
    let buffer = parser::parse::ParseBuffer::new(tokens.begin(), (), codespan::Span::default());

    match buffer.parse::<ast::Module>() {
        Ok(module) => Arc::new(module),
        Err(e) => {
            db.report(e);
            db.print_and_exit();
        }
    }
}
