pub mod ast;
mod parsing;
pub mod symbol;

use std::sync::Arc;

#[salsa::query_group(SyntaxDatabaseStorage)]
pub trait SyntaxDatabase: source::SourceDatabase {
    #[salsa::invoke(parse_query)]
    fn parse(&self, id: source::FileId) -> Arc<ast::Module>;
}

fn parse_query(db: &dyn SyntaxDatabase, id: source::FileId) -> Arc<ast::Module> {
    use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
    let writer = StandardStream::stderr(ColorChoice::Always);
    let config = codespan_reporting::term::Config::default();
    let source = db.file_content(id);
    let mut lexer = parser::lexer::Lexer::new(&source, id);
    let tokens = match lexer.run() {
        Ok(tokens) => tokens,
        Err(errors) => {
            for err in errors {
                codespan_reporting::term::emit(&mut writer.lock(), &config, &*db.files(), &err)
                    .unwrap();
            }

            std::process::exit(1);
        }
    };

    let tokens = parser::layout::fix_layout(&db.files(), tokens);
    // println!("{}", tokens);
    let buffer = parser::parse::ParseBuffer::new(tokens.begin(), (), codespan::Span::default());

    match buffer.parse::<ast::Module>() {
        Ok(module) => Arc::new(module),
        Err(e) => {
            codespan_reporting::term::emit(&mut writer.lock(), &config, &*db.files(), &e).unwrap();
            std::process::exit(1);
        }
    }
}
