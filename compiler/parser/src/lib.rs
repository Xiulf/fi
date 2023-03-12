#![feature(let_chains, unboxed_closures, fn_traits)]

use error::SyntaxError;
use text_size::{TextRange, TextSize};
use token::SyntaxKind;

pub mod error;
pub mod event;
pub mod grammar;
pub mod lexer;
pub mod parser;
pub mod token;
pub mod token_set;

pub fn parse(tokens: &[token::Token]) -> Vec<event::Event> {
    let tokens = tokens
        .iter()
        .filter_map({
            let mut pos = TextSize::from(0);
            move |t| {
                let token = if t.kind.is_trivia() {
                    None
                } else {
                    Some((t.kind, TextRange::at(pos, t.len)))
                };

                pos += t.len;
                token
            }
        })
        .collect::<Vec<_>>();
    let mut parser = parser::Parser::new(&tokens);

    grammar::source_file(&mut parser);
    parser.finish()
}

pub trait TreeSink {
    fn token(&mut self, kind: SyntaxKind);
    fn start_node(&mut self, kind: SyntaxKind);
    fn finish_node(&mut self);
    fn error(&mut self, error: SyntaxError);
}

#[cfg(test)]
pub struct DebugTreeSink;

#[cfg(test)]
impl TreeSink for DebugTreeSink {
    fn token(&mut self, kind: SyntaxKind) {
        eprintln!("token({kind})");
    }

    fn start_node(&mut self, kind: SyntaxKind) {
        eprintln!("start({kind})");
    }

    fn finish_node(&mut self) {
        eprintln!("finish");
    }

    fn error(&mut self, error: SyntaxError) {
        eprintln!("error({error:?})");
    }
}
