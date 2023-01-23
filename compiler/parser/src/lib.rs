#![feature(let_chains, unboxed_closures, fn_traits)]

use error::SyntaxError;
use text_size::TextSize;
use token::SyntaxKind;

pub mod error;
pub mod event;
pub mod lexer;
pub mod parser;
pub mod token;

pub use self::parser::parse;

pub trait TreeSink {
    fn token(&mut self, kind: SyntaxKind, len: TextSize);
    fn start_node(&mut self, kind: SyntaxKind);
    fn finish_node(&mut self);
    fn error(&mut self, error: SyntaxError);
}

#[cfg(test)]
pub struct DebugTreeSink;

#[cfg(test)]
impl TreeSink for DebugTreeSink {
    fn token(&mut self, kind: SyntaxKind, len: TextSize) {
        eprintln!("token({kind}, {})", u32::from(len));
    }

    fn start_node(&mut self, kind: SyntaxKind) {
        eprintln!("start({kind})");
    }

    fn finish_node(&mut self) {
        eprintln!("finish");
    }

    fn error(&mut self, error: SyntaxError) {
        eprintln!("error({error})");
    }
}
