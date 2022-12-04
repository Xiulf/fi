pub mod event;
mod grammar;
mod parser;
pub mod syntax_kind;
mod token_set;

use syntax_kind::SyntaxKind;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ParseError(pub Box<str>);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Token {
    pub kind: SyntaxKind,
}

pub trait TokenSource {
    fn current(&self) -> Token;
    fn current_text(&self) -> &str;

    fn lookahead_nth(&self, n: usize) -> Token;
    fn lookahead_text(&self, n: usize) -> &str;

    fn bump(&mut self);

    fn is_text(&self, text: &str) -> bool;
}

pub trait TreeSink {
    fn token(&mut self, kind: SyntaxKind);

    fn start_node(&mut self, kind: SyntaxKind);

    fn finish_node(&mut self);

    fn error(&mut self, error: ParseError);
}

fn parse_from_tokens(
    token_source: &mut dyn TokenSource,
    tree_sink: &mut dyn TreeSink,
    f: impl FnOnce(&mut parser::Parser),
) {
    let mut p = parser::Parser::new(token_source);

    f(&mut p);

    let events = p.finish();

    event::process(tree_sink, events);
}

pub fn parse(token_source: &mut dyn TokenSource, tree_sink: &mut dyn TreeSink) {
    parse_from_tokens(token_source, tree_sink, grammar::root);
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum FragmentKind {
    Path,
    Type,
}

pub fn parse_fragment(token_source: &mut dyn TokenSource, tree_sink: &mut dyn TreeSink, fragment_kind: FragmentKind) {
    let parser: fn(&mut parser::Parser) = match fragment_kind {
        | FragmentKind::Path => grammar::fragments::path,
        | FragmentKind::Type => grammar::fragments::type_,
    };

    parse_from_tokens(token_source, tree_sink, parser);
}
