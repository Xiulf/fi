pub mod lexer;
mod token_source;
mod tree_sink;

use crate::error::SyntaxError;
use parser::FragmentKind;
use rowan::GreenNode;

pub fn parse_text(text: &str) -> (GreenNode, Vec<SyntaxError>) {
    let (tokens, errors) = lexer::tokenize(text);
    // let mut pos = rowan::TextSize::from(0);

    // for token in &tokens {
    //     println!("{:?}: {:?}", &text[rowan::TextRange::at(pos, token.len)], token);
    //     pos += token.len;
    // }

    let mut token_source = token_source::TextTokenSource::new(text, &tokens);
    let mut tree_sink = tree_sink::TextTreeSink::new(text, &tokens);

    parser::parse(&mut token_source, &mut tree_sink);

    let (tree, mut parser_errors) = tree_sink.finish();

    parser_errors.extend(errors);

    (tree, parser_errors)
}

pub fn parse_fragment(text: &str, kind: FragmentKind) -> (GreenNode, Vec<SyntaxError>) {
    let (tokens, errors) = lexer::tokenize(text);
    let mut token_source = token_source::TextTokenSource::new(text, &tokens);
    let mut tree_sink = tree_sink::TextTreeSink::new(text, &tokens);

    parser::parse_fragment(&mut token_source, &mut tree_sink, kind);

    let (tree, mut parser_errors) = tree_sink.finish();

    parser_errors.extend(errors);

    (tree, parser_errors)
}
