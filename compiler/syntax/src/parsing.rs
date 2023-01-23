use cstree::interning::Interner;
use cstree::TextSize;
use parser::error::SyntaxError;
use parser::token::SyntaxKind;
use parser::TreeSink;

use crate::syntax_node::{SyntaxNode, SyntaxTreeBuilder};
use crate::Parsed;

pub fn parse_text<I: Interner>(text: &str, interner: &mut I) -> Parsed<SyntaxNode> {
    let (output, errors) = parser::parse(text);
    let mut tree_sink = TextTreeSink::new(text, interner);

    if let Some(event) = output {
        parser::event::process(&mut tree_sink, event);
    } else {
        tree_sink.start_node(SyntaxKind::SOURCE_FILE);
        tree_sink.finish_node();
    }

    for error in errors {
        tree_sink.error(SyntaxError::ParseError(error));
    }

    tree_sink.inner.finish()
}

pub struct TextTreeSink<'t, 'i, I> {
    text: &'t str,
    text_pos: TextSize,
    inner: SyntaxTreeBuilder<'i, I>,
}

impl<'t, 'i, I: Interner> TextTreeSink<'t, 'i, I> {
    pub fn new(text: &'t str, interner: &'i mut I) -> Self {
        Self {
            text,
            text_pos: TextSize::from(0),
            inner: SyntaxTreeBuilder::new(interner),
        }
    }
}

impl<'t, I: Interner> TreeSink for TextTreeSink<'t, '_, I> {
    fn token(&mut self, kind: SyntaxKind, len: TextSize) {
        let text = &self.text[self.text_pos.into()..(self.text_pos + len).into()];
        self.text_pos += len;
        self.inner.token(kind, text);
    }

    fn start_node(&mut self, kind: SyntaxKind) {
        self.inner.start_node(kind);
    }

    fn finish_node(&mut self) {
        self.inner.finish_node();
    }

    fn error(&mut self, error: SyntaxError) {
        self.inner.error(error);
    }
}

#[test]
fn test_parsing() {
    let input = r#"
        module Core.Cmp =

        main = 0

        type X =
            | Y
            | Z

        trait Iterator self it =
            next :: self -> Option it

        impl Iterator Iter Item =
            next self = _
    "#;
    let input = unindent::unindent(input.trim());
    let mut interner = cstree::interning::new_interner();
    let parsed = parse_text(&input, &mut interner);

    insta::assert_debug_snapshot!(parsed);
}
