use crate::error::*;
use crate::Parsed;
use parser::syntax_kind::SyntaxKind;
use rowan::{GreenNode, GreenNodeBuilder, Language};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum FcLanguage {}

impl Language for FcLanguage {
    type Kind = SyntaxKind;

    fn kind_from_raw(raw: rowan::SyntaxKind) -> SyntaxKind {
        SyntaxKind::from(raw.0)
    }

    fn kind_to_raw(kind: SyntaxKind) -> rowan::SyntaxKind {
        rowan::SyntaxKind(kind.into())
    }
}

pub type SyntaxNode = rowan::SyntaxNode<FcLanguage>;
pub type SyntaxToken = rowan::SyntaxToken<FcLanguage>;
pub type SyntaxElement = rowan::NodeOrToken<SyntaxNode, SyntaxToken>;
pub type SyntaxNodeChildren = rowan::SyntaxNodeChildren<FcLanguage>;
pub type SyntaxElementChildren = rowan::SyntaxElementChildren<FcLanguage>;

pub use rowan::{Direction, NodeOrToken};

pub struct SyntaxTreeBuilder<'i> {
    inner: GreenNodeBuilder<'i>,
    errors: Vec<SyntaxError>,
}

impl SyntaxTreeBuilder<'_> {
    pub fn new() -> Self {
        SyntaxTreeBuilder {
            inner: GreenNodeBuilder::new(),
            errors: Vec::new(),
        }
    }

    pub(crate) fn finish_raw(self) -> (GreenNode, Vec<SyntaxError>) {
        (self.inner.finish(), self.errors)
    }

    pub fn finish(self) -> Parsed<SyntaxNode> {
        let (green, errors) = self.finish_raw();

        Parsed::new(green, errors)
    }

    pub fn token(&mut self, kind: SyntaxKind, text: &str) {
        let kind = FcLanguage::kind_to_raw(kind);

        self.inner.token(kind, text);
    }

    pub fn start_node(&mut self, kind: SyntaxKind) {
        let kind = FcLanguage::kind_to_raw(kind);

        self.inner.start_node(kind);
    }

    pub fn finish_node(&mut self) {
        self.inner.finish_node();
    }

    pub fn error(&mut self, error: parser::ParseError, pos: rowan::TextSize) {
        self.errors.push(SyntaxError::new_at_offset(error.0, pos));
    }
}
