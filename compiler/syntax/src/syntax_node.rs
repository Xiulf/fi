use cstree::interning::Interner;
use cstree::{GreenNode, GreenNodeBuilder, Language};
use parser::error::SyntaxError;
use parser::token::SyntaxKind;

use crate::Parsed;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Lang {}

impl Language for Lang {
    type Kind = SyntaxKind;

    fn kind_from_raw(raw: cstree::SyntaxKind) -> Self::Kind {
        assert!(raw.0 < SyntaxKind::__LAST as u16);
        unsafe { std::mem::transmute(raw.0) }
    }

    fn kind_to_raw(kind: Self::Kind) -> cstree::SyntaxKind {
        cstree::SyntaxKind(kind as u16)
    }

    fn static_text(kind: Self::Kind) -> Option<&'static str> {
        match kind {
            | _ => None,
        }
    }
}

pub type SyntaxNode = cstree::SyntaxNode<Lang>;
pub type SyntaxToken = cstree::SyntaxToken<Lang>;
pub type SyntaxElement = cstree::SyntaxElement<Lang>;

pub struct SyntaxTreeBuilder<'i, I> {
    inner: GreenNodeBuilder<'i, 'i, Lang, I>,
    errors: Vec<SyntaxError>,
}

impl<'i, I: Interner> SyntaxTreeBuilder<'i, I> {
    pub fn new(interner: &'i mut I) -> Self {
        Self {
            inner: GreenNodeBuilder::with_interner(interner),
            errors: Vec::new(),
        }
    }

    pub(crate) fn finish_raw(self) -> (GreenNode, Vec<SyntaxError>) {
        let (node, _) = self.inner.finish();

        (node, self.errors)
    }

    pub fn finish(self) -> Parsed<SyntaxNode> {
        let (green, errors) = self.finish_raw();

        Parsed::new(green, errors)
    }

    pub fn token(&mut self, kind: SyntaxKind, text: &str) {
        self.inner.token(kind, text);
    }

    pub fn start_node(&mut self, kind: SyntaxKind) {
        self.inner.start_node(kind);
    }

    pub fn finish_node(&mut self) {
        self.inner.finish_node();
    }

    pub fn error(&mut self, error: SyntaxError) {
        self.errors.push(error);
    }
}
