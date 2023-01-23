pub mod ast;
pub mod parsing;
pub mod ptr;
pub mod syntax_node;

use std::marker::PhantomData;

pub use cstree::interning::{new_interner, Rodeo as Interner};
use cstree::{GreenNode, Language};
pub use cstree::{TextRange, TextSize};
use parser::error::SyntaxError;
pub use parser::token::SyntaxKind;
pub use syntax_node::*;
use triomphe::Arc;

#[derive(Debug, PartialEq, Eq)]
pub struct Parsed<T> {
    green: GreenNode,
    errors: Arc<[SyntaxError]>,
    _marker: PhantomData<fn() -> T>,
}

impl<T> Parsed<T> {
    pub fn new(green: GreenNode, errors: Vec<SyntaxError>) -> Self {
        Self {
            green,
            errors: errors.into(),
            _marker: PhantomData,
        }
    }

    pub fn syntax_node(&self) -> SyntaxNode {
        SyntaxNode::new_root(self.green.clone())
    }

    pub fn errors(&self) -> &[SyntaxError] {
        &self.errors
    }
}

impl<T> Clone for Parsed<T> {
    fn clone(&self) -> Self {
        Self {
            green: self.green.clone(),
            errors: self.errors.clone(),
            _marker: PhantomData,
        }
    }
}

impl<T: ast::AstNode> Parsed<T> {
    pub fn into_syntax(self) -> Parsed<SyntaxNode> {
        Parsed {
            green: self.green,
            errors: self.errors,
            _marker: PhantomData,
        }
    }

    pub fn tree(&self) -> T {
        T::cast(&self.syntax_node()).unwrap()
    }

    pub fn cast<N: ast::AstNode>(self) -> Option<Parsed<N>> {
        if N::can_cast(Lang::kind_from_raw(self.green.kind())) {
            Some(Parsed {
                green: self.green,
                errors: self.errors,
                _marker: PhantomData,
            })
        } else {
            None
        }
    }
}

impl ast::SourceFile {
    pub fn parse(text: &str, interner: &mut Interner) -> Parsed<Self> {
        let node = parsing::parse_text(text, interner);

        Parsed {
            green: node.green,
            errors: node.errors,
            _marker: PhantomData,
        }
    }
}
