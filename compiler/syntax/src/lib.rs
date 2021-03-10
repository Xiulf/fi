#![feature(crate_visibility_modifier)]
#![feature(or_patterns)]

pub mod ast;
pub mod error;
pub mod parsing;
pub mod ptr;
pub mod syntax_node;

pub use ast::*;
pub use error::*;
pub use parser::syntax_kind::{self, SyntaxKind};
pub use ptr::*;
pub use rowan::{TextRange, TextSize};
pub use syntax_node::*;

use rowan::GreenNode;
use std::marker::PhantomData;
use std::sync::Arc;

#[derive(Debug, PartialEq, Eq)]
pub struct Parsed<T> {
    green: GreenNode,
    errors: Arc<[SyntaxError]>,
    _marker: PhantomData<fn() -> T>,
}

impl<T> Clone for Parsed<T> {
    fn clone(&self) -> Self {
        Parsed {
            green: self.green.clone(),
            errors: self.errors.clone(),
            _marker: PhantomData,
        }
    }
}

impl<T> Parsed<T> {
    fn new(green: GreenNode, errors: Vec<SyntaxError>) -> Self {
        Parsed {
            green,
            errors: errors.into(),
            _marker: PhantomData,
        }
    }

    pub fn syntax_node(&self) -> SyntaxNode {
        SyntaxNode::new_root(self.green.clone())
    }
}

impl<T: AstNode> Parsed<T> {
    pub fn into_syntax(self) -> Parsed<SyntaxNode> {
        Parsed {
            green: self.green,
            errors: self.errors,
            _marker: PhantomData,
        }
    }

    pub fn tree(&self) -> T {
        T::cast(self.syntax_node()).unwrap()
    }

    pub fn errors(&self) -> &[SyntaxError] {
        &*self.errors
    }

    pub fn ok(self) -> Result<T, Arc<[SyntaxError]>> {
        if self.errors.is_empty() {
            Ok(self.tree())
        } else {
            Err(self.errors)
        }
    }

    pub fn cast<N: AstNode>(self) -> Option<Parsed<N>> {
        if N::cast(self.syntax_node()).is_some() {
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

impl ast::Module {
    pub fn parse(text: &str) -> Parsed<Self> {
        let (green, errors) = parsing::parse_text(text);

        Parsed {
            green,
            errors: errors.into(),
            _marker: PhantomData,
        }
    }
}

impl Parsed<ast::Path> {
    pub fn parse(text: &str) -> Self {
        let (green, errors) = parsing::parse_fragment(text, parser::FragmentKind::Path);

        Parsed {
            green,
            errors: errors.into(),
            _marker: PhantomData,
        }
    }
}

impl Parsed<ast::Type> {
    pub fn parse(text: &str) -> Self {
        let (green, errors) = parsing::parse_fragment(text, parser::FragmentKind::Type);

        Parsed {
            green,
            errors: errors.into(),
            _marker: PhantomData,
        }
    }
}
