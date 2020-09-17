use crate::parse::{Parse, ParseStream, ToTokens};
use crate::token::Token;
use crate::buffer::{Cursor, TokenBuffer, Entry};
use crate::error::Result;
use diagnostics::{Span, Spanned};
use std::fmt::{Display, Formatter, Result as FmtResult};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Ident {
    pub span: Span,
    pub name: String
}

impl<D> Parse<D> for Ident {
    fn parse(input: ParseStream<D>) -> Result<Ident> {
        input.step(|cursor| match cursor.ident() {
            Some((ident, rest)) => Ok((ident, rest)),
            None => Err(cursor.error("expected an identifier"))
        })
    }
}

impl Token for Ident {
    fn peek(cursor: Cursor) -> bool {
        match cursor.ident() {
            Some((_, _)) => true,
            None => false
        }
    }
    
    fn display() -> &'static str {
        "identifier"
    }
}

impl Spanned for Ident {
    fn span(&self) -> Span {
        self.span.clone()
    }
}

impl ToTokens for Ident {
    fn to_tokens(&self) -> TokenBuffer {
        TokenBuffer::new(vec![
            Entry::Ident(self.clone())
        ])
    }
}

impl Display for Ident {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        self.name.fmt(f)
    }
}
