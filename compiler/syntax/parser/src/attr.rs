use crate::buffer::Cursor;
use crate::error::Result;
use crate::parse::{Parse, ParseStream};
use crate::token::Token;
use codespan::Span;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Attr {
    pub span: Span,
    pub text: String,
}

impl<D> Parse<D> for Attr {
    fn parse(input: ParseStream<D>) -> Result<Self> {
        input.step(|cursor| match cursor.attr() {
            Some((a, rest)) => Ok((a, rest)),
            None => Err(cursor.error("expected an attribute")),
        })
    }
}

impl Token for Attr {
    fn peek(cursor: Cursor) -> bool {
        match cursor.attr() {
            Some(_) => true,
            None => false,
        }
    }

    fn display() -> &'static str {
        "attribute"
    }
}

// impl ToTokens for Attr {
//     fn to_tokens(&self) -> TokenBuffer {
//         TokenBuffer::new(vec![Entry::Attr(self.clone())])
//     }
// }

impl std::fmt::Display for Attr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "--|{}", self.text)
    }
}
