use crate::buffer::Cursor;
use crate::error::Result;
use crate::parse::{Parse, ParseStream};
use crate::token::Token;
use codespan::Span;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Punct {
    pub span: Span,
    pub ch: char,
    pub spacing: Spacing,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Spacing {
    Alone,
    Joint,
}

impl<D> Parse<D> for Punct {
    fn parse(input: ParseStream<D>) -> Result<Punct> {
        input.step(|cursor| match cursor.punct() {
            Some((p, rest)) => Ok((p, rest)),
            None => Err(cursor.error("expected a punctuation character")),
        })
    }
}

impl Token for Punct {
    fn peek(cursor: Cursor) -> bool {
        match cursor.punct() {
            Some(_) => true,
            None => false,
        }
    }

    fn display() -> &'static str {
        "punctuation"
    }
}

// impl ToTokens for Punct {
//     fn to_tokens(&self) -> TokenBuffer {
//         TokenBuffer::new(vec![Entry::Punct(self.clone())])
//     }
// }
