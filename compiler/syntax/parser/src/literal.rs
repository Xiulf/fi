use crate::buffer::{Cursor, Entry, TokenBuffer};
use crate::error::Result;
use crate::parse::{Parse, ParseStream, ToTokens};
use crate::token::Token;
use diagnostics::{Span, Spanned};
use std::fmt::{Display, Formatter, Result as FmtResult};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Literal {
    String(StringLiteral),
    Char(CharLiteral),
    Int(IntLiteral),
    Float(FloatLiteral),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
pub struct StringLiteral {
    pub span: Span,
    pub text: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CharLiteral {
    pub span: Span,
    pub ch: char,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct IntLiteral {
    pub span: Span,
    pub int: u128,
    pub ty: IntType,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FloatLiteral {
    pub span: Span,
    pub float: u64,
    pub ty: FloatType,
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum IntType {
    U8,
    U16,
    U32,
    U64,
    U128,
    I8,
    I16,
    I32,
    I64,
    I128,
    Unknown,
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum FloatType {
    F32,
    F64,
    Unknown,
}

impl Literal {
    pub fn string(&self) -> Option<&str> {
        match self {
            Literal::String(lit) => Some(&lit.text),
            _ => None,
        }
    }
}

impl<D> Parse<D> for Literal {
    fn parse(input: ParseStream<D>) -> Result<Literal> {
        input.step(|cursor| {
            if let Some((literal, rest)) = cursor.literal() {
                Ok((literal, rest))
            } else {
                Err(cursor.error("expected a string, character, integer or float"))
            }
        })
    }
}

impl Token for Literal {
    fn peek(cursor: Cursor) -> bool {
        if let Some(_) = cursor.literal() {
            true
        } else {
            false
        }
    }

    fn display() -> &'static str {
        "string, character, integer or float"
    }
}

impl Spanned for Literal {
    fn span(&self) -> Span {
        match self {
            Literal::String(string) => string.span(),
            Literal::Char(char) => char.span(),
            Literal::Int(int) => int.span(),
            Literal::Float(float) => float.span(),
        }
    }
}

impl<D> Parse<D> for IntLiteral {
    fn parse(input: ParseStream<D>) -> Result<IntLiteral> {
        input.step(|cursor| {
            if let Some((Literal::Int(literal), rest)) = cursor.literal() {
                Ok((literal, rest))
            } else {
                Err(cursor.error("expected an integer"))
            }
        })
    }
}

impl<D> Parse<D> for FloatLiteral {
    fn parse(input: ParseStream<D>) -> Result<FloatLiteral> {
        input.step(|cursor| {
            if let Some((Literal::Float(literal), rest)) = cursor.literal() {
                Ok((literal, rest))
            } else {
                Err(cursor.error("expected a float"))
            }
        })
    }
}

impl<D> Parse<D> for StringLiteral {
    fn parse(input: ParseStream<D>) -> Result<StringLiteral> {
        input.step(|cursor| {
            if let Some((Literal::String(literal), rest)) = cursor.literal() {
                Ok((literal, rest))
            } else {
                Err(cursor.error("expected a string"))
            }
        })
    }
}

impl Spanned for StringLiteral {
    fn span(&self) -> Span {
        self.span.clone()
    }
}

impl Spanned for CharLiteral {
    fn span(&self) -> Span {
        self.span.clone()
    }
}

impl Spanned for IntLiteral {
    fn span(&self) -> Span {
        self.span.clone()
    }
}

impl Spanned for FloatLiteral {
    fn span(&self) -> Span {
        self.span.clone()
    }
}

impl ToTokens for Literal {
    fn to_tokens(&self) -> TokenBuffer {
        TokenBuffer::new(vec![Entry::Literal(self.clone())])
    }
}

impl ToTokens for IntLiteral {
    fn to_tokens(&self) -> TokenBuffer {
        TokenBuffer::new(vec![Entry::Literal(Literal::Int(self.clone()))])
    }
}

impl ToTokens for FloatLiteral {
    fn to_tokens(&self) -> TokenBuffer {
        TokenBuffer::new(vec![Entry::Literal(Literal::Float(self.clone()))])
    }
}

impl ToTokens for CharLiteral {
    fn to_tokens(&self) -> TokenBuffer {
        TokenBuffer::new(vec![Entry::Literal(Literal::Char(self.clone()))])
    }
}

impl ToTokens for StringLiteral {
    fn to_tokens(&self) -> TokenBuffer {
        TokenBuffer::new(vec![Entry::Literal(Literal::String(self.clone()))])
    }
}

impl Display for Literal {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        match self {
            Literal::Int(i) => i.fmt(f),
            Literal::Float(i) => i.fmt(f),
            Literal::Char(i) => i.fmt(f),
            Literal::String(i) => i.fmt(f),
        }
    }
}

impl Display for IntLiteral {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        write!(f, "{}{}", self.int, self.ty)
    }
}

impl Display for FloatLiteral {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        write!(f, "{}{}", f64::from_bits(self.float), self.ty)
    }
}

impl Display for CharLiteral {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        std::fmt::Debug::fmt(&self.ch, f)
    }
}

impl Display for StringLiteral {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        std::fmt::Debug::fmt(&self.text, f)
    }
}

impl Display for IntType {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        match self {
            IntType::U8 => write!(f, "u8"),
            IntType::U16 => write!(f, "u16"),
            IntType::U32 => write!(f, "u32"),
            IntType::U64 => write!(f, "u64"),
            IntType::U128 => write!(f, "u128"),
            IntType::I8 => write!(f, "i8"),
            IntType::I16 => write!(f, "i16"),
            IntType::I32 => write!(f, "i32"),
            IntType::I64 => write!(f, "i64"),
            IntType::I128 => write!(f, "i128"),
            IntType::Unknown => write!(f, ""),
        }
    }
}

impl Display for FloatType {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        match self {
            FloatType::F32 => write!(f, "f32"),
            FloatType::F64 => write!(f, "f64"),
            FloatType::Unknown => write!(f, ""),
        }
    }
}
