use crate::buffer::Cursor;

pub trait Token {
    fn peek(cursor: Cursor) -> bool;
    fn display() -> &'static str;
}

macro_rules! tokens {
    ($($name:ident, $type:ident, $str:literal)*) => {
        $(
            #[derive(Clone, Copy)]
            pub struct $name {
                pub span: $crate::Span,
            }

            impl $crate::token::Token for $name {
                fn peek(cursor: $crate::buffer::Cursor) -> bool {
                    match cursor.token() {
                        $crate::lexer::Token {
                            span: _,
                            kind: $crate::lexer::TokenType::$type
                        } => true,
                        _ => false,
                    }
                }

                fn display() -> &'static str {
                    $str
                }
            }

            impl $crate::parse::Parse for $name {
                fn parse(input: $crate::parse::ParseStream) -> $crate::parse::Result<Self> {
                    input.step(|cursor: $crate::parse::StepCursor| {
                        if let Some((
                            $crate::lexer::Token {
                                span,
                                kind: $crate::lexer::TokenType::$type
                            }, rest
                        )) = cursor.any() {
                            Ok(($name { span }, rest))
                        } else {
                            Err($crate::parse::ParseError {
                                span: cursor.span(),
                                expected: $str.into(),
                            })
                        }
                    })
                }
            }
        )*
    }
}

#[macro_export]
macro_rules! keywords {
    ($($name:ident, $str:literal)*) => {
        $(
            #[derive(Clone, Copy)]
            pub struct $name {
                pub span: $crate::Span,
            }

            impl $crate::token::Token for $name {
                fn peek(cursor: $crate::buffer::Cursor) -> bool {
                    match cursor.token() {
                        $crate::lexer::Token {
                            span,
                            kind: $crate::lexer::TokenType::Name,
                        } => cursor.text(*span) == $str,
                        _ => false,
                    }
                }

                fn display() -> &'static str {
                    $str
                }
            }

            impl $crate::parse::Parse for $name {
                fn parse(input: $crate::parse::ParseStream) -> $crate::parse::Result<Self> {
                    input.step(|cursor: $crate::parse::StepCursor| {
                        match cursor.any() {
                            Some((
                                $crate::lexer::Token {
                                    span,
                                    kind: $crate::lexer::TokenType::Name,
                                },
                                rest,
                            )) if cursor.text(span) == $str => {
                                Ok(($name { span }, rest))
                            },
                            _ => {
                                Err($crate::parse::ParseError {
                                    span: cursor.span(),
                                    expected: $str.into(),
                                })
                            }
                        }
                    })
                }
            }
        )*
    }
}

tokens! {
    TLParen,      LeftParen,     "("
    TRParen,      RightParen,    ")"
    TLBrace,      LeftBrace,     "{"
    TRBrace,      RightBrace,    "}"
    TLBracket,    LeftBracket,   "["
    TRBracket,    RightBracket,  "]"
    TLArrow,      LeftArrow,     "<-"
    TRArrow,      RightArrow,    "->"
    TFatArrow,    FatArrow,      "=>"
    TDblColon,    DoubleColon,   "::"
    TDblDot,      DoubleDot,     ".."
    TDot,         Dot,           "."
    TComma,       Comma,         ","
    TPipe,        Pipe,          "|"
    TEquals,      Equals,        "="
    TQmark,       Qmark,         "?"
    TAt,          At,            "@"
    TUnderscore,  Underscore,    "underscore"
    TOperator,    Operator,      "operator"
    TSymbol,      Symbol,        "symbol"
    TName,        Name,          "identifier"
    TInt,         Int,           "integer literal"
    TFloat,       Float,         "floating point literal"
    TString,      String,        "string literal"
    TRawString,   RawString,     "raw string literal"
    TChar,        Char,          "character literal"
    TLytStart,    LayoutStart,   "block start"
    TLytSep,      LayoutSep,     "newline"
    TLytEnd,      LayoutEnd,     "block end"
}
