use codespan::Span;
use lexer::{Token, TokenType};
use std::marker::PhantomData;

#[derive(Clone, Copy)]
pub struct Cursor<'a> {
    pub file: codespan::FileId,
    src: &'a str,
    ptr: *const Token,
    start: *const Token,
    end: *const Token,
    marker: PhantomData<&'a Token>,
}

pub struct TokenBuffer {
    pub file: codespan::FileId,
    pub tokens: Vec<Token>,
}

impl<'a> Cursor<'a> {
    pub fn new(file: codespan::FileId, src: &'a str, ptr: *const Token, start: *const Token, end: *const Token) -> Self {
        Cursor {
            file,
            src,
            ptr,
            start,
            end,
            marker: PhantomData,
        }
    }

    pub fn span(&self) -> Span {
        self.token().span
    }

    pub fn token(&self) -> &'a Token {
        unsafe { &*self.ptr }
    }

    pub fn prev(&self) -> Option<&'a Token> {
        if self.ptr > self.start {
            Some(unsafe { &*(self.ptr.offset(-1)) })
        } else {
            None
        }
    }

    pub fn text(&self, span: Span) -> &'a str {
        &self.src[span.start().to_usize()..span.end().to_usize()]
    }

    pub fn bump(self) -> Self {
        unsafe { Cursor::new(self.file, self.src, self.ptr.offset(1), self.start, self.end) }
    }

    pub fn eof(&self) -> bool {
        self.ptr == self.end
    }

    pub fn any(self) -> Option<(Token, Self)> {
        Some((self.token().clone(), self.bump()))
    }

    pub fn offset(self, other: Cursor) -> usize {
        unsafe { other.ptr.offset_from(self.ptr) as usize }
    }
}

impl TokenBuffer {
    pub fn new(file: codespan::FileId, tokens: Vec<Token>) -> TokenBuffer {
        TokenBuffer { file, tokens }
    }

    pub fn begin<'a>(&self, src: &'a str) -> Cursor<'a> {
        if self.tokens.is_empty() {
            struct UnsafeSyncToken(Token);
            unsafe impl Sync for UnsafeSyncToken {
            }
            static EMPTY_TOKEN: UnsafeSyncToken = UnsafeSyncToken(Token {
                span: Span::initial(),
                kind: TokenType::EOF,
            });

            Cursor {
                file: self.file,
                src,
                ptr: &EMPTY_TOKEN.0,
                start: &EMPTY_TOKEN.0,
                end: &EMPTY_TOKEN.0,
                marker: PhantomData,
            }
        } else {
            Cursor::new(self.file, src, &self.tokens[0], &self.tokens[0], &self.tokens[self.tokens.len() - 1])
        }
    }
}
