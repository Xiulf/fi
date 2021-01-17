#![feature(or_patterns)]

mod layout;

use codespan::{ByteIndex, ByteOffset, Span};
use std::collections::VecDeque;
use std::iter::Peekable;
use std::str::Chars;
use unicode_xid::UnicodeXID;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Token {
    pub span: Span,
    pub kind: TokenType,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TokenType {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
    LeftArrow,
    RightArrow,
    FatArrow,
    DoubleColon,
    DoubleDot,
    Dot,
    Comma,
    Pipe,
    Equals,
    Qmark,
    At,
    Underscore,
    Operator,
    Symbol,
    Name,
    Int,
    Float,
    String,
    RawString,
    Char,
    LayoutStart,
    LayoutSep,
    LayoutEnd,
    EOF,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LayoutDelim {
    Root,
    TopDecl,
    TopDeclHead,
    Attr,
    Prop,
    DeclGuard,
    Case,
    CaseBinders,
    CaseGuard,
    Paren,
    Brace,
    Square,
    If,
    Then,
    Forall,
    Let,
    LetStmt,
    Where,
    Of,
    Do,
}

#[derive(Debug, Clone, Copy)]
struct Pos(usize, usize, ByteIndex);

#[derive(Debug)]
pub enum LexicalError {
    UnknownChar(ByteIndex, char),
    InvalidCharLiteral(Span),
    UnterminatedString(Span),
    InvalidEscape(Span),
}

pub type Result<T> = std::result::Result<T, LexicalError>;

pub struct Lexer<'src> {
    source: &'src str,
    chars: Peekable<Chars<'src>>,
    start: ByteIndex,
    pos: ByteIndex,
    line: usize,
    col: usize,
    queue: VecDeque<Token>,
    stack: Vec<(Pos, LayoutDelim)>,
}

impl<'a, 'src> Iterator for &'a mut Lexer<'src> {
    type Item = Result<Token>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.done() {
            None
        } else {
            Some(Lexer::next(self))
        }
    }
}

impl<'src> Lexer<'src> {
    pub fn new(source: &'src str) -> Lexer<'src> {
        Lexer {
            source,
            chars: source.chars().peekable(),
            start: ByteIndex::default(),
            pos: ByteIndex::default(),
            line: 0,
            col: 0,
            queue: VecDeque::with_capacity(2),
            stack: vec![(Pos(0, 0, ByteIndex::default()), LayoutDelim::Root)],
        }
    }

    pub fn next(&mut self) -> Result<Token> {
        self.skip();

        if let Some(token) = self.queue.pop_front() {
            return Ok(token);
        }

        if self.eof() {
            self.unwind();

            return self.next();
        }

        self.start = self.pos;

        let start = Pos(self.line, self.col, self.start);
        let ch = self.peek();

        self.advance();

        let token = match ch {
            '(' if bsearch_range_table(self.peek(), OP_START) => self.symbol(),
            '(' => Ok(self.token(TokenType::LeftParen)),
            ')' => Ok(self.token(TokenType::RightParen)),
            '{' => Ok(self.token(TokenType::LeftBrace)),
            '}' => Ok(self.token(TokenType::RightBrace)),
            '[' => Ok(self.token(TokenType::LeftBracket)),
            ']' => Ok(self.token(TokenType::RightBracket)),
            '<' if self.peek() == '-' => {
                self.advance();
                Ok(self.token(TokenType::LeftArrow))
            }
            '-' if self.peek() == '>' => {
                self.advance();
                Ok(self.token(TokenType::RightArrow))
            }
            '=' if self.peek() == '>' => {
                self.advance();
                Ok(self.token(TokenType::FatArrow))
            }
            ':' if self.peek() == ':' => {
                self.advance();
                Ok(self.token(TokenType::DoubleColon))
            }
            '.' if self.peek() == '.' => {
                self.advance();
                Ok(self.token(TokenType::DoubleDot))
            }
            '.' => Ok(self.token(TokenType::Dot)),
            ',' => Ok(self.token(TokenType::Comma)),
            '|' if bsearch_range_table(self.peek(), OP_CONT) => self.operator(),
            '|' => Ok(self.token(TokenType::Pipe)),
            '=' if bsearch_range_table(self.peek(), OP_CONT) => self.operator(),
            '=' => Ok(self.token(TokenType::Equals)),
            '?' if bsearch_range_table(self.peek(), OP_CONT) => self.operator(),
            '?' => Ok(self.token(TokenType::Qmark)),
            '@' if bsearch_range_table(self.peek(), OP_CONT) => self.operator(),
            '@' => Ok(self.token(TokenType::At)),
            '_' => Ok(self.token(TokenType::Underscore)),
            c if c.is_xid_start() => self.name(),
            c if bsearch_range_table(c, OP_START) => self.operator(),
            '0'..='9' => self.number(ch),
            'r' if self.peek() == '"' => self.string(true),
            '"' => self.string(false),
            '\'' => self.char(),
            c => Err(LexicalError::UnknownChar(self.start, c)),
        }?;

        self.do_layout(start, token);
        self.next()
    }

    fn skip(&mut self) {
        self.start = self.pos;

        while !self.eof() {
            match self.peek() {
                c if c.is_whitespace() => self.advance(),
                '-' if self.peek_n(1) == '-' => {
                    self.advance();

                    while !self.eof() && self.peek() != '\n' {
                        self.advance();
                    }
                }
                _ => break,
            }
        }
    }

    fn name(&mut self) -> Result<Token> {
        while self.peek().is_xid_continue() {
            self.advance();
        }

        Ok(self.token(TokenType::Name))
    }

    fn operator(&mut self) -> Result<Token> {
        while bsearch_range_table(self.peek(), OP_CONT) {
            self.advance();
        }

        Ok(self.token(TokenType::Operator))
    }

    fn symbol(&mut self) -> Result<Token> {
        self.advance();

        while bsearch_range_table(self.peek(), OP_CONT) {
            self.advance();
        }

        if self.peek() == ')' {
            self.advance();

            Ok(self.token(TokenType::Symbol))
        } else {
            Err(LexicalError::UnknownChar(self.pos, self.peek()))
        }
    }

    fn number(&mut self, start: char) -> Result<Token> {
        enum NumKind {
            Decimal,
            Hexadecimal,
            Octal,
            Binary,
        }

        let mut kind = NumKind::Decimal;

        if start == '0' {
            match self.peek() {
                'x' => {
                    self.advance();
                    kind = NumKind::Hexadecimal;
                }
                'o' => {
                    self.advance();
                    kind = NumKind::Octal;
                }
                'b' => {
                    self.advance();
                    kind = NumKind::Binary;
                }
                _ => {}
            }
        }

        while let '0'..='9' = self.peek() {
            self.advance();
        }

        let mut float = false;

        if matches!(kind, NumKind::Decimal) && self.peek() == '.' {
            self.advance();
            float = true;
        }

        while let '0'..='9' = self.peek() {
            self.advance();
        }

        if float && self.peek() == 'e' {
            self.advance();

            if let '+' | '-' = self.peek() {
                self.advance();

                while let '0'..='9' = self.peek() {
                    self.advance();
                }
            } else {
                return Err(LexicalError::UnknownChar(self.pos, self.peek()));
            }
        }

        if float {
            Ok(self.token(TokenType::Float))
        } else {
            Ok(self.token(TokenType::Int))
        }
    }

    fn char(&mut self) -> Result<Token> {
        match self.peek() {
            '\\' => {
                self.advance();
                self.escape()?;
            }
            '\'' => return Err(LexicalError::InvalidCharLiteral(self.span())),
            _ => self.advance(),
        }

        if self.peek() == '\'' {
            self.advance();
            Ok(self.token(TokenType::Char))
        } else {
            self.advance();
            Err(LexicalError::InvalidCharLiteral(self.span()))
        }
    }

    fn string(&mut self, raw: bool) -> Result<Token> {
        while !self.eof() {
            match self.peek() {
                '"' => break,
                '\\' if !raw => {
                    self.advance();
                    self.escape()?;
                }
                _ => self.advance(),
            }
        }

        if self.peek() == '"' {
            self.advance();

            if raw {
                Ok(self.token(TokenType::RawString))
            } else {
                Ok(self.token(TokenType::String))
            }
        } else {
            self.advance();
            Err(LexicalError::UnterminatedString(self.span()))
        }
    }

    fn escape(&mut self) -> Result<char> {
        let start = self.pos - ByteOffset::from_char_len('\\');
        let ch = self.peek();

        self.advance();

        match ch {
            '"' => Ok('"'),
            '\'' => Ok('\''),
            '\\' => Ok('\\'),
            'n' => Ok('\n'),
            'r' => Ok('\r'),
            't' => Ok('\t'),
            '0' => Ok('\0'),
            _ => Err(LexicalError::InvalidEscape(Span::new(start, self.pos))),
        }
    }

    fn token(&self, kind: TokenType) -> Token {
        Token { span: self.span(), kind }
    }

    fn span(&self) -> Span {
        Span::new(self.start, self.pos)
    }

    pub fn text(&self, span: Span) -> &str {
        &self.source[span.start().to_usize()..span.end().to_usize()]
    }

    pub fn done(&self) -> bool {
        self.eof() && self.stack.is_empty() && self.queue.is_empty()
    }

    fn eof(&self) -> bool {
        self.pos.to_usize() >= self.source.len()
    }

    fn peek(&mut self) -> char {
        self.chars.peek().copied().unwrap_or('\0')
    }

    fn peek_n(&self, n: usize) -> char {
        let mut chars = self.chars.clone();
        let mut ch = chars.next();

        for _ in 0..n {
            ch = chars.next();
        }

        ch.unwrap_or('\0')
    }

    fn advance(&mut self) {
        if let Some(ch) = self.chars.next() {
            self.pos += ByteOffset::from_char_len(ch);
            self.col += 1;

            if ch == '\n' {
                self.col = 0;
                self.line += 1;
            }
        }
    }
}

const OP_START: &[(char, char)] = &[
    ('\u{21}', '\u{21}'),
    ('\u{23}', '\u{26}'),
    ('\u{2A}', '\u{2B}'),
    ('\u{2D}', '\u{2D}'),
    ('\u{2F}', '\u{2F}'),
    ('\u{3A}', '\u{3A}'),
    ('\u{3C}', '\u{3F}'),
    ('\u{5C}', '\u{5C}'),
    ('\u{5E}', '\u{5E}'),
    ('\u{7C}', '\u{7C}'),
    ('\u{7E}', '\u{7E}'),
];

const OP_CONT: &[(char, char)] = &[
    ('\u{21}', '\u{21}'),
    ('\u{23}', '\u{26}'),
    ('\u{2A}', '\u{2B}'),
    ('\u{2D}', '\u{2D}'),
    ('\u{2F}', '\u{2F}'),
    ('\u{3A}', '\u{3A}'),
    ('\u{3C}', '\u{3F}'),
    ('\u{40}', '\u{40}'),
    ('\u{5C}', '\u{5C}'),
    ('\u{5E}', '\u{5E}'),
    ('\u{7C}', '\u{7C}'),
    ('\u{7E}', '\u{7E}'),
];

fn bsearch_range_table(c: char, r: &[(char, char)]) -> bool {
    use core::cmp::Ordering::{Equal, Greater, Less};

    r.binary_search_by(|&(lo, hi)| {
        if lo > c {
            Greater
        } else if hi < c {
            Less
        } else {
            Equal
        }
    })
    .is_ok()
}
