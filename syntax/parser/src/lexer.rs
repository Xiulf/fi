use crate::attr::Attr;
use crate::buffer::{Entry, TokenBuffer};
use crate::error::Result;
use crate::ident::Ident;
use crate::literal::*;
use crate::punct::{Punct, Spacing};
use diagnostics::{Diagnostic, FileId, Position, Reporter, Severity, Span};
use unicode_xid::UnicodeXID;

pub struct Lexer<'a> {
    reporter: &'a Reporter,
    file: FileId,
    source: &'a str,
    chars: std::iter::Peekable<std::str::CharIndices<'a>>,
    start: Position,
    pos: Position,
    prev_dot: bool,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str, file: FileId, reporter: &'a Reporter) -> Lexer<'a> {
        Lexer {
            reporter,
            file,
            source,
            chars: source.char_indices().peekable(),
            start: Position::default(),
            pos: Position::default(),
            prev_dot: false,
        }
    }

    pub fn run(&mut self) -> TokenBuffer {
        let mut tokens = Vec::new();

        while !self.eof() {
            match self.next() {
                Ok(t) => tokens.push(t),
                Err(e) => self.reporter.add(e),
            }
        }

        if let Some(Entry::Empty) = tokens.last() {
        } else {
            tokens.push(Entry::Empty);
        }

        TokenBuffer::new(tokens)
    }

    fn next(&mut self) -> Result<Entry> {
        self.skip();
        self.start = self.pos;

        let ch = self.peek();

        self.advance();

        match ch {
            'r' if self.peek() == '"' => {
                self.advance();
                self.string(true)
            }
            '"' => self.string(false),
            '\'' => self.char(),
            '0'..='9' => self.number(),
            'a'..='z' | 'A'..='Z' | '_' => self.ident(),
            c if c.is_xid_start() => self.ident(),
            '-' if self.peek() == '-' && self.peek_n(1) == '-' && self.peek_n(2) == '|' => {
                self.ml_attr()
            }
            '-' if self.peek() == '-' && self.peek_n(1) == '|' => self.attr(),
            '\0' => Ok(Entry::Empty),
            ch => {
                let spacing = if match self.peek() {
                    'r' if self.peek_n(1) == '"' => false,
                    '"' => false,
                    '\'' => false,
                    '0'..='9' => false,
                    'a'..='z' | 'A'..='Z' | '_' => false,
                    c if c.is_whitespace() => false,
                    _ => true,
                } {
                    Spacing::Joint
                } else {
                    Spacing::Alone
                };

                self.prev_dot = ch == '.';

                Ok(Entry::Punct(Punct {
                    span: self.span(),
                    ch,
                    spacing,
                }))
            }
        }
    }

    fn skip(&mut self) {
        while !self.eof() {
            match self.peek() {
                c if c.is_whitespace() => self.advance(),
                '-' if self.peek_n(1) == '-' && self.peek_n(2) == '-' && self.peek_n(3) != '|' => {
                    self.advance();

                    while !self.eof()
                        && !(self.peek() == '-' && self.peek_n(1) == '-' && self.peek_n(2) == '-')
                    {
                        self.advance();
                    }
                }
                '-' if self.peek_n(1) == '-' && !matches!(self.peek_n(2), '|' | '-') => {
                    self.advance();

                    while !self.eof() && self.peek() != '\n' {
                        self.advance();
                    }
                }
                _ => break,
            }
        }
    }

    fn attr(&mut self) -> Result<Entry> {
        let mut text = String::new();

        self.advance();
        self.advance();

        while !self.eof() {
            let c = self.peek();

            if let '\r' | '\n' = c {
                break;
            } else {
                text.push(c);
                self.advance();
            }
        }

        Ok(Entry::Attr(Attr {
            span: self.span(),
            text,
        }))
    }

    fn ml_attr(&mut self) -> Result<Entry> {
        let mut text = String::new();

        self.advance();
        self.advance();
        self.advance();

        while !self.eof() {
            let c = self.peek();

            if c == '-' && self.peek_n(1) == '-' && self.peek_n(2) == '-' {
                self.advance();
                self.advance();
                self.advance();

                break;
            } else {
                text.push(c);
                self.advance();
            }
        }

        Ok(Entry::Attr(Attr {
            span: self.span(),
            text,
        }))
    }

    fn ident(&mut self) -> Result<Entry> {
        while !self.eof() {
            match self.peek() {
                'a'..='z' | 'A'..='Z' | '0'..='9' | '_' => self.advance(),
                c if c.is_xid_continue() => self.advance(),
                _ => break,
            }
        }

        Ok(Entry::Ident(Ident {
            span: self.span(),
            name: self.text().to_string(),
        }))
    }

    fn string(&mut self, raw: bool) -> Result<Entry> {
        let mut text = String::new();

        while !self.eof() {
            match self.peek() {
                '"' => {
                    self.advance();
                    break;
                }
                '\\' if !raw => {
                    self.advance();
                    text.push(self.escape()?);
                }
                ch => {
                    self.advance();
                    text.push(ch);
                }
            }
        }

        Ok(Entry::Literal(Literal::String(StringLiteral {
            span: self.span(),
            text,
        })))
    }

    fn char(&mut self) -> Result<Entry> {
        let ch = match self.peek() {
            '\\' => {
                self.advance();
                self.escape()?
            }
            ch => {
                self.advance();
                ch
            }
        };

        if self.peek() == '\'' {
            self.advance();
        } else {
            return Err(
                Diagnostic::new(Severity::Error, None, "invalid character literal").label(
                    Severity::Error,
                    self.span(),
                    "test",
                ),
            );
        }

        Ok(Entry::Literal(Literal::Char(CharLiteral {
            span: self.span(),
            ch,
        })))
    }

    fn number(&mut self) -> Result<Entry> {
        let mut count = 0;

        while !self.eof() {
            match self.peek() {
                '_' if self.peek_n(1) >= '0' && self.peek_n(1) <= '9' => self.advance(),
                '0'..='9' => self.advance(),
                _ => break,
            }

            count += 1;
        }

        let mut float = false;

        if !self.prev_dot && self.peek() == '.' && self.peek_n(1) != '.' {
            if let '0'..='9' = self.peek_n(1) {
                self.advance();
                float = true;

                while !self.eof() {
                    match self.peek() {
                        '_' if self.peek_n(1) >= '0' && self.peek_n(1) <= '9' => self.advance(),
                        '0'..='9' => self.advance(),
                        _ => break,
                    }
                }

                if self.peek() == 'e' && (self.peek_n(1) == '+' || self.peek_n(1) == '-') {
                    self.advance();
                    self.advance();

                    while !self.eof() {
                        match self.peek() {
                            '0'..='9' => self.advance(),
                            _ => break,
                        }
                    }
                }
            }
        } else if self.peek() == 'x' && count == 0 {
            self.advance();

            while !self.eof() {
                match self.peek() {
                    '_' if self.peek_n(1) >= '0' && self.peek_n(1) <= '9' => self.advance(),
                    '0'..='9' | 'a'..='z' | 'A'..='Z' => self.advance(),
                    _ => break,
                }
            }
        } else if self.peek() == 'b' && count == 0 {
            self.advance();

            while !self.eof() {
                match self.peek() {
                    '_' if self.peek_n(1) >= '0' && self.peek_n(1) <= '9' => self.advance(),
                    '0' | '1' => self.advance(),
                    _ => break,
                }
            }
        }

        let mut ty = String::new();

        while !self.eof() {
            match self.peek() {
                c @ 'a'..='z' | c @ '0'..='9' => {
                    ty.push(c);
                    self.advance();
                }
                _ => break,
            }
        }

        if float || ty == "f32" || ty == "f64" || ty == "float" {
            let val: f64 =
                self.source[self.start.offset..self.pos.offset - ty.len()]
                    .replace('_', "")
                    .parse()
                    .map_err(|_| {
                        Diagnostic::new(Severity::Error, None, "Invalid floating point literal")
                            .label(Severity::Error, self.span(), None::<String>)
                    })?;
            Ok(Entry::Literal(Literal::Float(FloatLiteral {
                span: self.span(),
                float: val.to_bits(),
                ty: match ty.as_str() {
                    "f32" => FloatType::F32,
                    "f64" => FloatType::F64,
                    "" => FloatType::Unknown,
                    _ => {
                        let span = self.span();
                        let span = Span {
                            start: Position {
                                line: span.end.line,
                                col: span.end.col - ty.len(),
                                offset: span.end.offset - ty.len(),
                            },
                            ..span
                        };

                        return Err(
                            Diagnostic::new(Severity::Error, None, "Invalid number type").label(
                                Severity::Error,
                                span,
                                None::<String>,
                            ),
                        );
                    }
                },
            })))
        } else {
            let text = self.source[self.start.offset..self.pos.offset - ty.len()].replace('_', "");
            let val = if text.contains('x') && text.find('x') == Some(1) {
                u128::from_str_radix(&text[2..], 16)
            } else if text.contains('b') && text.find('b') == Some(1) {
                u128::from_str_radix(&text[2..], 2)
            } else {
                u128::from_str_radix(&text, 10)
            }
            .map_err(|_| {
                Diagnostic::new(Severity::Error, None, "Invalid integer literal").label(
                    Severity::Error,
                    self.span(),
                    None::<String>,
                )
            })?;

            Ok(Entry::Literal(Literal::Int(IntLiteral {
                span: self.span(),
                int: val,
                ty: match ty.as_str() {
                    "u8" => IntType::U8,
                    "u16" => IntType::U16,
                    "u32" => IntType::U32,
                    "u64" => IntType::U64,
                    "u128" => IntType::U128,
                    "i8" => IntType::I8,
                    "i16" => IntType::I16,
                    "i32" => IntType::I32,
                    "i64" => IntType::I64,
                    "i128" => IntType::I128,
                    "" => IntType::Unknown,
                    _ => {
                        let span = self.span();
                        let span = Span {
                            start: Position {
                                line: span.end.line,
                                col: span.end.col - ty.len(),
                                offset: span.end.offset - ty.len(),
                            },
                            ..span
                        };

                        return Err(
                            Diagnostic::new(Severity::Error, None, "Invalid number type").label(
                                Severity::Error,
                                span,
                                None::<String>,
                            ),
                        );
                    }
                },
            })))
        }
    }

    fn escape(&mut self) -> Result<char> {
        let ch = self.peek();

        self.advance();

        match ch {
            '"' => Ok('"'),
            '\'' => Ok('\''),
            'n' => Ok('\n'),
            'r' => Ok('\r'),
            't' => Ok('\t'),
            '0' => Ok('\0'),
            'u' => {
                let mut num = String::new();

                while !self.eof() {
                    match self.peek() {
                        c @ '0'..='9' | c @ 'a'..='z' | c @ 'A'..='Z' => {
                            self.advance();
                            num.push(c);
                        }
                        _ => break,
                    }
                }

                u32::from_str_radix(&num, 16)
                    .map(|n| match std::char::from_u32(n) {
                        Some(c) => Ok(c),
                        None => {
                            Err(
                                Diagnostic::new(Severity::Error, None, "invalid character escape")
                                    .label(Severity::Error, self.span(), None::<String>),
                            )
                        }
                    })
                    .map_err(|_| {
                        Diagnostic::new(Severity::Error, None, "invalid character escape").label(
                            Severity::Error,
                            self.span(),
                            None::<String>,
                        )
                    })?
            }
            _ => Err(
                Diagnostic::new(Severity::Error, None, "invalid character escape").label(
                    Severity::Error,
                    self.span(),
                    None::<String>,
                ),
            ),
        }
    }

    fn span(&self) -> Span {
        Span {
            start: self.start,
            end: self.pos,
            file: self.file,
        }
    }

    fn text(&self) -> &str {
        &self.source[self.start.offset..self.pos.offset]
    }

    fn eof(&mut self) -> bool {
        self.peek() == '\0'
    }

    fn peek(&mut self) -> char {
        self.chars.peek().map(|c| c.1).unwrap_or('\0')
    }

    fn peek_n(&self, n: usize) -> char {
        let mut chars = self.chars.clone();
        let mut ch = chars.next();

        for _ in 0..n {
            ch = chars.next();
        }

        ch.map(|(_, c)| c).unwrap_or('\0')
    }

    fn advance(&mut self) {
        if let Some((idx, ch)) = self.chars.next() {
            if ch == '\n' {
                self.pos.line += 1;
                self.pos.col = 0;
            } else {
                self.pos.col += 1;
            }

            self.pos.offset = idx + ch.len_utf8();
        }
    }
}
