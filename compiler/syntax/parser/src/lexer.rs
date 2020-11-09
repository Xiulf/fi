use crate::attr::Attr;
use crate::buffer::{Entry, TokenBuffer};
use crate::error::Result;
use crate::ident::Ident;
use crate::literal::*;
use crate::punct::{Punct, Spacing};
use codespan::{ByteIndex, ByteOffset, FileId, Span};
use codespan_reporting::diagnostic::{Diagnostic, Label};
use unicode_xid::UnicodeXID;

pub struct Lexer<'a> {
    file: FileId,
    source: &'a str,
    chars: std::iter::Peekable<std::str::Chars<'a>>,
    start: ByteIndex,
    pos: ByteIndex,
    prev_dot: bool,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str, file: FileId) -> Lexer<'a> {
        Lexer {
            file,
            source,
            chars: source.chars().peekable(),
            start: ByteIndex::default(),
            pos: ByteIndex::default(),
            prev_dot: false,
        }
    }

    pub fn run(&mut self) -> std::result::Result<TokenBuffer, Vec<Diagnostic<FileId>>> {
        let mut tokens = Vec::new();
        let mut errors = Vec::new();

        while !self.eof() {
            match self.next() {
                Ok(t) => tokens.push(t),
                Err(e) => errors.push(e),
            }
        }

        if errors.is_empty() {
            if let Some(Entry::Empty) = tokens.last() {
            } else {
                tokens.push(Entry::Empty);
            }

            Ok(TokenBuffer::new(self.file, tokens))
        } else {
            Err(errors)
        }
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
                '-' if self.peek_n(1) == '-' && self.peek_n(2) != '|' => {
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

        let text2 = text.trim_start();
        let text3 = text2.trim_end().to_string();
        let end = text2.len() - text3.len();

        Ok(Entry::Attr(Attr {
            span: Span::new(self.start, self.pos - ByteOffset::from(end as i64)),
            text: text3,
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
            return Err(Diagnostic::error()
                .with_message("invalid character literal")
                .with_code("E0001")
                .with_labels(vec![Label::primary(self.file, self.span())]));
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
            let val: f64 = self.source[self.start.to_usize()..self.pos.to_usize() - ty.len()]
                .replace('_', "")
                .parse()
                .map_err(|_| {
                    Diagnostic::error()
                        .with_message("invalid floating point literal")
                        .with_code("E0002")
                        .with_labels(vec![Label::primary(self.file, self.span())])
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
                        let span =
                            Span::new(span.end() - ByteOffset::from_str_len(&ty), span.end());

                        return Err(Diagnostic::error()
                            .with_message("invalid number type")
                            .with_code("E0003")
                            .with_labels(vec![Label::primary(self.file, span)]));
                    }
                },
            })))
        } else {
            let text =
                self.source[self.start.to_usize()..self.pos.to_usize() - ty.len()].replace('_', "");
            let val = if text.contains('x') && text.find('x') == Some(1) {
                u128::from_str_radix(&text[2..], 16)
            } else if text.contains('b') && text.find('b') == Some(1) {
                u128::from_str_radix(&text[2..], 2)
            } else {
                u128::from_str_radix(&text, 10)
            }
            .map_err(|_| {
                Diagnostic::error()
                    .with_message("invalid integer literal")
                    .with_code("E0004")
                    .with_labels(vec![Label::primary(self.file, self.span())])
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
                        let span =
                            Span::new(span.end() - ByteOffset::from_str_len(&ty), span.end());

                        return Err(Diagnostic::error()
                            .with_message("invalid number type")
                            .with_code("E0003")
                            .with_labels(vec![Label::primary(self.file, span)]));
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
                        None => Err(Diagnostic::error()
                            .with_message("invalid character escape")
                            .with_code("E0005")
                            .with_labels(vec![Label::primary(self.file, self.span())])),
                    })
                    .map_err(|_| {
                        Diagnostic::error()
                            .with_message("invalid character escape")
                            .with_code("E0005")
                            .with_labels(vec![Label::primary(self.file, self.span())])
                    })?
            }
            _ => Err(Diagnostic::error()
                .with_message("invalid character escape")
                .with_code("E0005")
                .with_labels(vec![Label::primary(self.file, self.span())])),
        }
    }

    fn span(&self) -> Span {
        Span::new(self.start, self.pos)
    }

    fn text(&self) -> &str {
        &self.source[self.start.to_usize()..self.pos.to_usize()]
    }

    fn eof(&mut self) -> bool {
        self.peek() == '\0'
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
        }
    }
}
