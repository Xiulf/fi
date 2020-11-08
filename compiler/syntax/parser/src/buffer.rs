use crate::attr::Attr;
use crate::ident::Ident;
use crate::literal::Literal;
use crate::punct::Punct;
use codespan::Span;
use std::marker::PhantomData;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Entry {
    Ident(Ident),
    Punct(Punct),
    Literal(Literal),
    Attr(Attr),
    LayoutStart(Span),
    LayoutSep(Span),
    LayoutEnd(Span),
    Empty,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Cursor<'a> {
    pub file: codespan::FileId,
    ptr: *const Entry,
    start: *const Entry,
    end: *const Entry,
    marker: PhantomData<&'a Entry>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TokenBuffer {
    pub file: codespan::FileId,
    pub tokens: Vec<Entry>,
}

impl<'a> Cursor<'a> {
    pub fn new(
        file: codespan::FileId,
        ptr: *const Entry,
        start: *const Entry,
        end: *const Entry,
    ) -> Cursor<'a> {
        Cursor {
            file,
            ptr,
            start,
            end,
            marker: PhantomData,
        }
    }

    pub fn span(&self) -> Span {
        self.entry().span()
    }

    pub fn entry(self) -> &'a Entry {
        unsafe { &*self.ptr }
    }

    pub fn prev(self) -> Option<&'a Entry> {
        if self.ptr > self.start {
            Some(unsafe { &*(self.ptr.offset(-1)) })
        } else {
            None
        }
    }

    pub fn bump(self) -> Cursor<'a> {
        unsafe { Cursor::new(self.file, self.ptr.offset(1), self.start, self.end) }
    }

    pub fn eof(&self) -> bool {
        self.ptr == self.end
    }

    pub fn any(self) -> Option<(Entry, Cursor<'a>)> {
        Some((self.entry().clone(), self.bump()))
    }

    pub fn ident(self) -> Option<(Ident, Cursor<'a>)> {
        match self.entry() {
            Entry::Ident(ident) => Some((ident.clone(), self.bump())),
            _ => None,
        }
    }

    pub fn punct(self) -> Option<(Punct, Cursor<'a>)> {
        match self.entry() {
            Entry::Punct(punct) => Some((punct.clone(), self.bump())),
            _ => None,
        }
    }

    pub fn literal(self) -> Option<(Literal, Cursor<'a>)> {
        match self.entry() {
            Entry::Literal(literal) => Some((literal.clone(), self.bump())),
            _ => None,
        }
    }

    pub fn attr(self) -> Option<(Attr, Cursor<'a>)> {
        match self.entry() {
            Entry::Attr(attr) => Some((attr.clone(), self.bump())),
            _ => None,
        }
    }

    pub fn lyt_start(self) -> Option<(Span, Cursor<'a>)> {
        match self.entry() {
            Entry::LayoutStart(span) => Some((*span, self.bump())),
            _ => None,
        }
    }

    pub fn lyt_sep(self) -> Option<(Span, Cursor<'a>)> {
        match self.entry() {
            Entry::LayoutSep(span) => Some((*span, self.bump())),
            _ => None,
        }
    }

    pub fn lyt_end(self) -> Option<(Span, Cursor<'a>)> {
        match self.entry() {
            Entry::LayoutEnd(span) => Some((*span, self.bump())),
            _ => None,
        }
    }

    pub fn offset(self, other: Cursor) -> usize {
        unsafe { other.ptr.offset_from(self.ptr) as usize }
    }
}

impl TokenBuffer {
    pub fn new(file: codespan::FileId, tokens: Vec<Entry>) -> TokenBuffer {
        TokenBuffer {
            file,
            tokens: tokens.into(),
        }
    }

    pub fn begin(&self) -> Cursor {
        if self.tokens.is_empty() {
            struct UnsafeSyncEntry(Entry);
            unsafe impl Sync for UnsafeSyncEntry {}
            static EMPTY_ENTRY: UnsafeSyncEntry = UnsafeSyncEntry(Entry::Empty);

            Cursor {
                file: self.file,
                ptr: &EMPTY_ENTRY.0,
                start: &EMPTY_ENTRY.0,
                end: &EMPTY_ENTRY.0,
                marker: PhantomData,
            }
        } else {
            Cursor::new(
                self.file,
                &self.tokens[0],
                &self.tokens[0],
                &self.tokens[self.tokens.len() - 1],
            )
        }
    }

    pub fn span(&self) -> Span {
        if self.tokens.is_empty() {
            Span::default()
        } else {
            let first = self.tokens[0].span();
            let last = self.tokens[self.tokens.len() - 2].span();

            Span::new(first.start(), last.end())
        }
    }

    pub fn extend(self, other: TokenBuffer) -> TokenBuffer {
        assert_eq!(self.file, other.file);

        TokenBuffer {
            file: self.file,
            tokens: self
                .tokens
                .into_iter()
                .chain(other.tokens.into_iter())
                .collect(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.tokens.is_empty() || (self.tokens.len() == 1 && self.tokens[0] == Entry::Empty)
    }
}

impl Entry {
    pub fn span(&self) -> Span {
        match self {
            Entry::Ident(ident) => ident.span,
            Entry::Punct(punct) => punct.span,
            Entry::Literal(literal) => literal.span(),
            Entry::Attr(attr) => attr.span,
            Entry::LayoutStart(span) => *span,
            Entry::LayoutSep(span) => *span,
            Entry::LayoutEnd(span) => *span,
            Entry::Empty => unreachable!(),
        }
    }

    pub fn span_mut(&mut self) -> &mut Span {
        match self {
            Entry::Ident(ident) => &mut ident.span,
            Entry::Punct(punct) => &mut punct.span,
            Entry::Literal(lit) => match lit {
                Literal::Int(lit) => &mut lit.span,
                Literal::Float(lit) => &mut lit.span,
                Literal::Char(lit) => &mut lit.span,
                Literal::String(lit) => &mut lit.span,
            },
            Entry::Attr(attr) => &mut attr.span,
            Entry::LayoutStart(span) => span,
            Entry::LayoutSep(span) => span,
            Entry::LayoutEnd(span) => span,
            Entry::Empty => unreachable!(),
        }
    }
}

impl std::fmt::Display for Entry {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Entry::Empty => write!(f, "<EOF>"),
            Entry::Ident(n) => write!(f, "{}", n),
            Entry::Punct(p) => write!(f, "{}", p.ch),
            Entry::Literal(l) => write!(f, "{}", l),
            Entry::Attr(a) => write!(f, "{}", a),
            Entry::LayoutStart(_) => write!(f, "<INDENT>"),
            Entry::LayoutSep(_) => write!(f, "<NEWLINE>"),
            Entry::LayoutEnd(_) => write!(f, "<DEDENT>"),
        }
    }
}

impl std::fmt::Display for TokenBuffer {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        for entry in &self.tokens {
            match entry {
                Entry::Empty => (),
                Entry::Ident(id) => write!(f, "{} ", id.name)?,
                Entry::Punct(p) => {
                    if let crate::punct::Spacing::Joint = &p.spacing {
                        p.ch.fmt(f)?;
                    } else {
                        write!(f, "{} ", p.ch)?;
                    }
                }
                Entry::Literal(l) => match l {
                    Literal::Int(i) => write!(f, "{} ", i.int)?,
                    Literal::Float(v) => write!(f, "{} ", v.float)?,
                    Literal::Char(c) => write!(f, "{:?} ", c.ch)?,
                    Literal::String(s) => write!(f, "{:?} ", s.text)?,
                },
                Entry::Attr(attr) => write!(f, "({}) ", attr)?,
                Entry::LayoutStart(_) => write!(f, "≫ ")?,
                Entry::LayoutSep(_) => write!(f, "∥ ")?,
                Entry::LayoutEnd(_) => write!(f, "≪ ")?,
            }
        }

        Ok(())
    }
}
