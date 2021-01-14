use crate::buffer::Cursor;
use crate::token::Token;
use codespan::Span;
use std::cell::Cell;
use std::marker::PhantomData;

pub trait Parse: Sized {
    fn parse(input: ParseStream) -> Result<Self>;
}

pub type ParseStream<'a> = &'a ParseBuffer<'a>;

pub struct ParseBuffer<'a> {
    cell: Cell<Cursor<'static>>,
    prev_span: Cell<Span>,
    _marker: PhantomData<Cursor<'a>>,
}

pub struct StepCursor<'c, 'a> {
    span: Span,
    cursor: Cursor<'c>,
    _marker: PhantomData<fn(Cursor<'c>) -> Cursor<'a>>,
}

pub type Result<T> = std::result::Result<T, ParseError>;

pub struct ParseError {
    pub span: Span,
    pub expected: String,
}

impl<'a> ParseBuffer<'a> {
    pub fn new(cursor: Cursor<'a>, start: Span) -> Self {
        ParseBuffer {
            cell: Cell::new(unsafe { std::mem::transmute(cursor) }),
            prev_span: Cell::new(start),
            _marker: PhantomData,
        }
    }

    pub fn span(&self) -> Span {
        if self.is_empty() {
            self.prev_span.get()
        } else {
            self.cursor().span()
        }
    }

    pub fn prev_span(&self) -> Span {
        self.prev_span.get()
    }

    pub fn bump(&self) {
        let bump = self.cell.get().bump();

        self.cell.set(bump);
    }

    pub fn restore(&self, save: Cursor<'a>) {
        self.cell.set(unsafe { std::mem::transmute(save) });
    }

    pub fn parse<T: Parse>(&self) -> Result<T> {
        T::parse(self)
    }

    pub fn peek<T: Token>(&self) -> bool {
        T::peek(self.cursor())
    }

    pub fn peek2<T: Token>(&self) -> bool {
        let ahead = self.fork();

        skip(&ahead) && ahead.peek::<T>()
    }

    pub fn is_empty(&self) -> bool {
        self.cursor().eof()
    }

    pub fn cursor(&self) -> Cursor<'a> {
        self.cell.get()
    }

    pub fn fork(&self) -> Self {
        ParseBuffer {
            cell: self.cell.clone(),
            prev_span: self.prev_span.clone(),
            _marker: PhantomData,
        }
    }

    pub fn step<F: for<'c> FnOnce(StepCursor<'c, 'a>) -> Result<(R, Cursor<'c>)>, R>(&self, f: F) -> Result<R> {
        let (node, rest) = f(StepCursor {
            span: self.span(),
            cursor: self.cell.get(),
            _marker: PhantomData,
        })?;

        self.prev_span.set(self.span());
        self.cell.set(rest);

        Ok(node)
    }
}

fn skip(input: ParseStream) -> bool {
    input
        .step(|cursor: StepCursor| {
            if let Some((_, rest)) = cursor.any() {
                Ok((true, rest))
            } else {
                Ok((false, *cursor))
            }
        })
        .unwrap_or_default()
}

impl<'c, 'a> StepCursor<'c, 'a> {
    pub fn span(&self) -> Span {
        self.span
    }
}

impl<'c, 'a> std::ops::Deref for StepCursor<'c, 'a> {
    type Target = Cursor<'c>;

    fn deref(&self) -> &Self::Target {
        &self.cursor
    }
}

impl<T: Parse> Parse for Box<T> {
    fn parse(input: ParseStream) -> Result<Self> {
        input.parse().map(Box::new)
    }
}
