use crate::buffer::{Cursor, TokenBuffer};
use crate::error::Result;
use crate::literal::Literal;
use crate::token::Token;
use diagnostics::{Diagnostic, Reporter, Severity, Span};
use std::cell::{Cell, RefCell};
use std::marker::PhantomData;

pub trait Parse<D = ()>: Sized {
    fn parse(input: ParseStream<D>) -> Result<Self>;
}

pub trait ToTokens {
    fn to_tokens(&self) -> TokenBuffer;
}

pub type ParseStream<'a, D = ()> = &'a ParseBuffer<'a, D>;

pub struct ParseBuffer<'a, D = ()> {
    pub data: D,
    pub reporter: &'a Reporter,
    pub node_id: Cell<usize>,
    cell: Cell<Cursor<'static>>,
    prev_span: RefCell<Span>,
    marker: PhantomData<Cursor<'a>>,
}

pub struct StepCursor<'c, 'a> {
    span: Span,
    cursor: Cursor<'c>,
    marker: PhantomData<fn(Cursor<'c>) -> Cursor<'a>>,
}

impl<'a, D> ParseBuffer<'a, D> {
    pub fn new(
        cursor: Cursor<'a>,
        reporter: &'a Reporter,
        data: D,
        start: Span,
    ) -> ParseBuffer<'a, D> {
        ParseBuffer {
            data,
            reporter,
            cell: Cell::new(unsafe { std::mem::transmute(cursor) }),
            node_id: Cell::new(0),
            prev_span: RefCell::new(start),
            marker: PhantomData,
        }
    }

    pub fn span(&self) -> Span {
        if self.is_empty() {
            self.prev_span.borrow().clone()
        } else {
            self.cursor().span()
        }
    }

    pub fn prev_span(&self) -> Span {
        self.prev_span.borrow().clone()
    }

    pub fn error<T, I: Into<String>>(&self, msg: I, code: impl Into<Option<u16>>) -> Result<T> {
        Err(Diagnostic::new(Severity::Error, code, msg).label(
            Severity::Error,
            self.span(),
            None::<String>,
        ))
    }

    pub fn error_at<T, I: Into<String>>(
        &self,
        msg: I,
        span: Span,
        code: impl Into<Option<u16>>,
    ) -> Result<T> {
        Err(Diagnostic::new(Severity::Error, code, msg).label(
            Severity::Error,
            span,
            None::<String>,
        ))
    }

    pub fn bump(&self) {
        let bump = self.cell.get().bump();
        self.cell.set(bump);
    }

    pub fn restore(&self, save: Cursor<'a>) {
        self.cell.set(unsafe { std::mem::transmute(save) });
    }

    pub fn parse<T: Parse<D>>(&self) -> Result<T> {
        T::parse(self)
    }

    pub fn call<T>(&self, f: fn(ParseStream<D>) -> Result<T>) -> Result<T> {
        f(self)
    }

    pub fn peek<T: Token>(&self) -> bool {
        T::peek(self.cursor())
    }

    pub fn peek2<T: Token>(&self) -> bool
    where
        D: Copy,
    {
        let ahead = self.fork();
        skip(&ahead) && ahead.peek::<T>()
    }

    pub fn peek3<T: Token>(&self) -> bool
    where
        D: Copy,
    {
        let ahead = self.fork();
        skip(&ahead) && skip(&ahead) && ahead.peek::<T>()
    }

    pub fn peek4<T: Token>(&self) -> bool
    where
        D: Copy,
    {
        let ahead = self.fork();
        skip(&ahead) && skip(&ahead) && skip(&ahead) && ahead.peek::<T>()
    }

    pub fn is_empty(&self) -> bool {
        self.cursor().eof()
    }

    pub fn fork(&self) -> ParseBuffer<'a, D>
    where
        D: Copy,
    {
        ParseBuffer {
            data: self.data,
            reporter: self.reporter,
            cell: self.cell.clone(),
            prev_span: self.prev_span.clone(),
            node_id: self.node_id.clone(),
            marker: PhantomData,
        }
    }

    pub fn step<F: for<'c> FnOnce(StepCursor<'c, 'a>) -> Result<(R, Cursor<'c>)>, R>(
        &self,
        f: F,
    ) -> Result<R> {
        let (node, rest) = f(StepCursor {
            span: self.span(),
            cursor: self.cell.get(),
            marker: PhantomData,
        })?;
        *self.prev_span.borrow_mut() = self.span();
        self.cell.set(rest);
        Ok(node)
    }

    pub fn cursor(&self) -> Cursor<'a> {
        self.cell.get()
    }

    pub fn steps(&self, other: &ParseBuffer<D>) -> usize {
        self.cursor().offset(other.cursor())
    }
}

fn skip<D>(input: ParseStream<D>) -> bool {
    input
        .step(|cursor| {
            if let Some((_token, rest)) = cursor.any() {
                Ok((true, rest))
            } else {
                Ok((false, *cursor))
            }
        })
        .unwrap_or_default()
}

impl<'c, 'a> StepCursor<'c, 'a> {
    pub fn error<T: Into<String>>(self, message: T) -> Diagnostic {
        Diagnostic::new(Severity::Error, 0001, message).label(
            Severity::Error,
            self.span,
            None::<String>,
        )
    }
}

impl<'c, 'a> std::ops::Deref for StepCursor<'c, 'a> {
    type Target = Cursor<'c>;

    fn deref(&self) -> &Cursor<'c> {
        &self.cursor
    }
}

impl<T: Parse<D>, D> Parse<D> for Box<T> {
    fn parse(input: ParseStream<D>) -> Result<Box<T>> {
        input.parse().map(Box::new)
    }
}

impl<T: Parse<D> + Token, D> Parse<D> for Option<T> {
    fn parse(input: ParseStream<D>) -> Result<Option<T>> {
        if T::peek(input.cursor()) {
            Ok(Some(T::parse(input)?))
        } else {
            Ok(None)
        }
    }
}

impl<T: Parse<D>, D: Copy> Parse<D> for Vec<T> {
    fn parse(input: ParseStream<D>) -> Result<Vec<T>> {
        let mut items = Vec::new();
        while !input.is_empty() && input.fork().parse::<T>().is_ok() {
            items.push(input.parse::<T>()?);
        }
        Ok(items)
    }
}

impl<D> Parse<D> for crate::buffer::Entry {
    fn parse(input: ParseStream<D>) -> Result<Self> {
        input.step(|cursor| match cursor.any() {
            Some((entry, rest)) => Ok((entry, rest)),
            _ => Err(cursor.error("expected any token")),
        })
    }
}

impl<D> Parse<D> for String {
    fn parse(input: ParseStream<D>) -> Result<String> {
        input.step(|cursor| match cursor.literal() {
            Some((Literal::String(s), rest)) => Ok((s.text, rest)),
            _ => Err(cursor.error("expected a string literal")),
        })
    }
}

impl<T: ToTokens> ToTokens for Option<T> {
    fn to_tokens(&self) -> TokenBuffer {
        match self {
            Some(t) => t.to_tokens(),
            None => TokenBuffer::new(Vec::new()),
        }
    }
}

impl<T: ToTokens> ToTokens for Box<T> {
    fn to_tokens(&self) -> TokenBuffer {
        (**self).to_tokens()
    }
}

impl<T: ToTokens> ToTokens for Vec<T> {
    fn to_tokens(&self) -> TokenBuffer {
        self.into_iter()
            .fold(TokenBuffer::new(Vec::new()), |acc, t| {
                acc.extend(t.to_tokens())
            })
    }
}
