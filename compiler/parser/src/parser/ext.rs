use std::rc::Rc;

use chumsky::combinator::Map;
use chumsky::primitive::empty;
use chumsky::{BoxedParser, Parser};

use super::trivia;
use crate::error::ParseError;
use crate::event::Event;
use crate::token::SyntaxKind;

pub trait ParserExt: Parser<SyntaxKind, Event, Error = ParseError> + Sized {
    fn to_node(self, kind: SyntaxKind) -> Map<Self, ToNode, Event>;

    fn pad_ws<'a>(self) -> BoxedParser<'a, SyntaxKind, Event, ParseError>
    where
        Self: 'a;

    fn separated<'a, Sep>(self, sep: Sep, trailing: bool, min: usize) -> BoxedParser<'a, SyntaxKind, Event, ParseError>
    where
        Self: Clone + 'a,
        Sep: Parser<SyntaxKind, Event, Error = ParseError> + Clone + 'a;
}

pub trait ToEvent<O>: Parser<SyntaxKind, O, Error = ParseError> + Sized {
    fn to_event(self) -> Map<Self, fn(O) -> Event, O>;
}

#[derive(Clone)]
pub struct ToNode(Rc<dyn Fn(Event) -> Event>);

impl FnOnce<(Event,)> for ToNode {
    type Output = Event;

    extern "rust-call" fn call_once(self, _: (Event,)) -> Self::Output {
        unreachable!("cannot call call_once on ToNode")
    }
}

impl FnMut<(Event,)> for ToNode {
    extern "rust-call" fn call_mut(&mut self, _: (Event,)) -> Self::Output {
        unreachable!("cannot call call_mut on ToNode")
    }
}

impl Fn<(Event,)> for ToNode {
    extern "rust-call" fn call(&self, args: (Event,)) -> Self::Output {
        self.0.call(args)
    }
}

impl<P> ParserExt for P
where
    P: Parser<SyntaxKind, Event, Error = ParseError>,
{
    fn to_node(self, kind: SyntaxKind) -> Map<Self, ToNode, Event> {
        self.map(ToNode(Rc::new(move |e| {
            ((Event::Start(kind), e), Event::Finish).into()
        })))
    }

    fn pad_ws<'a>(self) -> BoxedParser<'a, SyntaxKind, Event, ParseError>
    where
        Self: 'a,
    {
        trivia().then(self).then(trivia()).to_event().boxed()
    }

    fn separated<'a, Sep>(self, sep: Sep, trailing: bool, min: usize) -> BoxedParser<'a, SyntaxKind, Event, ParseError>
    where
        Self: Clone + 'a,
        Sep: Parser<SyntaxKind, Event, Error = ParseError> + Clone + 'a,
    {
        let at_least = min.saturating_sub(1);
        let base = self
            .clone()
            .then(sep.clone().then(self).repeated().at_least(at_least).collect::<Event>());

        if trailing {
            let base = base.then(opt(sep)).to_event();

            if min > 0 {
                base.boxed()
            } else {
                opt(base).boxed()
            }
        } else if min > 0 {
            base.to_event().boxed()
        } else {
            opt(base.to_event()).boxed()
        }
    }
}

impl<P, O> ToEvent<O> for P
where
    P: Parser<SyntaxKind, O, Error = ParseError>,
    O: Into<Event> + 'static,
{
    fn to_event(self) -> Map<Self, fn(O) -> Event, O> {
        self.map(Into::into)
    }
}

pub fn opt(
    p: impl Parser<SyntaxKind, Event, Error = ParseError> + Clone,
) -> impl Parser<SyntaxKind, Event, Error = ParseError> + Clone {
    p.or(empty().to(Event::Empty))
}
