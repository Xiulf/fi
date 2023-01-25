use std::ops::Range;
use std::rc::Rc;

use chumsky::combinator::{Map, Repeated, Then};
use chumsky::prelude::*;
use chumsky::{Parser, Stream};
use text_size::TextSize;

use crate::error::ParseError;
use crate::event::Event;
use crate::lexer::Lexer;
use crate::token::SyntaxKind::{self, *};

pub fn parse(text: &str) -> (Option<Event>, Vec<ParseError>) {
    let tokens = tokens_with_range(text);
    let stream = Stream::from_iter(text.len()..text.len(), tokens);

    source_file().parse_recovery(stream)
}

fn tokens_with_range(text: &str) -> impl Iterator<Item = (SyntaxKind, Range<usize>)> + '_ {
    let mut pos = TextSize::from(0);

    Lexer::new(text).map(move |token| {
        let start = pos;
        pos += token.len;
        let range = usize::from(start)..usize::from(pos);
        (token.kind, range)
    })
}

fn source_file() -> impl Parser<SyntaxKind, Event, Error = ParseError> {
    rtoken(LYT_SEP)
        .repeated()
        .collect::<Event>()
        .then(module(item()))
        .then(token(EOF))
        .to_event()
        .recover_with(skip_until([EOF], err))
        .to_node(SOURCE_FILE)
}

fn item() -> impl Parser<SyntaxKind, Event, Error = ParseError> + Clone {
    recursive(|item| {
        let module = module(item);
        let func = func();
        let type_ctor = type_ctor();
        let trait_ = trait_();
        let impl_ = impl_();

        module.or(func).or(type_ctor).or(trait_).or(impl_)
    })
}

fn assoc_item() -> impl Parser<SyntaxKind, Event, Error = ParseError> + Clone {
    func()
}

fn module(
    item: impl Parser<SyntaxKind, Event, Error = ParseError> + Clone,
) -> impl Parser<SyntaxKind, Event, Error = ParseError> {
    module_header()
        .then(token(EQUALS))
        .then(choice((
            rtoken(LYT_SEP)
                .then(item.clone().separated_trailing(rtoken(LYT_SEP)).collect())
                .to_event(),
            rtoken(LYT_SEP).then(item.clone().separated(rtoken(LYT_SEP))).to_event(),
            block(item),
        )))
        .to_event()
        .to_node(ITEM_MODULE)
}

fn module_header() -> impl Parser<SyntaxKind, Event, Error = ParseError> {
    token(MODULE_KW).then(module_name()).to_event()
}

fn module_name() -> impl Parser<SyntaxKind, Event, Error = ParseError> {
    trivia()
        .then(rtoken(TYPE).to_node(PATH_SEGMENT).separated(rtoken(DOT)))
        .then(trivia())
        .to_event()
        .to_node(PATH)
}

fn func() -> impl Parser<SyntaxKind, Event, Error = ParseError> + Clone {
    let params = pat_atom().repeated().collect();
    let decl = name().then(token(DBL_COLON)).then(typ()).to_event();
    let defi = name().then(params).then(token(EQUALS)).then(expr()).to_event();

    decl.or(defi).to_node(ITEM_VALUE)
}

fn type_ctor() -> impl Parser<SyntaxKind, Event, Error = ParseError> {
    let header = type_header();
    let ctor = token(PIPE).then(token(TYPE)).to_event().to_node(CTOR);
    let body = choice((ctor.clone().repeated().at_least(1).collect(), block(ctor.to_event())));

    header.then(token(EQUALS)).then(body).to_event().to_node(ITEM_TYPE)
}

fn type_header() -> impl Parser<SyntaxKind, Event, Error = ParseError> {
    token(TYPE_KW).then(token(TYPE)).then(type_vars()).to_event()
}

fn trait_() -> impl Parser<SyntaxKind, Event, Error = ParseError> {
    let header = token(TRAIT_KW).then(token(TYPE)).then(type_vars()).to_event();
    let body = token(EQUALS).then(block(assoc_item())).to_event();

    header.clone().then(body).to_event().or(header).to_node(ITEM_TRAIT)
}

fn impl_() -> impl Parser<SyntaxKind, Event, Error = ParseError> {
    let header = token(IMPL_KW)
        .then(token(TYPE))
        .then(typ_atom().repeated().at_least(1).collect())
        .to_event();
    let body = token(EQUALS).then(block(assoc_item())).to_event();

    header.clone().then(body).to_event().or(header).to_node(ITEM_IMPL)
}

fn type_vars() -> impl Parser<SyntaxKind, Event, Error = ParseError> + Clone {
    token(IDENT).repeated().labelled("type variables").collect()
}

fn pat_atom() -> impl Parser<SyntaxKind, Event, Error = ParseError> + Clone {
    let ident = token(IDENT);
    let underscore = token(UNDERSCORE);
    let parens = parens(pat());

    ident.or(underscore).or(parens)
}

fn pat() -> impl Parser<SyntaxKind, Event, Error = ParseError> + Clone {
    recursive(|pat| {
        let ident = token(IDENT);
        let underscore = token(UNDERSCORE);
        let parens = parens(pat.clone());
        let atom = ident.or(underscore).or(parens);
        let typed = atom.clone().then(token(DBL_COLON)).then(typ()).to_event();

        typed.or(atom)
    })
}

fn typ_atom() -> impl Parser<SyntaxKind, Event, Error = ParseError> + Clone {
    let name = token(TYPE);
    let ident = token(IDENT);
    let parens = parens(typ());

    name.or(ident).or(parens)
}

fn typ() -> impl Parser<SyntaxKind, Event, Error = ParseError> + Clone {
    recursive(move |typ| {
        let name = token(TYPE);
        let ident = token(IDENT);
        let parens = parens(typ);
        let atom = name.or(ident).or(parens);
        let app = atom.clone().then(atom.clone().repeated().collect()).to_event();
        let func = app
            .clone()
            .then(token(ARROW).then(app.clone()).repeated().collect())
            .map(Into::into);

        func.or(app).or(atom)
    })
}

fn expr() -> impl Parser<SyntaxKind, Event, Error = ParseError> + Clone {
    recursive(|expr| {
        let ident = token(IDENT);
        let int = token(INT);
        let hole = token(UNDERSCORE);
        let block = block(stmt(expr.clone()));
        let parens = parens(expr);
        let atom = ident.or(int).or(hole).or(block).or(parens);

        atom
    })
}

fn stmt(
    expr: impl Parser<SyntaxKind, Event, Error = ParseError> + Clone,
) -> impl Parser<SyntaxKind, Event, Error = ParseError> + Clone {
    expr
}

fn parens(
    item: impl Parser<SyntaxKind, Event, Error = ParseError> + Clone,
) -> impl Parser<SyntaxKind, Event, Error = ParseError> + Clone {
    token(L_PAREN)
        .then(item)
        .then(token(R_PAREN))
        .to_event()
        .recover_with(nested_delimiters(
            L_PAREN,
            R_PAREN,
            [(L_BRACE, R_BRACE), (L_BRACKET, R_BRACKET), (LYT_START, LYT_END)],
            err,
        ))
}

fn block(
    item: impl Parser<SyntaxKind, Event, Error = ParseError> + Clone,
) -> impl Parser<SyntaxKind, Event, Error = ParseError> + Clone {
    rtoken(LYT_START)
        .then(item.separated(rtoken(LYT_SEP)))
        .then(rtoken(LYT_END))
        .to_event()
        .recover_with(nested_delimiters(
            LYT_START,
            LYT_END,
            [(L_PAREN, R_PAREN), (L_BRACE, R_BRACE), (L_BRACKET, R_BRACKET)],
            err,
        ))
}

fn name() -> impl Parser<SyntaxKind, Event, Error = ParseError> + Clone {
    token(IDENT).to_node(NAME)
}

fn token(kind: SyntaxKind) -> impl Parser<SyntaxKind, Event, Error = ParseError> + Clone {
    trivia().then(rtoken(kind)).then(trivia()).to_event()
}

fn rtoken(kind: SyntaxKind) -> impl Parser<SyntaxKind, Event, Error = ParseError> + Clone {
    just(kind).map_with_span(|t, s: Range<usize>| Event::Token(t, TextSize::from((s.end - s.start) as u32)))
}

fn trivia() -> impl Parser<SyntaxKind, Event, Error = ParseError> + Clone {
    rtoken(WHITESPACE).or(rtoken(COMMENT)).repeated().collect()
}

fn err(span: Range<usize>) -> Event {
    Event::Token(ERROR, TextSize::from((span.end - span.start) as u32))
}

trait ParserExt: Parser<SyntaxKind, Event, Error = ParseError> + Sized {
    fn to_node(self, kind: SyntaxKind) -> Map<Self, ToNode, Event>;

    fn separated<Sep>(
        self,
        sep: Sep,
    ) -> Map<
        Then<Map<Repeated<Then<Self, Sep>>, fn(Vec<(Event, Event)>) -> Event, Vec<(Event, Event)>>, Self>,
        fn((Event, Event)) -> Event,
        (Event, Event),
    >
    where
        Self: Clone,
        Sep: Parser<SyntaxKind, Event, Error = ParseError> + Clone;

    fn separated_trailing<Sep>(self, sep: Sep) -> Repeated<Then<Self, Sep>>
    where
        Sep: Parser<SyntaxKind, Event, Error = ParseError>;
}

trait ToEvent<O>: Parser<SyntaxKind, O, Error = ParseError> + Sized {
    fn to_event(self) -> Map<Self, fn(O) -> Event, O>;
}

#[derive(Clone)]
struct ToNode(Rc<dyn Fn(Event) -> Event>);

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

    fn separated<Sep>(
        self,
        sep: Sep,
    ) -> Map<
        Then<Map<Repeated<Then<Self, Sep>>, fn(Vec<(Event, Event)>) -> Event, Vec<(Event, Event)>>, Self>,
        fn((Event, Event)) -> Event,
        (Event, Event),
    >
    where
        Self: Clone,
        Sep: Parser<SyntaxKind, Event, Error = ParseError> + Clone,
    {
        self.clone()
            .then(sep)
            .repeated()
            .collect::<Event>()
            .then(self)
            .to_event()
    }

    fn separated_trailing<Sep>(self, sep: Sep) -> Repeated<Then<Self, Sep>>
    where
        Sep: Parser<SyntaxKind, Event, Error = ParseError>,
    {
        self.then(sep).repeated()
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

#[test]
fn test_parser() {
    let input = r#"
        module Core.Cmp =

        main = 0
        id x = a

        other (a :: T) =
            a

        type X =
            | Y
            | Z

        trait Iterator self it =
            next :: self -> Option it

        impl Iterator Iter Item =
            next self = _
    "#;
    let input = unindent::unindent(input.trim());
    let output = parse(&input);

    insta::assert_ron_snapshot!(output, {
        "[1].$expected" => insta::sorted_redaction(),
    });
}
