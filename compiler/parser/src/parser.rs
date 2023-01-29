mod ext;

use std::ops::Range;

use chumsky::prelude::*;
use chumsky::primitive::Container;
use chumsky::{Parser, Stream};
use ext::*;
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
    opt(rtoken(LYT_SEP))
        .then(module(item()))
        .then_ignore(end())
        .to_event()
        .recover_with(skip_until([], err))
        .to_node(SOURCE_FILE)
}

fn attr() -> impl Parser<SyntaxKind, Event, Error = ParseError> + Clone {
    rtoken(AT)
        .then(rtoken(IDENT))
        .then(opt(token(EQUALS).then(literal()).to_event().or(attr_args())))
        .to_event()
        .to_node(ATTR)
}

fn attr_args() -> impl Parser<SyntaxKind, Event, Error = ParseError> + Clone {
    recursive(|attr_args| {
        let attr_arg = name_ref()
            .then(attr_args)
            .to_event()
            .to_node(ATTR_ARG_CALL)
            .or(name_ref()
                .then(token(EQUALS))
                .then(literal())
                .to_event()
                .to_node(ATTR_ARG_EQUAL))
            .or(name_ref().to_node(ATTR_ARG_IDENT))
            .or(literal().to_node(ATTR_ARG_LIT));

        parens(attr_arg.separated(token(COMMA), true, 0)).to_node(ATTR_ARGS)
    })
}

fn item() -> impl Parser<SyntaxKind, Event, Error = ParseError> + Clone {
    recursive(|item| {
        let module = module(item);
        let import = import();
        let fixity = fixity();
        let value = value();
        let type_ = type_ctor();
        let trait_ = trait_();
        let impl_ = impl_();

        choice((module, import, fixity, value, type_, trait_, impl_))
    })
}

fn assoc_item() -> impl Parser<SyntaxKind, Event, Error = ParseError> + Clone {
    value()
}

fn module<'a>(
    item: impl Parser<SyntaxKind, Event, Error = ParseError> + Clone + 'a,
) -> impl Parser<SyntaxKind, Event, Error = ParseError> + 'a {
    attr()
        .repeated()
        .collect::<Event>()
        .then(module_header())
        .then(token(EQUALS))
        .then(
            rtoken(LYT_SEP)
                .then(item.clone().separated(rtoken(LYT_SEP), true, 1))
                .to_event()
                .or(block(item)),
        )
        .to_event()
        .to_node(ITEM_MODULE)
}

fn module_header() -> impl Parser<SyntaxKind, Event, Error = ParseError> {
    token(MODULE_KW).then(module_name()).then(opt(exports())).to_event()
}

fn module_name() -> impl Parser<SyntaxKind, Event, Error = ParseError> + Clone {
    let name = rtoken(TYPE)
        .to_node(NAME_REF)
        .to_node(PATH_SEGMENT)
        .separated(rtoken(DOT), false, 1)
        .to_node(PATH);
    trivia().then(name).then(trivia()).to_event()
}

fn exports() -> impl Parser<SyntaxKind, Event, Error = ParseError> + Clone {
    parens(export().separated(token(COMMA), true, 0)).to_node(EXPORTS)
}

fn export() -> impl Parser<SyntaxKind, Event, Error = ParseError> + Clone {
    any_name_ref()
        .to_node(EXPORT_NAME)
        .or(token(MODULE_KW).then(type_name_ref()).to_event().to_node(EXPORT_MODULE))
}

fn import() -> impl Parser<SyntaxKind, Event, Error = ParseError> + Clone {
    let base = token(IMPORT_KW).then(module_name()).to_event();
    let items = parens(import_item().separated(token(COMMA), true, 0)).to_node(IMPORT_ITEMS);
    let hiding = token(HIDING_KW)
        .then(parens(any_name_ref().separated(token(COMMA), true, 0)))
        .to_event()
        .to_node(IMPORT_HIDING);
    let rename = token(AS_KW).then(type_name()).to_event();

    attr()
        .repeated()
        .collect::<Event>()
        .then(base)
        .then(opt(items.or(hiding)))
        .then(opt(rename))
        .to_event()
        .to_node(ITEM_IMPORT)
}

fn import_item() -> impl Parser<SyntaxKind, Event, Error = ParseError> + Clone {
    any_name_ref()
        .then(opt(token(AS_KW).then(any_name()).to_event()))
        .to_event()
        .to_node(IMPORT_ITEM)
}

fn fixity() -> impl Parser<SyntaxKind, Event, Error = ParseError> + Clone {
    let infix = token([INFIX_KW, INFIXL_KW, INFIXR_KW])
        .then(opt(token(TYPE_KW)))
        .then(token(INT))
        .then(symbol().to_node(NAME))
        .then(token(EQUALS))
        .then(path())
        .to_event();

    let prefix = token([PREFIX_KW, POSTFIX_KW])
        .then(opt(token(TYPE_KW)))
        .then(symbol().to_node(NAME))
        .then(token(EQUALS))
        .then(path())
        .to_event();

    attr()
        .repeated()
        .collect::<Event>()
        .then(infix.or(prefix))
        .to_event()
        .to_node(ITEM_FIXITY)
}

fn value() -> impl Parser<SyntaxKind, Event, Error = ParseError> + Clone {
    name()
        .then(pat_atom().repeated().collect::<Event>())
        .then(opt(token(DBL_COLON).then(typ()).to_event()))
        .then(opt(token(EQUALS).then(expr()).to_event()))
        .to_event()
        .to_node(ITEM_VALUE)
}

fn type_ctor() -> impl Parser<SyntaxKind, Event, Error = ParseError> {
    let header = type_header();
    let ctor = token(PIPE)
        .then(type_name())
        .then(typ_atom().repeated().collect::<Event>())
        .to_event()
        .to_node(CTOR);

    let body = choice((ctor.clone().repeated().at_least(1).collect(), block(ctor.to_event())));

    header.then(token(EQUALS)).then(body).to_event().to_node(ITEM_TYPE)
}

fn type_header() -> impl Parser<SyntaxKind, Event, Error = ParseError> {
    token(TYPE_KW).then(type_name()).then(type_vars()).to_event()
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
    let unit = token(L_PAREN).then(token(R_PAREN)).to_event();
    let parens = parens(pat());

    ident.or(underscore).or(unit).or(parens)
}

fn pat() -> impl Parser<SyntaxKind, Event, Error = ParseError> + Clone {
    recursive(|pat| {
        let ident = token(IDENT);
        let underscore = token(UNDERSCORE);
        let unit = token(L_PAREN).then(token(R_PAREN)).to_event();
        let parens = parens(pat.clone());
        let atom = ident.or(underscore).or(unit).or(parens);
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

fn block<'a>(
    item: impl Parser<SyntaxKind, Event, Error = ParseError> + Clone + 'a,
) -> impl Parser<SyntaxKind, Event, Error = ParseError> + Clone + 'a {
    rtoken(LYT_START)
        .then(item.separated(rtoken(LYT_SEP), false, 1))
        .then(rtoken(LYT_END))
        .to_event()
        .recover_with(nested_delimiters(
            LYT_START,
            LYT_END,
            [(L_PAREN, R_PAREN), (L_BRACE, R_BRACE), (L_BRACKET, R_BRACKET)],
            err,
        ))
}

fn literal() -> impl Parser<SyntaxKind, Event, Error = ParseError> + Clone {
    let lit = rtoken(INT)
        .to_node(LIT_INT)
        .or(rtoken(FLOAT).to_node(LIT_FLOAT))
        .or(rtoken(CHAR).to_node(LIT_CHAR))
        .or(rtoken(STRING).to_node(LIT_STRING))
        .labelled("literal");

    trivia().then(lit).then(trivia()).to_event()
}

fn path() -> impl Parser<SyntaxKind, Event, Error = ParseError> + Clone {
    rtoken(TYPE)
        .to_node(NAME_REF)
        .to_node(PATH_SEGMENT)
        .then(rtoken(DOT))
        .repeated()
        .collect::<Event>()
        .then(any_name_ref().to_node(PATH_SEGMENT))
        .to_event()
        .to_node(PATH)
}

fn name() -> impl Parser<SyntaxKind, Event, Error = ParseError> + Clone {
    token(IDENT).to_node(NAME)
}

fn type_name() -> impl Parser<SyntaxKind, Event, Error = ParseError> + Clone {
    token(TYPE).to_node(NAME)
}

fn any_name() -> impl Parser<SyntaxKind, Event, Error = ParseError> + Clone {
    token([IDENT, TYPE]).or(symbol()).to_node(NAME)
}

fn name_ref() -> impl Parser<SyntaxKind, Event, Error = ParseError> + Clone {
    token(IDENT).to_node(NAME_REF)
}

fn type_name_ref() -> impl Parser<SyntaxKind, Event, Error = ParseError> + Clone {
    token(TYPE).to_node(NAME_REF)
}

fn any_name_ref() -> impl Parser<SyntaxKind, Event, Error = ParseError> + Clone {
    token([IDENT, TYPE]).or(symbol()).to_node(NAME_REF)
}

fn symbol() -> impl Parser<SyntaxKind, Event, Error = ParseError> + Clone {
    rtoken(L_PAREN).then(rtoken(SYMBOL)).then(rtoken(R_PAREN)).to_event()
}

fn token(kinds: impl Container<SyntaxKind> + Copy) -> impl Parser<SyntaxKind, Event, Error = ParseError> + Clone {
    trivia().then(rtoken(kinds)).then(trivia()).to_event()
}

fn rtoken(kinds: impl Container<SyntaxKind> + Copy) -> impl Parser<SyntaxKind, Event, Error = ParseError> + Clone {
    one_of(kinds).map_with_span(|t, s: Range<usize>| Event::Token(t, TextSize::from((s.end - s.start) as u32)))
}

fn trivia() -> impl Parser<SyntaxKind, Event, Error = ParseError> + Clone {
    rtoken([WHITESPACE, COMMENT]).repeated().collect()
}

fn err(span: Range<usize>) -> Event {
    Event::Token(ERROR, TextSize::from((span.end - span.start) as u32))
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
