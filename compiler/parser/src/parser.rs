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
    // dbg!(tokens_with_range(text).collect::<Vec<_>>());
    let tokens = tokens_with_range(text);
    let stream = Stream::from_iter(text.len()..text.len(), tokens);

    source_file().parse_recovery_verbose(stream)
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
        .to_event()
        .to_node(SOURCE_FILE)
}

fn attrs() -> impl Parser<SyntaxKind, Event, Error = ParseError> + Clone {
    attr().separated(rtoken(LYT_SEP), true, 0)
}

fn attr() -> impl Parser<SyntaxKind, Event, Error = ParseError> + Clone {
    rtoken(AT)
        .then(rtoken(IDENT).to_node(NAME_REF))
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
        let foreign_type = foreign_type();
        let trait_ = trait_();
        let impl_ = impl_();

        choice((module, import, fixity, value, type_, foreign_type, trait_, impl_))
            .recover_with(skip_until([LYT_SEP], err))
    })
}

fn assoc_item() -> impl Parser<SyntaxKind, Event, Error = ParseError> + Clone {
    value()
}

fn module<'a>(
    item: impl Parser<SyntaxKind, Event, Error = ParseError> + Clone + 'a,
) -> impl Parser<SyntaxKind, Event, Error = ParseError> + 'a {
    attrs()
        .then(module_header())
        .then(token(EQUALS))
        .then(
            rtoken(LYT_SEP)
                .then(item.clone().separated(rtoken(LYT_SEP), true, 0))
                .then_ignore(end())
                .to_event()
                .recover_with(skip_until([], err))
                .or(block(item)),
        )
        .to_event()
        .to_node(ITEM_MODULE)
        .labelled("module")
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

    attrs()
        .then(base)
        .then(opt(items.or(hiding)))
        .then(opt(rename))
        .to_event()
        .to_node(ITEM_IMPORT)
        .labelled("import")
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

    attrs()
        .then(infix.or(prefix))
        .to_event()
        .to_node(ITEM_FIXITY)
        .labelled("fixity definition")
}

fn value() -> impl Parser<SyntaxKind, Event, Error = ParseError> + Clone {
    attrs()
        .then(opt(token(FOREIGN_KW)))
        .then(name())
        .then(pat_atom().repeated().collect::<Event>())
        .then(opt(token(DBL_COLON).then(typ()).to_event()))
        .then(opt(token(EQUALS).then(expr()).to_event()))
        .to_event()
        .to_node(ITEM_VALUE)
        .labelled("value definition")
}

fn type_ctor() -> impl Parser<SyntaxKind, Event, Error = ParseError> + Clone {
    let header = type_header();
    let body = choice((ctor().repeated().at_least(1).collect(), block(ctor()), typ()));

    attrs()
        .then(header)
        .then(token(EQUALS))
        .then(body)
        .to_event()
        .to_node(ITEM_TYPE)
        .labelled("type definition")
}

fn foreign_type() -> impl Parser<SyntaxKind, Event, Error = ParseError> + Clone {
    attrs()
        .then(token(FOREIGN_KW))
        .then(type_header())
        .to_event()
        .to_node(ITEM_TYPE)
        .labelled("type definition")
}

fn type_header() -> impl Parser<SyntaxKind, Event, Error = ParseError> + Clone {
    token(TYPE_KW).then(type_name()).then(type_vars()).to_event()
}

fn ctor() -> impl Parser<SyntaxKind, Event, Error = ParseError> + Clone {
    token(PIPE)
        .then(type_name())
        .then(ctor_record().or(typ_atom().repeated().collect::<Event>()))
        .to_event()
        .to_node(CTOR)
        .labelled("constructor")
}

fn ctor_record() -> impl Parser<SyntaxKind, Event, Error = ParseError> + Clone {
    braces(
        ctor_field()
            .separated(token(COMMA), true, 0)
            .then(opt(rtoken(LYT_SEP)))
            .to_event(),
    )
    .to_node(CTOR_RECORD)
    .labelled("record")
}

fn ctor_field() -> impl Parser<SyntaxKind, Event, Error = ParseError> + Clone {
    name()
        .then(token(DBL_COLON))
        .then(typ_atom())
        .to_event()
        .to_node(CTOR_FIELD)
        .labelled("field")
}

fn trait_() -> impl Parser<SyntaxKind, Event, Error = ParseError> {
    let header = attrs()
        .then(token(TRAIT_KW))
        .then(type_name())
        .then(type_vars())
        .then(opt(where_clause(typ_atom())))
        .to_event();
    let body = token(EQUALS).then(block(assoc_item())).to_event();

    header
        .clone()
        .then(body)
        .to_event()
        .or(header)
        .to_node(ITEM_TRAIT)
        .labelled("trait")
}

fn impl_() -> impl Parser<SyntaxKind, Event, Error = ParseError> {
    let header = attrs()
        .then(token(IMPL_KW))
        .then(type_path())
        .then(typ_atom().repeated().at_least(1).collect())
        .then(opt(where_clause(typ_atom())))
        .to_event();
    let body = token(EQUALS).then(block(assoc_item())).to_event();

    header
        .clone()
        .then(body)
        .to_event()
        .or(header)
        .to_node(ITEM_IMPL)
        .labelled("impl")
}

fn type_vars() -> impl Parser<SyntaxKind, Event, Error = ParseError> + Clone {
    name()
        .repeated()
        .collect()
        .to_node(TYPE_VARS)
        .labelled("type variables")
}

fn where_clause<'a>(
    typ_atom: impl Parser<SyntaxKind, Event, Error = ParseError> + Clone + 'a,
) -> impl Parser<SyntaxKind, Event, Error = ParseError> + Clone + 'a {
    let constraint = type_path()
        .then(typ_atom.repeated().collect())
        .to_event()
        .to_node(WHERE_CLAUSE_CONSTRAINT);
    let item = choice((constraint,));

    token(WHERE_KW)
        .then(item.separated(token(COMMA), true, 0))
        .to_event()
        .to_node(WHERE_CLAUSE)
        .labelled("where clause")
}

fn typ_atom() -> impl Parser<SyntaxKind, Event, Error = ParseError> + Clone {
    let ident = name().to_node(TYPE_VAR);
    let name = type_path().to_node(TYPE_PATH);
    let list = brackets(typ()).to_node(TYPE_LIST);
    let parens = parens(typ()).to_node(TYPE_PARENS);
    let unit = token(L_PAREN).then(token(R_PAREN)).to_event().to_node(TYPE_UNIT);

    choice((name, ident, list, unit, parens)).labelled("type")
}

fn typ() -> impl Parser<SyntaxKind, Event, Error = ParseError> + Clone {
    recursive(|typ| {
        let ident = name().to_node(TYPE_VAR);
        let name = type_path().to_node(TYPE_PATH);
        let list = brackets(typ.clone()).to_node(TYPE_LIST);
        let parens = parens(typ).to_node(TYPE_PARENS);
        let unit = token(L_PAREN).then(token(R_PAREN)).to_event().to_node(TYPE_UNIT);
        let atom = choice((name, ident, list, unit, parens));
        let app = atom
            .clone()
            .then(atom.clone().repeated().at_least(1).collect())
            .to_event()
            .to_node(TYPE_APP)
            .or(atom.clone());

        let infix = app
            .clone()
            .separated(symbol_operator(), false, 2)
            .to_node(TYPE_INFIX)
            .or(app.clone());

        let func = recursive(move |func| {
            app.clone()
                .separated(token(COMMA), false, 1)
                .then(token(ARROW))
                .then(func)
                .to_event()
                .to_node(TYPE_FUNC)
                .or(infix)
        });

        let where_ = func
            .clone()
            .then(where_clause(atom.clone()))
            .to_event()
            .to_node(TYPE_WHERE);

        where_.or(func)
    })
    .labelled("type")
}

fn pat_atom() -> impl Parser<SyntaxKind, Event, Error = ParseError> + Clone {
    let bind = name().to_node(PAT_BIND);
    let underscore = token(UNDERSCORE).to_node(PAT_WILDCARD);
    let ctor = type_path().to_node(PAT_PATH);
    let literal = literal().to_node(PAT_LITERAL);
    let unit = token(L_PAREN).then(token(R_PAREN)).to_event().to_node(PAT_UNIT);
    let parens = parens(pat()).to_node(PAT_PARENS);

    choice((bind, underscore, ctor, unit, literal, parens)).labelled("pattern")
}

fn pat() -> impl Parser<SyntaxKind, Event, Error = ParseError> + Clone {
    recursive(|pat| {
        let bind = name()
            .then(opt(token(AT).then(pat.clone()).to_event()))
            .to_event()
            .to_node(PAT_BIND);
        let underscore = token(UNDERSCORE).to_node(PAT_WILDCARD);
        let unit = token(L_PAREN).then(token(R_PAREN)).to_event().to_node(PAT_UNIT);
        let parens = parens(pat.clone()).to_node(PAT_PARENS);
        let ctor = type_path().to_node(PAT_PATH);
        let literal = literal().to_node(PAT_LITERAL);
        let atom = choice((bind, underscore, unit, literal, parens, ctor.clone())).pad_ws();
        let app = ctor
            .then(atom.clone().repeated().at_least(1).collect())
            .to_event()
            .to_node(PAT_APP)
            .pad_ws();
        let typed = app
            .clone()
            .or(atom.clone())
            .then(token(DBL_COLON))
            .then(typ())
            .to_event()
            .to_node(PAT_TYPED);

        choice((typed, app, atom))
    })
    .labelled("pattern")
}

fn expr() -> impl Parser<SyntaxKind, Event, Error = ParseError> + Clone {
    recursive(|expr| {
        let ident = path().to_node(EXPR_PATH);
        let lit = literal().to_node(EXPR_LITERAL);
        let hole = rtoken(UNDERSCORE).to_node(EXPR_HOLE);
        let unit = rtoken(L_PAREN).then(rtoken(R_PAREN)).to_event().to_node(EXPR_UNIT);
        let block = block(stmt(expr.clone(), false)).to_node(EXPR_BLOCK);
        let parens = parens(expr.clone()).to_node(EXPR_PARENS);
        let atom = choice((ident, lit, hole, unit, block, parens)).pad_ws();
        let app = atom.clone().repeated().at_least(2).collect().to_node(EXPR_APP).pad_ws();
        let app = app.or(atom);
        let infix = app.clone().separated(operator(), false, 2).to_node(EXPR_INFIX).pad_ws();
        let infix = infix.or(app);
        let ifelse = rtoken(IF_KW)
            .then(infix.clone())
            .then(opt(rtoken(LYT_SEP)))
            .then(rtoken(THEN_KW))
            .then(expr.clone())
            .then(opt(opt(rtoken(LYT_SEP))
                .then(rtoken(ELSE_KW))
                .then(expr.clone())
                .to_event()))
            .to_event()
            .to_node(EXPR_IF)
            .pad_ws();
        let match_ = rtoken(MATCH_KW)
            .then(infix.clone())
            .then(rtoken(WITH_KW))
            .then(rtoken(LYT_SEP).then(match_arm(expr.clone())).repeated().collect())
            .to_event()
            .to_node(EXPR_MATCH)
            .pad_ws();
        let lambda = rtoken(FN_KW)
            .then(pat_atom().repeated().collect::<Event>())
            .then(rtoken(ARROW))
            .then(expr.clone())
            .to_event()
            .to_node(EXPR_LAMBDA)
            .pad_ws();
        let do_block = rtoken(DO_KW)
            .then(self::block(stmt(expr.clone(), true)))
            .to_event()
            .to_node(EXPR_DO);
        let base = choice((ifelse, match_, lambda, do_block, infix));

        base.clone()
            .then(token(DBL_COLON))
            .then(typ())
            .to_event()
            .to_node(EXPR_TYPED)
            .pad_ws()
            .or(base)
    })
    .labelled("expression")
}

fn match_arm(
    expr: impl Parser<SyntaxKind, Event, Error = ParseError> + Clone,
) -> impl Parser<SyntaxKind, Event, Error = ParseError> + Clone {
    let guard = rtoken(IF_KW).then(expr.clone()).to_event().to_node(MATCH_GUARD);

    rtoken(PIPE)
        .then(pat())
        .then(opt(guard))
        .then(rtoken(ARROW))
        .then(expr)
        .to_event()
        .to_node(MATCH_ARM)
}

fn stmt<'a>(
    expr: impl Parser<SyntaxKind, Event, Error = ParseError> + Clone + 'a,
    allow_bind: bool,
) -> impl Parser<SyntaxKind, Event, Error = ParseError> + Clone + 'a {
    let let_ = pat()
        .then(token(EQUALS))
        .then(expr.clone())
        .to_event()
        .to_node(STMT_LET);
    let bind = opt(pat())
        .then(token(LEFT_ARROW))
        .then(expr.clone())
        .to_event()
        .to_node(STMT_BIND);
    let expr = expr.to_node(STMT_EXPR);

    if allow_bind {
        choice((let_, bind, expr)).boxed().labelled("statement")
    } else {
        choice((let_, expr)).boxed().labelled("statement")
    }
}

fn parens<'a>(
    item: impl Parser<SyntaxKind, Event, Error = ParseError> + Clone + 'a,
) -> impl Parser<SyntaxKind, Event, Error = ParseError> + Clone + 'a {
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

fn braces<'a>(
    item: impl Parser<SyntaxKind, Event, Error = ParseError> + Clone + 'a,
) -> impl Parser<SyntaxKind, Event, Error = ParseError> + Clone + 'a {
    token(L_BRACE)
        .then(item)
        .then(token(R_BRACE))
        .to_event()
        .recover_with(nested_delimiters(
            L_BRACE,
            R_BRACE,
            [(L_PAREN, R_PAREN), (L_BRACKET, R_BRACKET), (LYT_START, LYT_END)],
            err,
        ))
}

fn brackets<'a>(
    item: impl Parser<SyntaxKind, Event, Error = ParseError> + Clone + 'a,
) -> impl Parser<SyntaxKind, Event, Error = ParseError> + Clone + 'a {
    token(L_BRACKET)
        .then(item)
        .then(token(R_BRACKET))
        .to_event()
        .recover_with(nested_delimiters(
            L_BRACKET,
            R_BRACKET,
            [(L_PAREN, R_PAREN), (L_BRACE, R_BRACE), (LYT_START, LYT_END)],
            err,
        ))
}

fn block<'a>(
    item: impl Parser<SyntaxKind, Event, Error = ParseError> + Clone + 'a,
) -> impl Parser<SyntaxKind, Event, Error = ParseError> + Clone + 'a {
    rtoken(LYT_START)
        .then(item.separated(rtoken(LYT_SEP), true, 0))
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

    lit.pad_ws().labelled("literal")
}

fn path() -> impl Parser<SyntaxKind, Event, Error = ParseError> + Clone {
    rtoken(TYPE)
        .to_node(NAME_REF)
        .to_node(PATH_SEGMENT)
        .then(rtoken(DOT))
        .repeated()
        .collect::<Event>()
        .then(rany_name_ref().to_node(PATH_SEGMENT))
        .to_event()
        .to_node(PATH)
        .labelled("path")
}

fn type_path() -> impl Parser<SyntaxKind, Event, Error = ParseError> + Clone {
    rtoken(TYPE)
        .to_node(NAME_REF)
        .to_node(PATH_SEGMENT)
        .then(rtoken(DOT))
        .repeated()
        .collect::<Event>()
        .then(rtype_name_ref().to_node(PATH_SEGMENT))
        .then(trivia())
        .to_event()
        .to_node(PATH)
        .labelled("path")
}

fn name() -> impl Parser<SyntaxKind, Event, Error = ParseError> + Clone {
    trivia().then(rtoken(IDENT).to_node(NAME)).then(trivia()).to_event()
}

fn type_name() -> impl Parser<SyntaxKind, Event, Error = ParseError> + Clone {
    trivia().then(rtoken(TYPE).to_node(NAME)).then(trivia()).to_event()
}

fn any_name() -> impl Parser<SyntaxKind, Event, Error = ParseError> + Clone {
    trivia()
        .then(rtoken([IDENT, TYPE]).or(symbol()).to_node(NAME))
        .then(trivia())
        .to_event()
}

fn name_ref() -> impl Parser<SyntaxKind, Event, Error = ParseError> + Clone {
    trivia().then(rtoken(IDENT).to_node(NAME_REF)).then(trivia()).to_event()
}

fn type_name_ref() -> impl Parser<SyntaxKind, Event, Error = ParseError> + Clone {
    trivia().then(rtoken(TYPE).to_node(NAME_REF)).then(trivia()).to_event()
}

fn rtype_name_ref() -> impl Parser<SyntaxKind, Event, Error = ParseError> + Clone {
    rtoken(TYPE).to_node(NAME_REF)
}

fn any_name_ref() -> impl Parser<SyntaxKind, Event, Error = ParseError> + Clone {
    trivia()
        .then(rtoken([IDENT, TYPE]).or(symbol()).to_node(NAME_REF))
        .then(trivia())
        .to_event()
}

fn rany_name_ref() -> impl Parser<SyntaxKind, Event, Error = ParseError> + Clone {
    rtoken([IDENT, TYPE]).or(symbol()).to_node(NAME_REF)
}

fn operator() -> impl Parser<SyntaxKind, Event, Error = ParseError> + Clone {
    trivia()
        .then(
            rtoken(TICK)
                .then(path())
                .then(rtoken(TICK))
                .to_event()
                .or(rtoken([SYMBOL, COMMA, AT, PIPE])
                    .to_node(NAME_REF)
                    .to_node(PATH_SEGMENT)
                    .to_node(PATH)),
        )
        .then(trivia())
        .to_event()
}

fn symbol_operator() -> impl Parser<SyntaxKind, Event, Error = ParseError> + Clone {
    trivia()
        .then(
            rtoken([SYMBOL, COMMA, AT, PIPE])
                .to_node(NAME_REF)
                .to_node(PATH_SEGMENT)
                .to_node(PATH),
        )
        .then(trivia())
        .to_event()
}

fn symbol() -> impl Parser<SyntaxKind, Event, Error = ParseError> + Clone {
    rtoken(L_PAREN)
        .then(rtoken([SYMBOL, COMMA, AT, PIPE]))
        .then(rtoken(R_PAREN))
        .to_event()
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
