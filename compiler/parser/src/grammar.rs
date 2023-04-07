use crate::parser::{CompletedMarker, Marker, Parser};
use crate::token::SyntaxKind;
use crate::token_set::TokenSet;
use crate::SyntaxKind::*;

pub fn source_file(p: &mut Parser) {
    let s = p.start();
    let m = p.start();

    p.eat(LYT_SEP);
    attrs(p);
    item_module(p, m);
    s.complete(p, SOURCE_FILE);
}

fn attrs(p: &mut Parser) {
    while !p.eof() && p.at(AT) {
        attr(p);
        p.eat(LYT_SEP);
    }
}

fn attr(p: &mut Parser) {
    let m = p.start();
    p.bump(AT);
    ident_ref(p);

    if p.eat(EQUALS) {
        literal(p);
    } else if p.at(L_PAREN) {
        attr_args(p);
    }

    m.complete(p, ATTR);
}

fn attr_args(p: &mut Parser) {
    let m = p.start();
    p.expect(L_PAREN);
    separated(p, COMMA, R_PAREN, IDENT | INT | FLOAT | CHAR | STRING, attr_arg);
    p.expect(R_PAREN);
    m.complete(p, ATTR_ARGS);
}

fn attr_arg(p: &mut Parser) {
    let m = p.start();

    if p.at(IDENT) {
        ident_ref(p);

        if p.eat(EQUALS) {
            literal(p);
            m.complete(p, ATTR_ARG_EQUAL);
        } else if p.at(L_PAREN) {
            attr_args(p);
            m.complete(p, ATTR_ARG_CALL);
        } else {
            m.complete(p, ATTR_ARG_IDENT);
        }
    } else {
        literal(p);
        m.complete(p, ATTR_ARG_LIT);
    }
}

const ITEM_TOKENS: [SyntaxKind; 11] = [
    AT, MODULE_KW, IMPORT_KW, INFIX_KW, INFIXL_KW, INFIXR_KW, TYPE_KW, FOREIGN_KW, TRAIT_KW, IMPL_KW, IDENT,
];
const PEEK_ITEM: TokenSet = TokenSet::new(&ITEM_TOKENS);

fn item(p: &mut Parser) {
    let m = p.start();

    attrs(p);

    match p.current() {
        | Some(MODULE_KW) => item_module(p, m),
        | Some(IMPORT_KW) => item_import(p, m),
        | Some(INFIX_KW | INFIXL_KW | INFIXR_KW | PREFIX_KW | POSTFIX_KW) => item_fixity(p, m),
        | Some(TYPE_KW) => item_type(p, m),
        | Some(TRAIT_KW) => item_trait(p, m),
        | Some(IMPL_KW) => item_impl(p, m),
        | Some(IDENT) => item_value(p, m),
        | Some(FOREIGN_KW) => match p.nth(1) {
            | Some(TYPE_KW) => item_type(p, m),
            | Some(IDENT) => item_value(p, m),
            | _ => p.error([TYPE_KW, IDENT]),
        },
        | _ => {
            p.error(ITEM_TOKENS);
            m.abandon(p);
        },
    }
}

fn assoc_item(p: &mut Parser) {
    let m = p.start();

    attrs(p);
    item_value(p, m);
}

fn item_module(p: &mut Parser, m: Marker) {
    p.expect(MODULE_KW);
    module_name(p);

    if p.at(L_PAREN) {
        exports(p);
    }

    p.expect(EQUALS);
    p.eat(LYT_SEP);

    if p.at(LYT_START) {
        block(p, PEEK_ITEM, item);
    } else {
        separated(p, LYT_SEP, TokenSet::EMPTY, PEEK_ITEM, item);
    }

    m.complete(p, ITEM_MODULE);
}

fn module_name(p: &mut Parser) {
    let m = p.start();
    let s = p.start();
    type_ref(p);
    s.complete(p, PATH_SEGMENT);

    while !p.eof() && p.eat(DOT) {
        let s = p.start();
        type_ref(p);
        s.complete(p, PATH_SEGMENT);
    }

    m.complete(p, PATH);
}

fn exports(p: &mut Parser) {
    let m = p.start();
    p.expect(L_PAREN);
    separated(p, COMMA, R_PAREN, IDENT | TYPE | SYMBOL | MODULE_KW, export);
    p.expect(R_PAREN);
    m.complete(p, EXPORTS);
}

fn export(p: &mut Parser) {
    let m = p.start();

    if p.eat(MODULE_KW) {
        type_ref(p);
        m.complete(p, EXPORT_MODULE);
    } else {
        name_ref(p);
        m.complete(p, EXPORT_NAME);
    }
}

fn item_import(p: &mut Parser, m: Marker) {
    p.expect(IMPORT_KW);
    module_name(p);

    if p.at(L_PAREN) {
        import_items(p);
    }

    if p.at(HIDING_KW) {
        import_hiding(p);
    }

    if p.eat(AS_KW) {
        type_name(p);
    }

    m.complete(p, ITEM_IMPORT);
}

fn import_items(p: &mut Parser) {
    let m = p.start();
    p.expect(L_PAREN);
    separated(p, COMMA, R_PAREN, IDENT | TYPE | SYMBOL, import_item);
    p.expect(R_PAREN);
    m.complete(p, IMPORT_ITEMS);
}

fn import_item(p: &mut Parser) {
    let m = p.start();
    name_ref(p);

    if p.eat(AS_KW) {
        name(p);
    }

    m.complete(p, IMPORT_ITEM);
}

fn import_hiding(p: &mut Parser) {
    let m = p.start();
    p.expect(HIDING_KW);
    p.expect(L_PAREN);
    separated(p, COMMA, R_PAREN, IDENT | TYPE | SYMBOL, name_ref);
    p.expect(R_PAREN);
    m.complete(p, IMPORT_HIDING);
}

fn item_fixity(p: &mut Parser, m: Marker) {
    match p.current() {
        | Some(INFIX_KW | INFIXL_KW | INFIXR_KW) => {
            p.bump_any();
            p.eat(TYPE_KW);
            p.expect(INT);
            symbol_name(p);
            p.expect(EQUALS);
            path(p);
        },
        | Some(PREFIX_KW | POSTFIX_KW) => {
            p.bump_any();
            p.eat(TYPE_KW);
            symbol_name(p);
            p.expect(EQUALS);
            path(p);
        },
        | _ => {
            p.error([INFIX_KW, INFIXL_KW, INFIXR_KW, PREFIX_KW, POSTFIX_KW]);
            m.abandon(p);
            return;
        },
    }

    m.complete(p, ITEM_FIXITY);
}

fn item_value(p: &mut Parser, m: Marker) {
    p.eat(FOREIGN_KW);
    ident_name(p);

    while !p.eof() && p.at_ts(PEEK_PAT) {
        pat_atom(p);
    }

    if p.eat(DBL_COLON) {
        typ(p);
    }

    if p.eat(EQUALS) {
        expr(p);
    }

    m.complete(p, ITEM_VALUE);
}

fn item_type(p: &mut Parser, m: Marker) {
    if p.eat(FOREIGN_KW) {
        p.expect(TYPE_KW);
        type_name(p);
        p.expect(DBL_COLON);
        typ(p);
    } else {
        p.expect(TYPE_KW);
        type_name(p);
        type_vars(p);
        p.expect(EQUALS);

        if p.eat(LYT_START) {
            if p.at_ts(PIPE | AT) {
                while !p.eof() && p.at_ts(PIPE | AT) {
                    ctor(p);
                    p.eat(LYT_SEP);
                }
            } else {
                typ(p);
            }
            p.expect(LYT_END);
        } else if p.at_ts(PIPE | AT) {
            while !p.eof() && p.at_ts(PIPE | AT) {
                ctor(p);
            }
        } else {
            typ(p);
        }
    }

    m.complete(p, ITEM_TYPE);
}

fn ctor(p: &mut Parser) {
    let m = p.start();

    attrs(p);
    p.expect(PIPE);
    type_name(p);

    while !p.eof() && p.at_ts(PEEK_TYP) {
        typ_atom(p);
    }

    m.complete(p, CTOR);
}

fn item_trait(p: &mut Parser, m: Marker) {
    p.expect(TRAIT_KW);
    type_name(p);
    type_vars(p);

    if p.at(WHERE_KW) {
        where_clause(p);
    }

    if p.eat(EQUALS) {
        block(p, AT | IDENT, assoc_item);
    }

    m.complete(p, ITEM_TRAIT);
}

fn item_impl(p: &mut Parser, m: Marker) {
    p.expect(IMPL_KW);
    type_path(p);

    while !p.eof() && p.at_ts(PEEK_TYP) {
        typ_atom(p);
    }

    if p.at(WHERE_KW) {
        where_clause(p);
    }

    if p.eat(EQUALS) {
        block(p, AT | IDENT, assoc_item);
    }

    m.complete(p, ITEM_IMPL);
}

fn type_vars(p: &mut Parser) {
    let m = p.start();
    while !p.eof() && p.at(IDENT) {
        ident_name(p);
    }
    m.complete(p, TYPE_VARS);
}

fn where_clause(p: &mut Parser) {
    let m = p.start();
    p.expect(WHERE_KW);

    if p.eat(LYT_START) {
        where_clause_item(p);
        while !p.eof() && p.eat_ts(COMMA | LYT_SEP) {
            where_clause_item(p);
        }
        p.expect(LYT_END);
    } else {
        where_clause_item(p);
        while !p.eof() && p.eat(COMMA) {
            where_clause_item(p);
        }
    }

    m.complete(p, WHERE_CLAUSE);
}

fn where_clause_item(p: &mut Parser) {
    constraint(p);
}

fn constraint(p: &mut Parser) {
    let m = p.start();
    type_path(p);
    while !p.eof() && p.at_ts(PEEK_TYP) {
        typ_atom(p);
    }
    m.complete(p, WHERE_CLAUSE_CONSTRAINT);
}

const TYP_TOKENS: [SyntaxKind; 3] = [IDENT, TYPE, L_PAREN];
const PEEK_TYP: TokenSet = TokenSet::new(&TYP_TOKENS);

fn typ_atom(p: &mut Parser) -> Option<CompletedMarker> {
    let m = p.start();

    Some(match p.current() {
        | Some(IDENT) => {
            ident_name(p);
            m.complete(p, TYPE_VAR)
        },
        | Some(TYPE) => {
            type_path(p);
            m.complete(p, TYPE_PATH)
        },
        | Some(L_PAREN) => {
            p.bump(L_PAREN);
            if p.eat(R_PAREN) {
                m.complete(p, TYPE_PARENS)
            } else {
                typ(p);
                p.expect(R_PAREN);
                m.complete(p, TYPE_PARENS)
            }
        },
        | _ => {
            p.error(TYP_TOKENS);
            m.abandon(p);
            return None;
        },
    })
}

fn typ(p: &mut Parser) {
    typ_func(p);
}

fn typ_app(p: &mut Parser) -> Option<CompletedMarker> {
    let mut m = typ_atom(p)?;

    if p.at_ts(PEEK_TYP) {
        let n = m.precede(p);
        while !p.eof() && p.at_ts(PEEK_TYP) {
            typ_atom(p)?;
        }
        m = n.complete(p, TYPE_APP);
    }

    Some(m)
}

fn typ_func(p: &mut Parser) -> Option<CompletedMarker> {
    let start = p.checkpoint();
    let m = p.start();

    typ_app(p);
    while !p.eof() && p.eat(COMMA) {
        typ_app(p);
    }

    if p.eat(ARROW) {
        typ_func(p);
        return Some(m.complete(p, TYPE_FUNC));
    }

    m.abandon(p);
    start.restore(p);
    typ_app(p)
}

const PAT_TOKENS: [SyntaxKind; 7] = [IDENT, TYPE, INT, FLOAT, CHAR, STRING, L_PAREN];
const PEEK_PAT: TokenSet = TokenSet::new(&PAT_TOKENS);

fn pat_atom(p: &mut Parser) -> Option<CompletedMarker> {
    let m = p.start();

    Some(match p.current() {
        | Some(IDENT) => {
            ident_name(p);
            if p.at(AT) && p.nth_at_ts(1, PEEK_PAT) {
                p.bump(AT);
                pat_atom(p);
            }
            m.complete(p, PAT_BIND)
        },
        | Some(TYPE) => {
            path(p);
            m.complete(p, PAT_PATH)
        },
        | Some(INT | FLOAT | CHAR | STRING) => {
            literal(p);
            m.complete(p, PAT_LITERAL)
        },
        | Some(L_PAREN) => {
            p.bump(L_PAREN);
            if p.eat(R_PAREN) {
                m.complete(p, PAT_UNIT)
            } else {
                pat(p);
                p.expect(R_PAREN);
                m.complete(p, PAT_PARENS)
            }
        },
        | _ => {
            p.error(PAT_TOKENS);
            m.abandon(p);
            return None;
        },
    })
}

fn pat(p: &mut Parser) {
    pat_app(p);
}

fn pat_app(p: &mut Parser) -> Option<CompletedMarker> {
    let mut m = pat_atom(p)?;

    if p.at_ts(PEEK_PAT) {
        let n = m.precede(p);
        while !p.eof() && p.at_ts(PEEK_PAT) {
            pat_atom(p);
        }
        m = n.complete(p, PAT_APP);
    }

    Some(m)
}

const EXPR_TOKENS: [SyntaxKind; 12] = [
    TYPE, IDENT, INT, FLOAT, CHAR, STRING, L_PAREN, LYT_START, FN_KW, DO_KW, MATCH_KW, IF_KW,
];
const PEEK_EXPR: TokenSet = TokenSet::new(&EXPR_TOKENS);

fn expr_atom(p: &mut Parser) -> Option<CompletedMarker> {
    let m = p.start();

    Some(match p.current() {
        | Some(TYPE | IDENT) => {
            path(p);
            m.complete(p, EXPR_PATH)
        },
        | Some(INT | FLOAT | CHAR | STRING) => {
            literal(p);
            m.complete(p, EXPR_LITERAL)
        },
        | Some(L_PAREN) => {
            p.bump(L_PAREN);
            if p.eat(R_PAREN) {
                m.complete(p, EXPR_UNIT)
            } else {
                expr(p);
                p.expect(R_PAREN);
                m.complete(p, EXPR_PARENS)
            }
        },
        | Some(LYT_START) => {
            block(p, PEEK_PAT | PEEK_EXPR, stmt(false));
            m.complete(p, EXPR_BLOCK)
        },
        | Some(FN_KW) => {
            p.bump(FN_KW);
            pat_atom(p);
            while !p.eof() && p.at_ts(PEEK_PAT) {
                pat_atom(p);
            }
            p.expect(ARROW);
            expr(p);
            m.complete(p, EXPR_LAMBDA)
        },
        | Some(DO_KW) => {
            p.bump(DO_KW);
            block(p, PEEK_PAT | PEEK_EXPR | LEFT_ARROW, stmt(true));
            m.complete(p, EXPR_DO)
        },
        | Some(MATCH_KW) => {
            p.bump(MATCH_KW);
            expr(p);
            p.expect(WITH_KW);
            if p.at(LYT_START) {
                block(p, PIPE, match_arm);
            } else {
                while !p.eof() && p.eat(LYT_SEP) {
                    match_arm(p);
                }
            }
            m.complete(p, EXPR_MATCH)
        },
        | _ => {
            p.error(EXPR_TOKENS);
            m.abandon(p);
            return None;
        },
    })
}

fn expr(p: &mut Parser) {
    expr_typed(p);
}

fn expr_app(p: &mut Parser) -> Option<CompletedMarker> {
    let mut m = expr_atom(p)?;

    if p.at_ts(PEEK_EXPR) {
        let n = m.precede(p);
        while !p.eof() && p.at_ts(PEEK_EXPR) {
            expr_atom(p);
        }
        m = n.complete(p, EXPR_APP);
    }

    Some(m)
}

fn expr_infix(p: &mut Parser) -> Option<CompletedMarker> {
    let mut m = expr_app(p)?;

    if p.at_ts(SYMBOL | COMMA | AT | PIPE | TICK) {
        let n = m.precede(p);
        while !p.eof() && p.at_ts(SYMBOL | COMMA | AT | PIPE | TICK) {
            operator(p);
            expr_app(p);
        }
        m = n.complete(p, EXPR_INFIX);
    }

    Some(m)
}

fn expr_typed(p: &mut Parser) -> Option<CompletedMarker> {
    let mut m = expr_infix(p)?;

    if p.eat(DBL_COLON) {
        let n = m.precede(p);
        typ(p);
        m = n.complete(p, EXPR_TYPED);
    }

    Some(m)
}

fn stmt(allow_bind: bool) -> impl FnMut(&mut Parser) {
    move |p: &mut Parser| {
        let m = p.start();
        let start = p.checkpoint();

        if allow_bind && p.eat(LEFT_ARROW) {
            expr(p);
            m.complete(p, STMT_BIND);
            return;
        }

        pat(p);

        if p.eat(EQUALS) {
            expr(p);
            m.complete(p, STMT_LET);
            return;
        } else if allow_bind && p.eat(LEFT_ARROW) {
            expr(p);
            m.complete(p, STMT_BIND);
            return;
        }

        start.restore(p);
        expr(p);
        m.complete(p, STMT_EXPR);
    }
}

fn match_arm(p: &mut Parser) {
    let m = p.start();
    p.expect(PIPE);
    pat(p);
    if p.at(IF_KW) {
        let n = p.start();
        p.bump(IF_KW);
        expr(p);
        n.complete(p, MATCH_GUARD);
    }
    p.expect(ARROW);
    expr(p);
    m.complete(p, MATCH_ARM);
}

fn literal(p: &mut Parser) {
    let m = p.start();

    if p.eat(INT) {
        m.complete(p, LIT_INT);
    } else if p.eat(FLOAT) {
        m.complete(p, LIT_FLOAT);
    } else if p.eat(CHAR) {
        m.complete(p, LIT_CHAR);
    } else if p.eat(STRING) {
        m.complete(p, LIT_STRING);
    } else {
        p.error([INT, FLOAT, CHAR, STRING]);
        m.abandon(p);
    }
}

fn operator(p: &mut Parser) {
    if p.eat(TICK) {
        path(p);
        p.expect(TICK);
    } else {
        let m = p.start();
        let n = p.start();
        let o = p.start();
        p.expect_ts(SYMBOL | COMMA | AT | PIPE);
        o.complete(p, NAME_REF);
        n.complete(p, PATH_SEGMENT);
        m.complete(p, PATH);
    }
}

fn path(p: &mut Parser) {
    let m = p.start();

    while !p.eof() && p.at(TYPE) && p.nth_at(1, DOT) {
        let s = p.start();
        type_ref(p);
        s.complete(p, PATH_SEGMENT);
        p.bump(DOT);
    }

    let s = p.start();
    name_ref(p);
    s.complete(p, PATH_SEGMENT);
    m.complete(p, PATH);
}

fn type_path(p: &mut Parser) {
    let m = p.start();

    while !p.eof() && p.at(TYPE) && p.nth_at(1, DOT) {
        let s = p.start();
        type_ref(p);
        s.complete(p, PATH_SEGMENT);
        p.bump(DOT);
    }

    let s = p.start();
    type_ref(p);
    s.complete(p, PATH_SEGMENT);
    m.complete(p, PATH);
}

fn name(p: &mut Parser) {
    let m = p.start();
    match p.current() {
        | Some(IDENT | TYPE) => p.bump_any(),
        | Some(L_PAREN) if p.nth_at_ts(1, SYMBOL | COMMA) && p.nth_at(2, R_PAREN) => {
            p.bump(L_PAREN);
            p.bump_any();
            p.bump(R_PAREN);
        },
        | _ => {
            p.error([IDENT, TYPE, L_PAREN]);
            m.abandon(p);
            return;
        },
    }
    m.complete(p, NAME);
}

fn name_ref(p: &mut Parser) {
    let m = p.start();
    match p.current() {
        | Some(IDENT | TYPE) => p.bump_any(),
        | Some(L_PAREN) if p.nth_at_ts(1, SYMBOL | COMMA) && p.nth_at(2, R_PAREN) => {
            p.bump(L_PAREN);
            p.bump_any();
            p.bump(R_PAREN);
        },
        | _ => {
            p.error([IDENT, TYPE, L_PAREN]);
            m.abandon(p);
            return;
        },
    }
    m.complete(p, NAME_REF);
}

fn ident_name(p: &mut Parser) {
    let m = p.start();
    p.expect(IDENT);
    m.complete(p, NAME);
}

fn ident_ref(p: &mut Parser) {
    let m = p.start();
    p.expect(IDENT);
    m.complete(p, NAME_REF);
}

fn type_name(p: &mut Parser) {
    let m = p.start();
    p.expect(TYPE);
    m.complete(p, NAME);
}

fn type_ref(p: &mut Parser) {
    let m = p.start();
    p.expect(TYPE);
    m.complete(p, NAME_REF);
}

fn symbol_name(p: &mut Parser) {
    let m = p.start();
    p.expect(L_PAREN);
    p.expect_ts(SYMBOL | COMMA);
    p.expect(R_PAREN);
    m.complete(p, NAME);
}

fn block(p: &mut Parser, peek: impl Into<TokenSet>, item: impl FnMut(&mut Parser)) {
    p.expect(LYT_START);
    separated(p, LYT_SEP, LYT_END, peek, item);
    p.expect(LYT_END);
}

fn separated(
    p: &mut Parser,
    sep: SyntaxKind,
    end: impl Into<TokenSet>,
    peek: impl Into<TokenSet>,
    mut item: impl FnMut(&mut Parser),
) {
    let end = end.into();
    let peek = peek.into();

    while !p.eof() && !p.at_ts(end) {
        if !p.at_ts(peek) {
            p.error(peek);

            while !p.eof() && !p.at_ts(peek) {
                p.bump_any();
            }

            if p.eof() {
                return;
            }
        }

        item(p);

        if !p.eof() && !p.at_ts(end) {
            p.expect(sep);
        }
    }
}
