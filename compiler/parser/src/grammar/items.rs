use super::*;
use crate::parser::{Marker, Parser};
use crate::syntax_kind::*;
use crate::token_set::TokenSet;

pub(crate) fn any_item(p: &mut Parser) {
    let m = p.start();

    while p.at(AT) {
        attributes::attr(p);
        p.eat(LYT_SEP);
    }

    match p.current() {
        | IMPORT_KW => {
            import(p, m);
        },
        | INFIX_KW | INFIXL_KW | INFIXR_KW | POSTFIX_KW | PREFIX_KW => {
            fixity(p, m);
        },
        | FOREIGN_KW => {
            foreign(p, m);
        },
        // | FN_KW => {
        | IDENT => {
            fun(p, m);
        },
        | STATIC_KW => {
            static_(p, m);
        },
        | CONST_KW => {
            const_(p, m);
        },
        | TYPE_KW => {
            type_(p, m);
        },
        | CLASS_KW => {
            class(p, m);
        },
        | MEMBER_KW => {
            member(p, m);
        },
        | _ => {
            m.abandon(p);
            p.error("expected an item");

            while !p.at_ts(TokenSet::new(&[EOF, LYT_SEP, LYT_END])) {
                p.bump_any();
            }
        },
    }
}

pub(crate) fn import(p: &mut Parser, m: Marker) {
    p.expect(IMPORT_KW);
    paths::module_name_ref(p);

    if p.eat(L_PAREN) {
        let items = p.start();

        while !p.at(EOF) && !p.at(R_PAREN) {
            paths::name_or_symbol_ref(p);

            if !p.at(R_PAREN) {
                p.expect(COMMA);
            }
        }

        p.expect(R_PAREN);
        items.complete(p, IMPORT_ITEMS);
    }

    if p.eat(AS_KW) {
        paths::name(p);
    }

    m.complete(p, IMPORT);
}

pub(crate) fn fixity(p: &mut Parser, m: Marker) {
    if p.at(INFIX_KW) || p.at(INFIXL_KW) || p.at(INFIXR_KW) {
        p.bump_any();
        p.expect(INT);
        paths::path(p);
        p.expect(AS_KW);
        paths::symbol(p);
        m.complete(p, ITEM_FIXITY);
    } else if p.eat(POSTFIX_KW) {
        paths::path(p);
        p.expect(AS_KW);
        paths::symbol(p);
        m.complete(p, ITEM_FIXITY);
    } else if p.eat(PREFIX_KW) {
        paths::path(p);
        p.expect(AS_KW);
        paths::symbol(p);
        m.complete(p, ITEM_FIXITY);
    } else {
        p.error("expected 'infix', 'infixl', 'infixr', 'postfix' or 'prefix'");
        m.abandon(p);
    }
}

pub(crate) fn foreign(p: &mut Parser, m: Marker) {
    p.expect(FOREIGN_KW);

    match p.current() {
        // | FN_KW => fun(p, m),
        | IDENT => fun(p, m),
        | STATIC_KW => static_(p, m),
        | TYPE_KW => type_(p, m),
        | _ => {
            p.error("expected 'fn', 'static' or 'type'");
            m.abandon(p);
            return;
        },
    }
}

pub(crate) fn fun(p: &mut Parser, m: Marker) {
    // p.expect(FN_KW);
    paths::name(p);

    if p.eat(DBL_COLON) {
        types::ty(p);
        m.complete(p, ITEM_FUN);
    } else {
        while !p.at_ts(TokenSet::new(&[EOF, LYT_SEP, LYT_END, EQUALS])) {
            patterns::atom(p);
        }

        p.expect(EQUALS);

        let body = p.start();

        exprs::block(p, false);
        body.complete(p, EXPR_DO);
        m.complete(p, ITEM_FUN);
    }
}

pub(crate) fn static_(p: &mut Parser, m: Marker) {
    p.expect(STATIC_KW);
    paths::name(p);

    if p.eat(DBL_COLON) {
        types::ty(p);
        m.complete(p, ITEM_STATIC);
    } else {
        p.expect(EQUALS);
        exprs::expr(p);
        m.complete(p, ITEM_STATIC);
    }
}

pub(crate) fn const_(p: &mut Parser, m: Marker) {
    p.expect(CONST_KW);
    paths::name(p);

    if p.eat(DBL_COLON) {
        types::ty(p);
        m.complete(p, ITEM_CONST);
    } else {
        p.expect(EQUALS);
        exprs::expr(p);
        m.complete(p, ITEM_CONST);
    }
}

pub(crate) fn type_(p: &mut Parser, m: Marker) {
    p.expect(TYPE_KW);
    paths::name(p);

    if p.eat(DBL_COLON) {
        types::ty(p);
    } else {
        type_vars(p);
        p.expect(EQUALS);

        if p.at(PIPE) {
            while p.eat(PIPE) {
                let m = p.start();

                while p.at(AT) {
                    attributes::attr(p);
                }

                ctor(p, m);
            }
        } else {
            types::ty(p);
        }
    }

    m.complete(p, ITEM_TYPE);
}

pub(crate) fn ctor(p: &mut Parser, m: Marker) {
    paths::name(p);

    while types::peek(p) {
        types::atom(p);
    }

    m.complete(p, ITEM_CTOR);
}

pub(crate) fn class(p: &mut Parser, m: Marker) {
    p.expect(CLASS_KW);
    paths::name(p);
    type_vars(p);

    if p.eat(PIPE) {
        fun_dep(p);

        while p.eat(COMMA) {
            fun_dep(p);
        }
    }

    if p.at(WHERE_KW) {
        where_clause(p);
    }

    if p.eat(EQUALS) {
        p.expect(LYT_START);

        while !p.at_ts(TokenSet::new(&[EOF, LYT_END])) {
            assoc_item(p);

            if !p.at(LYT_END) {
                p.expect(LYT_SEP);
            }
        }

        p.expect(LYT_END);
    }

    m.complete(p, ITEM_CLASS);
}

pub(crate) fn where_clause(p: &mut Parser) {
    let m = p.start();

    p.expect(WHERE_KW);
    p.expect(LYT_START);
    where_clause_item(p);

    while p.eat(COMMA) || p.eat(LYT_SEP) {
        where_clause_item(p);
    }

    p.expect(LYT_END);
    m.complete(p, WHERE_CLAUSE);
}

pub(crate) fn where_clause_item(p: &mut Parser) {
    let m = p.start();

    if p.at(IDENT) && p.nth_at(1, DBL_COLON) {
        paths::name_ref(p);
        p.bump(DBL_COLON);
        types::infix(p, true);
        m.complete(p, TYPE_VAR_KIND);
    } else {
        paths::path(p);

        while types::peek(p) {
            types::atom(p);
        }

        m.complete(p, CONSTRAINT);
    }
}

pub(crate) fn type_vars(p: &mut Parser) {
    let m = p.start();

    while p.at(IDENT) {
        paths::name(p);
    }

    m.complete(p, TYPE_VARS);
}

pub(crate) fn fun_dep(p: &mut Parser) {
    let m = p.start();

    while p.at(IDENT) {
        paths::name_ref(p);
    }

    p.expect(ARROW);

    while p.at(IDENT) {
        paths::name_ref(p);
    }

    m.complete(p, FUN_DEP);
}

pub(crate) fn member(p: &mut Parser, m: Marker) {
    p.expect(MEMBER_KW);
    types::atom(p);

    while !p.at(OF_KW) {
        types::atom(p);
    }

    p.expect(OF_KW);
    paths::path(p);

    if p.at(WHERE_KW) {
        where_clause(p);
    }

    if p.eat(EQUALS) {
        p.expect(LYT_START);

        while !p.at(EOF) && !p.at(LYT_END) {
            assoc_item(p);

            if !p.at(LYT_END) {
                p.expect(LYT_SEP);
            }
        }

        p.expect(LYT_END);
    }

    m.complete(p, ITEM_MEMBER);
}

pub(crate) fn assoc_item(p: &mut Parser) {
    let m = p.start();

    while p.at(AT) {
        attributes::attr(p);
        p.eat(LYT_SEP);
    }

    match p.current() {
        // | FN_KW => fun(p, m),
        | IDENT => fun(p, m),
        | STATIC_KW => static_(p, m),
        | _ => {
            p.error("expected an associated item");
            m.abandon(p);

            while !p.at_ts(TokenSet::new(&[EOF, LYT_SEP, LYT_END])) {
                p.bump_any();
            }
        },
    }
}
