use super::*;
use crate::parser::Parser;
use crate::syntax_kind::*;
use crate::token_set::TokenSet;

pub(crate) fn attr(p: &mut Parser) {
    let attr = p.start();

    p.bump(AT);
    p.expect(IDENT);

    if p.eat(EQUALS) {
        exprs::literal(p);
    } else if p.at(L_PAREN) {
        attr_args(p);
    }

    attr.complete(p, ATTR);
}

pub(crate) fn attr_args(p: &mut Parser) {
    let m = p.start();

    p.expect(L_PAREN);

    while !p.at(EOF) && !p.at_ts(TokenSet::new(&[R_PAREN, AT, LYT_SEP])) {
        attr_arg(p);

        if !p.at(R_PAREN) {
            p.expect(COMMA);
        }
    }

    p.expect(R_PAREN);
    m.complete(p, ATTR_ARGS);
}

pub(crate) fn attr_arg(p: &mut Parser) {
    let m = p.start();

    if p.at(IDENT) {
        paths::name_ref(p);

        if p.at(L_PAREN) {
            attr_args(p);
            m.complete(p, ATTR_ARG_CALL);
        } else if p.eat(EQUALS) {
            exprs::literal(p);
            m.complete(p, ATTR_ARG_EQUAL);
        } else {
            m.complete(p, ATTR_ARG_IDENT);
        }
    } else if p.at_ts(TokenSet::new(&[INT, FLOAT, CHAR, STRING])) {
        exprs::literal(p);
        m.complete(p, ATTR_ARG_LIT);
    } else {
        p.error("expected an attribute arg");
        m.abandon(p);
    }
}
