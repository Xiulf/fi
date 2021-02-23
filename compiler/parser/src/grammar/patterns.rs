use super::*;
use crate::parser::Parser;
use crate::syntax_kind::*;
use crate::token_set::TokenSet;

crate fn pattern(p: &mut Parser) {
    let m = p.start();

    app(p);

    if p.eat(DBL_COLON) {
        types::func(p);
        m.complete(p, PAT_TYPED);
    } else {
        m.abandon(p);
    }
}

crate fn app(p: &mut Parser) {
    let m = p.start();

    atom(p);

    if peek(p) {
        while peek(p) {
            atom(p);
        }

        m.complete(p, PAT_APP);
    } else {
        m.abandon(p);
    }
}

crate fn atom(p: &mut Parser) {
    let m = p.start();

    if p.at(IDENT) {
        if p.nth_at(1, SLASH) {
            paths::path(p);
            m.complete(p, PAT_CTOR);
        } else {
            paths::name(p);
            m.complete(p, PAT_BIND);
        }
    } else if p.eat(UNDERSCORE) {
        m.complete(p, PAT_WILDCARD);
    } else {
        p.error("expected a pattern");
        m.abandon(p);
    }
}

fn peek(p: &mut Parser) -> bool {
    p.at_ts(TokenSet::new(&[
        IDENT, UNDERSCORE, L_PAREN, L_BRACE, L_BRACKET, INT, FLOAT, CHAR, STRING, RSTRING,
    ]))
}
