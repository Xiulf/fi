use super::*;
use crate::parser::Parser;
use crate::syntax_kind::*;

crate fn pattern(p: &mut Parser) {
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
    } else {
        p.error("expected a pattern");
        m.abandon(p);
    }
}

fn peek(p: &mut Parser) -> bool {
    p.at(IDENT) || p.at(L_PAREN) || p.at(L_BRACE) || p.at(L_BRACKET) || p.at(INT) || p.at(FLOAT) || p.at(CHAR) || p.at(STRING) || p.at(RSTRING)
}
