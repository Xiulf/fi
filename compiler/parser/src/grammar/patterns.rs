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

    match p.current() {
        | IDENT => {
            if p.nth_at(1, DOT) {
                paths::path(p);
                m.complete(p, PAT_CTOR);
            } else {
                paths::name(p);

                if p.eat(AT) {
                    pattern(p);
                }

                m.complete(p, PAT_BIND);
            }
        },
        | UNDERSCORE => {
            p.bump(UNDERSCORE);
            m.complete(p, PAT_WILDCARD);
        },
        | INT | FLOAT | CHAR | STRING => {
            exprs::literal(p);
            m.complete(p, PAT_LITERAL);
        },
        | L_PAREN => {
            p.bump(L_PAREN);

            if p.eat(R_PAREN) {
                m.complete(p, PAT_TUPLE);
            } else {
                let mut is_tuple = false;
                let _ = pattern(p);

                while p.eat(COMMA) {
                    is_tuple = true;

                    if p.at(R_PAREN) {
                        break;
                    } else {
                        pattern(p);
                    }
                }

                p.expect(R_PAREN);

                if is_tuple {
                    m.complete(p, PAT_TUPLE);
                } else {
                    m.complete(p, PAT_PARENS);
                }
            }
        },
        | L_BRACE => {
            p.bump(L_BRACE);
            record_fields(p, pattern);
            p.expect(R_BRACE);
            m.complete(p, PAT_RECORD);
        },
        | _ => {
            p.error("expected a pattern");
            m.abandon(p);
        },
    }
}

crate fn record_fields(p: &mut Parser, mut f: impl FnMut(&mut Parser)) {
    while !p.at(R_BRACE) {
        let field = p.start();

        if p.at(IDENT) && p.nth_at(1, COLON) {
            paths::name(p);
            p.bump(COLON);
            f(p);
            field.complete(p, FIELD_NORMAL);
        } else {
            paths::name(p);
            field.complete(p, FIELD_PUN);
        }

        if !p.at(R_BRACE) {
            p.expect(COMMA);
        }
    }
}

fn peek(p: &mut Parser) -> bool {
    p.at_ts(TokenSet::new(&[
        IDENT, UNDERSCORE, L_PAREN, L_BRACE, INT, FLOAT, CHAR, STRING,
    ]))
}
