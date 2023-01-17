mod attributes;
mod exprs;
mod items;
mod paths;
mod patterns;
mod types;

use crate::parser::Parser;
use crate::syntax_kind::*;
use crate::token_set::TokenSet;

pub(crate) fn root(p: &mut Parser) {
    let s = p.start();
    let m = p.start();

    p.eat(LYT_SEP);

    while !p.at(EOF) && p.at(AT) {
        attributes::attr(p);
        p.eat(LYT_SEP);
    }

    items::module(p, m);
    p.expect(EOF);
    s.complete(p, SOURCE_FILE);
}

fn exports(p: &mut Parser) {
    let m = p.start();

    p.expect(L_PAREN);

    while !p.at(EOF) && !p.at_ts(TokenSet::new(&[R_PAREN, EQUALS])) {
        export(p);

        if !p.at_ts(TokenSet::new(&[R_PAREN, EQUALS])) && !p.expect(COMMA) {
            break;
        }
    }

    p.expect(R_PAREN);
    m.complete(p, EXPORTS);
}

fn export(p: &mut Parser) {
    let m = p.start();

    match p.current() {
        | IDENT if p.nth_at(1, L_PAREN) => {
            paths::name_or_symbol_ref(p);

            let inner = p.start();

            p.bump(L_PAREN);

            if p.eat(DBL_DOT) {
                p.expect(R_PAREN);
                inner.complete(p, EXPORT_GROUP_ALL);
            } else {
                while !p.at(EOF) && !p.at(R_PAREN) {
                    paths::name_ref(p);

                    if !p.at(R_PAREN) && !p.expect(COMMA) {
                        break;
                    }
                }

                p.expect(R_PAREN);
                inner.complete(p, EXPORT_GROUP_NAMED);
            }

            m.complete(p, EXPORT_GROUP);
        },
        | IDENT | SYMBOL => {
            paths::name_or_symbol_ref(p);
            m.complete(p, EXPORT_NAME);
        },
        | MODULE_KW => {
            p.bump(MODULE_KW);
            paths::path(p);
            m.complete(p, EXPORT_MODULE);
        },
        | _ => {
            p.error("exported an export");
            p.bump_any();
            m.abandon(p);
        },
    }
}

fn peek_operator(p: &mut Parser, disallow: impl Into<TokenSet>) -> bool {
    const OPERATORS: TokenSet = TokenSet::new(&[OPERATOR, ARROW, LEFT_ARROW, DBL_DOT, COMMA, COLON, PIPE, EQUALS, AT]);

    p.at_ts(OPERATORS) && !p.at_ts(disallow.into())
}

pub(crate) mod fragments {
    use super::*;

    pub(crate) fn path(p: &mut Parser) {
        paths::path(p);
    }

    pub(crate) fn type_(p: &mut Parser) {
        types::ty(p);
    }
}
