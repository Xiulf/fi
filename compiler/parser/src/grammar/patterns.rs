use super::*;
use crate::parser::{CompletedMarker, Parser};
use crate::syntax_kind::*;
use crate::token_set::TokenSet;

pub(crate) fn pattern(p: &mut Parser) {
    if let Some(m) = infix(p) {
        if p.eat(DBL_COLON) {
            let pat = m.precede(p);

            types::ty(p);
            pat.complete(p, PAT_TYPED);
        }
    }
}

fn peek_operator_pat(p: &mut Parser) -> bool {
    peek_operator(p) && !p.at_ts(TokenSet::new(&[ARROW, LEFT_ARROW, EQUALS]))
}

pub(crate) fn infix(p: &mut Parser) -> Option<CompletedMarker> {
    let mut m = app(p)?;

    if peek_operator_pat(p) {
        let pat = m.precede(p);

        while peek_operator_pat(p) {
            p.bump_any();
            app(p);
        }

        m = pat.complete(p, PAT_INFIX);
    }

    Some(m)
}

pub(crate) fn app(p: &mut Parser) -> Option<CompletedMarker> {
    let mut m = atom(p)?;

    if peek(p) {
        let pat = m.precede(p);

        while peek(p) {
            atom(p);
        }

        m = pat.complete(p, PAT_APP);
    }

    Some(m)
}

pub(crate) fn atom(p: &mut Parser) -> Option<CompletedMarker> {
    let m = p.start();

    match p.current() {
        | IDENT => {
            if p.nth_at(1, PATH_SEP) {
                paths::path(p);
                Some(m.complete(p, PAT_CTOR))
            } else {
                paths::name(p);

                if p.eat(AT) {
                    pattern(p);
                }

                Some(m.complete(p, PAT_BIND))
            }
        },
        | UNDERSCORE => {
            p.bump(UNDERSCORE);
            Some(m.complete(p, PAT_WILDCARD))
        },
        | INT | FLOAT | CHAR | STRING => {
            exprs::literal(p);
            Some(m.complete(p, PAT_LITERAL))
        },
        | L_PAREN => {
            p.bump(L_PAREN);

            if p.eat(R_PAREN) {
                Some(m.complete(p, PAT_UNIT))
            } else {
                let _ = pattern(p);
                p.expect(R_PAREN);

                Some(m.complete(p, PAT_PARENS))
            }
        },
        | L_BRACE => {
            p.bump(L_BRACE);
            record_fields(p, pattern, true);
            p.expect(R_BRACE);

            Some(m.complete(p, PAT_RECORD))
        },
        | _ => {
            p.error("expected a pattern");
            m.abandon(p);
            None
        },
    }
}

pub(crate) fn record_fields(p: &mut Parser, mut f: impl FnMut(&mut Parser), allow_rest: bool) {
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

        if allow_rest && p.eat(DBL_DOT) {
            break;
        }
    }
}

fn peek(p: &mut Parser) -> bool {
    p.at_ts(TokenSet::new(&[
        IDENT, UNDERSCORE, L_PAREN, L_BRACE, INT, FLOAT, CHAR, STRING,
    ]))
}
