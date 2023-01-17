use super::*;
use crate::parser::{CompletedMarker, Parser};
use crate::syntax_kind::*;
use crate::token_set::TokenSet;

const TYPE_RECOVERY_SET: TokenSet = TokenSet::new(&[R_PAREN, LYT_SEP, LYT_END]);

pub(crate) fn ty(p: &mut Parser) {
    if let Some(m) = infix(p, TokenSet::EMPTY) {
        if p.at(WHERE_KW) {
            let m = m.precede(p);

            items::where_clause(p);
            m.complete(p, TYPE_WHERE);
        }
    }
}

pub(crate) fn infix(p: &mut Parser, disallow: impl Into<TokenSet> + Copy) -> Option<CompletedMarker> {
    let mut m = app(p)?;

    if peek_operator(p, disallow) {
        let ty = m.precede(p);

        while peek_operator(p, disallow) {
            p.bump_any();
            app(p);
        }

        m = ty.complete(p, TYPE_INFIX);
    }

    Some(m)
}

pub(crate) fn app(p: &mut Parser) -> Option<CompletedMarker> {
    let mut m = atom(p)?;

    while peek(p) {
        let ty = m.precede(p);
        let _ = atom(p);

        m = ty.complete(p, TYPE_APP);
    }

    Some(m)
}

pub(crate) fn atom(p: &mut Parser) -> Option<CompletedMarker> {
    let m = p.start();

    match p.current() {
        | IDENT => {
            paths::path(p);
            Some(m.complete(p, TYPE_PATH))
        },
        | UNDERSCORE => {
            p.bump(UNDERSCORE);
            Some(m.complete(p, TYPE_HOLE))
        },
        | INT => {
            let lit = p.start();

            p.bump(INT);
            lit.complete(p, LIT_INT);
            Some(m.complete(p, TYPE_FIGURE))
        },
        | STRING => {
            let lit = p.start();

            p.bump(STRING);
            lit.complete(p, LIT_STRING);
            Some(m.complete(p, TYPE_SYMBOL))
        },
        | FORALL_KW => {
            p.bump(FORALL_KW);
            items::type_vars(p);
            p.expect(DOT);
            let _ = ty(p);
            Some(m.complete(p, TYPE_FORALL))
        },
        | L_PAREN => {
            p.bump(L_PAREN);

            if p.eat(R_PAREN) {
                Some(m.complete(p, TYPE_UNIT))
            } else if p.at(IDENT) && p.nth_at(1, DBL_COLON) {
                while !p.at(EOF) && !p.at(R_PAREN) {
                    let field = p.start();

                    paths::name(p);
                    p.expect(DBL_COLON);
                    infix(p, COMMA | PIPE);
                    field.complete(p, ROW_FIELD);

                    if !p.at(R_BRACE) && !p.at(PIPE) && !p.expect(COMMA) {
                        break;
                    }

                    if p.eat(PIPE) {
                        let tail = p.start();

                        ty(p);
                        tail.complete(p, ROW_TAIL);
                    }
                }

                p.expect(R_PAREN);
                Some(m.complete(p, TYPE_ROW))
            } else {
                let _ = ty(p);
                p.expect(R_PAREN);

                Some(m.complete(p, TYPE_PARENS))
            }
        },
        | L_BRACE => {
            p.bump(L_BRACE);

            while !p.at(EOF) && !p.at(R_BRACE) {
                let field = p.start();

                paths::name(p);
                p.expect(DBL_COLON);
                infix(p, COMMA | PIPE);
                field.complete(p, ROW_FIELD);

                if !p.at(R_BRACE) && !p.at(PIPE) {
                    p.expect(COMMA);
                }

                if p.eat(PIPE) {
                    let tail = p.start();

                    ty(p);
                    tail.complete(p, ROW_TAIL);
                }
            }

            p.expect(R_BRACE);
            Some(m.complete(p, TYPE_REC))
        },
        | _ => {
            p.err_recover("expected a type", TYPE_RECOVERY_SET);
            m.abandon(p);
            None
        },
    }
}

pub(crate) fn peek(p: &Parser) -> bool {
    match p.current() {
        | IDENT | UNDERSCORE | L_PAREN | L_BRACKET | L_BRACE | INT | STRING => true,
        | _ => false,
    }
}
