use super::*;
use crate::parser::{CompletedMarker, Parser};
use crate::syntax_kind::*;
use crate::token_set::TokenSet;

const TYPE_RECOVERY_SET: TokenSet = TokenSet::new(&[R_PAREN, COMMA]);

crate fn ty(p: &mut Parser) {
    if let Some(m) = func(p) {
        if p.at(WHERE_KW) {
            let m = m.precede(p);

            items::where_clause(p);
            m.complete(p, TYPE_WHERE);
        }
    }
}

crate fn func(p: &mut Parser) -> Option<CompletedMarker> {
    let mut m = list(p)?;

    if p.eat(ARROW) {
        let ty = m.precede(p);
        let _ = func(p);

        m = ty.complete(p, TYPE_FN);
    }

    Some(m)
}

crate fn list(p: &mut Parser) -> Option<CompletedMarker> {
    let ty = app(p)?;

    if p.at(COMMA) {
        let list = ty.precede(p);

        while p.eat(COMMA) {
            let _ = app(p);
        }

        Some(list.complete(p, TYPE_TUPLE))
    } else {
        Some(ty)
    }
}

crate fn app(p: &mut Parser) -> Option<CompletedMarker> {
    let mut m = atom(p)?;

    if peek(p) {
        let ty = m.precede(p);

        while peek(p) {
            let _ = atom(p);
        }

        m = ty.complete(p, TYPE_APP);
    }

    Some(m)
}

crate fn atom(p: &mut Parser) -> Option<CompletedMarker> {
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
        | STAR => {
            p.bump(STAR);
            atom(p);
            Some(m.complete(p, TYPE_PTR))
        },
        | HASH => {
            p.bump(HASH);
            p.expect(L_PAREN);

            while !p.at(EOF) && !p.at(R_PAREN) {
                let field = p.start();

                paths::name(p);
                p.expect(DBL_COLON);
                ty(p);
                field.complete(p, ROW_FIELD);

                if !p.at(R_PAREN) && !p.at(PIPE) {
                    p.expect(COMMA);
                }

                if p.eat(PIPE) {
                    let tail = p.start();

                    ty(p);
                    tail.complete(p, ROW_TAIL);
                }
            }

            p.expect(R_PAREN);
            Some(m.complete(p, TYPE_ROW))
        },
        | L_BRACKET => {
            p.bump(L_BRACKET);

            if p.eat(STAR) {
                opt_sentinel(p);
                p.expect(R_BRACKET);
                atom(p);
                Some(m.complete(p, TYPE_PTR))
            } else if p.eat(INT) {
                p.expect(R_BRACKET);
                atom(p);
                Some(m.complete(p, TYPE_ARRAY))
            } else {
                p.expect(R_BRACKET);
                atom(p);
                Some(m.complete(p, TYPE_SLICE))
            }
        },
        | L_PAREN => {
            p.bump(L_PAREN);

            if p.eat(R_PAREN) {
                Some(m.complete(p, TYPE_TUPLE))
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
                ty(p);
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

crate fn peek(p: &Parser) -> bool {
    match p.current() {
        | IDENT | STAR | HASH | L_PAREN | L_BRACKET | L_BRACE | INT | STRING => true,
        | _ => false,
    }
}

fn opt_sentinel(p: &mut Parser) {
    if p.at(COLON) {
        let m = p.start();

        p.bump(COLON);
        p.expect(INT);
        m.complete(p, SENTINEL);
    }
}
