use super::*;
use crate::parser::{CompletedMarker, Parser};
use crate::syntax_kind::*;
use crate::token_set::TokenSet;

const TYPE_RECOVERY_SET: TokenSet = TokenSet::new(&[R_PAREN, COMMA]);

crate fn ty(p: &mut Parser) {
    if let Some(m) = ctnt(p) {
        if p.eat(DBL_COLON) {
            let m = m.precede(p);
            let _ = ctnt(p);

            m.complete(p, TYPE_KINDED);
        }
    }
}

crate fn ctnt(p: &mut Parser) -> Option<CompletedMarker> {
    if p.at(IDENT) {
        for i in 1..100 {
            if p.nth_at(i, FAT_ARROW) {
                let m = p.start();

                constraint(p);
                p.expect(FAT_ARROW);

                let _ = ctnt(p);

                return Some(m.complete(p, TYPE_CTNT));
            } else if p.nth_at_ts(
                i,
                TokenSet::new(&[LYT_START, LYT_SEP, LYT_END, COMMA, R_PAREN, R_BRACE]),
            ) {
                break;
            }
        }
    }

    func(p)
}

crate fn func(p: &mut Parser) -> Option<CompletedMarker> {
    let mut m = app(p)?;

    if p.eat(ARROW) {
        let ty = m.precede(p);
        let _ = func(p);

        m = ty.complete(p, TYPE_FN);
    }

    Some(m)
}

crate fn app(p: &mut Parser) -> Option<CompletedMarker> {
    let mut m = atom(p)?;

    if peek(p) {
        while peek(p) {
            let ty = m.precede(p);
            let _ = atom(p);

            m = ty.complete(p, TYPE_APP);
        }
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
                let mut is_tuple = false;

                ty(p);

                while p.eat(COMMA) {
                    is_tuple = true;

                    if p.at(R_PAREN) {
                        break;
                    } else {
                        ty(p);
                    }
                }

                p.expect(R_PAREN);

                if is_tuple {
                    Some(m.complete(p, TYPE_TUPLE))
                } else {
                    Some(m.complete(p, TYPE_PARENS))
                }
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
        | FOR_KW => {
            p.bump(FOR_KW);

            while p.at_ts(TokenSet::new(&[L_PAREN, IDENT])) {
                type_var(p);
            }

            p.expect(DOT);
            ctnt(p);
            Some(m.complete(p, TYPE_FOR))
        },
        | _ => {
            p.err_recover("expected a type", TYPE_RECOVERY_SET);
            m.abandon(p);
            None
        },
    }
}

crate fn generics(p: &mut Parser, end: SyntaxKind) {
    let m = p.start();

    while !p.at_ts(TokenSet::new(&[EOF, COLON, end])) {
        type_var(p);
    }

    if p.eat(COLON) {
        while !p.at(EOF) && !p.at(end) {
            constraint(p);

            if !p.at(end) {
                p.expect(COMMA);
            }
        }
    }

    m.complete(p, GENERICS);
}

crate fn constraint(p: &mut Parser) {
    let m = p.start();

    paths::path(p);

    while peek(p) {
        atom(p);
    }

    m.complete(p, CONSTRAINT);
}

crate fn type_var(p: &mut Parser) {
    let m = p.start();

    if p.eat(L_PAREN) {
        paths::name(p);
        p.expect(DBL_COLON);
        func(p);
        p.expect(R_PAREN);
        m.complete(p, TYPE_VAR);
    } else {
        paths::name(p);
        m.complete(p, TYPE_VAR);
    }
}

crate fn peek(p: &Parser) -> bool {
    match p.current() {
        | IDENT | STAR | HASH | L_PAREN | L_BRACKET | L_BRACE | FOR_KW | INT | STRING => true,
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
