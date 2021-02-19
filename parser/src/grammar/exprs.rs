use super::*;
use crate::parser::{CompletedMarker, Parser};
use crate::syntax_kind::*;

crate fn expr(p: &mut Parser) {
    expr_(p, true);
}

fn expr_(p: &mut Parser, allow_do: bool) {
    if let Some(m) = infix(p, allow_do) {
        if p.at(DBL_COLON) {
            let expr = m.precede(p);
            let _ = p.bump(DBL_COLON);
            let _ = types::ty(p);

            expr.complete(p, EXPR_TYPED);
        }
    }
}

crate fn infix(p: &mut Parser, allow_do: bool) -> Option<CompletedMarker> {
    let mut m = app(p, allow_do)?;

    if p.at(OPERATOR) {
        while p.at(OPERATOR) {
            let expr = m.precede(p);
            let _ = p.bump(OPERATOR);
            let _ = app(p, allow_do);

            m = expr.complete(p, EXPR_INFIX);
        }
    }

    Some(m)
}

crate fn app(p: &mut Parser, allow_do: bool) -> Option<CompletedMarker> {
    let mut m = postfix(p, allow_do)?;

    if peek(p, allow_do) {
        while peek(p, allow_do) {
            let expr = m.precede(p);
            let _ = postfix(p, allow_do);

            m = expr.complete(p, EXPR_APP);
        }
    }

    Some(m)
}

crate fn postfix(p: &mut Parser, allow_do: bool) -> Option<CompletedMarker> {
    let mut m = atom(p, allow_do)?;

    loop {
        match p.current() {
            | DOT => {
                let expr = m.precede(p);

                p.bump(DOT);

                match p.current() {
                    | STAR => {
                        p.bump(STAR);
                        m = expr.complete(p, EXPR_DEREF);
                    },
                    | _ => {
                        p.error("expected '*', '(' or an identifier");
                        expr.abandon(p);
                        return None;
                    },
                }
            },
            | _ => break,
        }
    }

    Some(m)
}

crate fn atom(p: &mut Parser, allow_do: bool) -> Option<CompletedMarker> {
    let m = p.start();

    match p.current() {
        | IDENT => {
            paths::path(p);
            Some(m.complete(p, EXPR_PATH))
        },
        | INT | FLOAT | CHAR | STRING | RSTRING => {
            literal(p);
            Some(m.complete(p, EXPR_LITERAL))
        },
        | DO_KW if allow_do => {
            p.bump(DO_KW);
            block(p);
            Some(m.complete(p, EXPR_DO))
        },
        | DO_KW => {
            p.error("do blocks are not allowed in this position");
            p.bump_any();
            m.abandon(p);
            None
        },
        | IF_KW | UNLESS_KW => {
            p.bump_any();
            expr_(p, false);

            match p.current() {
                | THEN_KW => {
                    p.bump(THEN_KW);
                    expr(p);
                },
                | DO_KW => {
                    p.bump(DO_KW);
                    block(p);
                },
                | _ => {
                    p.error("expected 'then' or 'do'");
                    m.abandon(p);
                    return None;
                },
            }

            if p.eat(ELSE_KW) {
                expr(p);
            }

            Some(m.complete(p, EXPR_IF))
        },
        | L_PAREN => {
            p.bump(L_PAREN);

            if p.eat(R_PAREN) {
                Some(m.complete(p, EXPR_TUPLE))
            } else {
                let mut is_tuple = false;
                let _ = expr(p);

                while p.eat(COMMA) {
                    is_tuple = true;

                    if p.at(R_PAREN) {
                        break;
                    } else {
                        expr(p);
                    }
                }

                p.expect(R_PAREN);

                if is_tuple {
                    Some(m.complete(p, EXPR_TUPLE))
                } else {
                    Some(m.complete(p, EXPR_PARENS))
                }
            }
        },
        | _ => {
            p.error("expred an expression");
            p.bump_any();
            m.abandon(p);
            None
        },
    }
}

fn peek(p: &Parser, allow_do: bool) -> bool {
    match p.current() {
        | DO_KW => allow_do,
        | IDENT | INT | FLOAT | CHAR | STRING | RSTRING | L_PAREN | L_BRACE | L_BRACKET | IF_KW | THEN_KW | UNLESS_KW | ELSE_KW | WHILE_KW | LOOP_KW
        | UNTIL_KW | NEXT_KW | BREAK_KW | YIELD_KW | RETURN_KW | CASE_KW | OF_KW | UNDERSCORE => true,
        | _ => false,
    }
}

crate fn literal(p: &mut Parser) {
    let m = p.start();

    if p.eat(INT) {
        m.complete(p, LIT_INT);
    } else if p.eat(FLOAT) {
        m.complete(p, LIT_FLOAT);
    } else if p.eat(CHAR) {
        m.complete(p, LIT_CHAR);
    } else if p.eat(STRING) || p.eat(RSTRING) {
        m.complete(p, LIT_STRING);
    } else {
        p.error("expected a literal");
        m.abandon(p);
    }
}

crate fn block(p: &mut Parser) {
    let m = p.start();

    p.expect(LYT_START);

    while !p.at(EOF) && !p.at(LYT_END) {
        expr(p);

        if !p.at(LYT_END) {
            p.expect(LYT_SEP);
        }
    }

    p.expect(LYT_END);
    m.complete(p, BLOCK);
}
