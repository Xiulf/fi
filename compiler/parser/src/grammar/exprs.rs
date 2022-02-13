use super::*;
use crate::parser::{CompletedMarker, Parser};
use crate::syntax_kind::*;

crate fn expr(p: &mut Parser) {
    expr_(p, true);
}

fn expr_(p: &mut Parser, allow_do: bool) {
    if let Some(m) = assign(p, allow_do) {
        if p.at(DBL_COLON) {
            let expr = m.precede(p);
            let _ = p.bump(DBL_COLON);
            let _ = types::ty(p);

            expr.complete(p, EXPR_TYPED);
        }
    }
}

crate fn assign(p: &mut Parser, allow_do: bool) -> Option<CompletedMarker> {
    let mut m = infix(p, allow_do)?;

    if p.eat(EQUALS) {
        let expr = m.precede(p);
        let _ = expr_(p, allow_do);

        m = expr.complete(p, EXPR_ASSIGN);
    }

    Some(m)
}

crate fn infix(p: &mut Parser, allow_do: bool) -> Option<CompletedMarker> {
    let mut m = app(p, allow_do)?;

    if p.at(TICK) {
        let expr = m.precede(p);

        paths::path(p);
        p.expect(TICK);
        infix(p, allow_do);

        m = expr.complete(p, EXPR_INFIX);
    } else if p.at(OPERATOR) || p.at(STAR) {
        let expr = m.precede(p);

        while p.at(OPERATOR) || p.at(STAR) {
            p.bump_any();
            app(p, allow_do);
        }

        m = expr.complete(p, EXPR_INFIX);
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
                    | IDENT => {
                        paths::name_ref(p);
                        m = expr.complete(p, EXPR_FIELD);
                    },
                    | INT => {
                        p.bump(INT);
                        m = expr.complete(p, EXPR_FIELD);
                    },
                    | _ => {
                        p.error("expected an identifier");
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
        | UNDERSCORE => {
            p.bump(UNDERSCORE);
            Some(m.complete(p, EXPR_HOLE))
        },
        | IDENT | SYMBOL => {
            paths::path(p);
            Some(m.complete(p, EXPR_PATH))
        },
        | INT | FLOAT | CHAR | STRING => {
            literal(p);
            Some(m.complete(p, EXPR_LITERAL))
        },
        | DO_KW if allow_do => {
            p.bump(DO_KW);

            if p.eat(PIPE) {
                while !p.at(EOF) && !p.at(PIPE) {
                    patterns::atom(p);

                    if !p.at(PIPE) {
                        p.expect(COMMA);
                    }
                }

                p.expect(PIPE);
                block(p);
                Some(m.complete(p, EXPR_CLOS))
            } else {
                block(p);
                Some(m.complete(p, EXPR_DO))
            }
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
                    p.expect(ELSE_KW);
                    expr(p);
                },
                | DO_KW => {
                    let do_expr = p.start();

                    p.bump(DO_KW);
                    block(p);
                    do_expr.complete(p, EXPR_DO);

                    if p.eat(ELSE_KW) {
                        expr(p);
                    }
                },
                | c => {
                    dbg!(c);
                    p.error("expected 'then' or 'do'");
                    m.abandon(p);
                    return None;
                },
            }

            Some(m.complete(p, EXPR_IF))
        },
        | CASE_KW => {
            p.bump(CASE_KW);
            expr(p);
            p.expect(OF_KW);
            p.expect(LYT_START);

            while !p.at(EOF) && !p.at(LYT_END) {
                case_arm(p);

                if !p.at(LYT_END) {
                    p.expect(LYT_SEP);
                }
            }

            p.expect(LYT_END);

            Some(m.complete(p, EXPR_CASE))
        },
        | WHILE_KW | UNTIL_KW => {
            p.bump_any();
            expr_(p, false);

            let body = p.start();

            p.expect(DO_KW);
            block(p);
            body.complete(p, EXPR_DO);
            Some(m.complete(p, EXPR_WHILE))
        },
        | LOOP_KW => {
            p.bump(LOOP_KW);

            let body = p.start();

            block(p);
            body.complete(p, EXPR_DO);
            Some(m.complete(p, EXPR_LOOP))
        },
        | NEXT_KW => {
            p.bump(NEXT_KW);

            if peek(p, true) {
                expr(p);
            }

            Some(m.complete(p, EXPR_NEXT))
        },
        | BREAK_KW => {
            p.bump(BREAK_KW);

            if peek(p, true) {
                expr(p);
            }

            Some(m.complete(p, EXPR_BREAK))
        },
        | YIELD_KW => {
            p.bump(YIELD_KW);

            while peek(p, true) {
                atom(p, true);
            }

            Some(m.complete(p, EXPR_YIELD))
        },
        | RETURN_KW => {
            p.bump(RETURN_KW);

            if peek(p, true) {
                expr(p);
            }

            Some(m.complete(p, EXPR_RETURN))
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
        | L_BRACKET => {
            p.bump(L_BRACKET);

            while !p.at(EOF) && !p.at(R_BRACKET) {
                expr(p);

                if !p.at(R_BRACKET) {
                    p.expect(COMMA);
                }
            }

            p.expect(R_BRACKET);
            Some(m.complete(p, EXPR_ARRAY))
        },
        | L_BRACE => {
            p.bump(L_BRACE);
            patterns::record_fields(p, expr, false);
            p.expect(R_BRACE);
            Some(m.complete(p, EXPR_RECORD))
        },
        | _ => {
            p.error("expected an expression");
            p.bump_any();
            m.abandon(p);
            None
        },
    }
}

fn peek(p: &Parser, allow_do: bool) -> bool {
    match p.current() {
        | DO_KW => allow_do,
        | IDENT | SYMBOL | INT | FLOAT | CHAR | STRING | L_PAREN | L_BRACE | L_BRACKET | IF_KW | UNLESS_KW
        | WHILE_KW | LOOP_KW | UNTIL_KW | NEXT_KW | BREAK_KW | YIELD_KW | RETURN_KW | CASE_KW | UNDERSCORE => true,
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
    } else if p.eat(STRING) {
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
        p.eat(LYT_SEP);
        stmt(p);

        if !p.at(LYT_END) {
            p.expect(LYT_SEP);
        }
    }

    p.expect(LYT_END);
    m.complete(p, BLOCK);
}

crate fn stmt(p: &mut Parser) {
    let m = p.start();

    if p.eat(LET_KW) {
        patterns::pattern(p);
        p.expect(EQUALS);
        expr(p);
        m.complete(p, STMT_LET);
    } else {
        for i in 0..100 {
            if p.nth_at(i, LEFT_ARROW) {
                patterns::app(p);
                p.expect(LEFT_ARROW);
                expr(p);
                m.complete(p, STMT_BIND);
                return;
            } else if p.nth_at(i, LYT_START) || p.nth_at(i, LYT_SEP) || p.nth_at(i, LYT_END) {
                break;
            }
        }

        expr(p);
        m.complete(p, STMT_EXPR);
    }
}

crate fn case_arm(p: &mut Parser) {
    let m = p.start();

    patterns::pattern(p);

    if p.eat(PIPE) {
        let guard = p.start();

        expr(p);
        guard.complete(p, CASE_GUARD);
    }

    p.expect(ARROW);
    expr(p);
    m.complete(p, CASE_ARM);
}
