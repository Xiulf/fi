use super::*;
use crate::parser::{Marker, Parser};
use crate::syntax_kind::*;
use crate::token_set::TokenSet;

crate fn any_item(p: &mut Parser) {
    let m = p.start();

    while p.at(AT) {
        attributes::attr(p);
        p.eat(LYT_SEP);
    }

    match p.current() {
        | IMPORT_KW => {
            import(p, m);
        },
        | INFIX_KW | INFIXL_KW | INFIXR_KW => {
            fixity(p, m);
        },
        | FOREIGN_KW => {
            foreign(p, m);
        },
        | FUN_KW => {
            fun(p, m);
        },
        | STATIC_KW => {
            static_(p, m);
        },
        | CONST_KW => {
            const_(p, m);
        },
        | TYPE_KW => {
            type_(p, m, false);
        },
        | CLASS_KW => {
            class(p, m);
        },
        | INSTANCE_KW => {
            instance(p, m);
        },
        | _ => {
            m.abandon(p);
            p.error("expected an item");
            p.bump_any();
        },
    }
}

crate fn import(p: &mut Parser, m: Marker) {
    p.expect(IMPORT_KW);
    paths::path(p);

    if p.eat(L_PAREN) {
        let items = p.start();

        while !p.at(EOF) && !p.at(R_PAREN) {
            paths::name_or_symbol_ref(p);

            if !p.at(R_PAREN) {
                p.expect(COMMA);
            }
        }

        p.expect(R_PAREN);
        items.complete(p, IMPORT_ITEMS);
    }

    if p.eat(AS_KW) {
        paths::name(p);
    }

    m.complete(p, IMPORT);
}

crate fn fixity(p: &mut Parser, m: Marker) {
    if p.at(INFIX_KW) || p.at(INFIXL_KW) || p.at(INFIXR_KW) {
        p.bump_any();
        p.expect(INT);
        paths::path(p);
        p.expect(AS_KW);
        paths::symbol(p);
        m.complete(p, ITEM_FIXITY);
    } else {
        p.error("expected 'infix', 'infixl' or 'infixr'");
        m.abandon(p);
    }
}

crate fn foreign(p: &mut Parser, m: Marker) {
    p.expect(FOREIGN_KW);

    match p.current() {
        | FUN_KW => fun(p, m),
        | STATIC_KW => static_(p, m),
        | TYPE_KW => type_(p, m, true),
        | _ => {
            p.error("expected 'fun', 'static' or 'type'");
            m.abandon(p);
            return;
        },
    }
}

crate fn fun(p: &mut Parser, m: Marker) {
    p.expect(FUN_KW);
    paths::name(p);

    if p.eat(DBL_COLON) {
        types::func(p);
        m.complete(p, ITEM_FUN);
    } else {
        while !p.at_ts(TokenSet::new(&[EOF, LYT_SEP, LYT_END, EQUALS, PIPE])) {
            patterns::atom(p);
        }

        if p.eat(PIPE) {
            types::generics(p, EQUALS);
        }

        p.expect(EQUALS);
        exprs::expr(p);
        m.complete(p, ITEM_FUN);
    }
}

crate fn static_(p: &mut Parser, m: Marker) {
    p.expect(STATIC_KW);
    paths::name(p);

    if p.eat(DBL_COLON) {
        types::func(p);
        m.complete(p, ITEM_STATIC);
    } else {
        p.expect(EQUALS);
        exprs::expr(p);
        m.complete(p, ITEM_STATIC);
    }
}

crate fn const_(p: &mut Parser, m: Marker) {
    p.expect(CONST_KW);
    paths::name(p);

    if p.eat(DBL_COLON) {
        types::func(p);
        m.complete(p, ITEM_CONST);
    } else {
        p.expect(EQUALS);
        exprs::expr(p);
        m.complete(p, ITEM_CONST);
    }
}

crate fn type_(p: &mut Parser, m: Marker, foreign: bool) {
    p.expect(TYPE_KW);
    paths::name(p);

    if foreign && p.eat(DBL_COLON) {
        types::func(p);
    } else {
        while p.at(L_PAREN) || p.at(IDENT) {
            types::type_var(p);
        }

        p.expect(EQUALS);

        if p.at(PIPE) {
            while p.eat(PIPE) {
                let m = p.start();

                while p.at(AT) {
                    attributes::attr(p);
                }

                ctor(p, m);
            }
        } else {
            types::ty(p);
        }
    }

    m.complete(p, ITEM_TYPE);
}

crate fn ctor(p: &mut Parser, m: Marker) {
    paths::name(p);

    while types::peek(p) {
        types::atom(p);
    }

    m.complete(p, ITEM_CTOR);
}

crate fn class(p: &mut Parser, m: Marker) {
    p.expect(CLASS_KW);
    paths::name(p);

    while p.at(L_PAREN) || p.at(IDENT) {
        types::type_var(p);
    }

    if p.eat(PIPE) {
        fun_dep(p);

        while p.eat(COMMA) {
            fun_dep(p);
        }
    }

    if p.eat(COLON) {
        types::constraint(p);

        while p.eat(COMMA) {
            types::constraint(p);
        }
    }

    if p.eat(EQUALS) {
        p.expect(LYT_START);

        while !p.at(EOF) && !p.at(LYT_END) {
            assoc_item(p);

            if !p.at(LYT_END) {
                p.expect(LYT_SEP);
            }
        }

        p.expect(LYT_END);
    }

    m.complete(p, ITEM_CLASS);
}

crate fn fun_dep(p: &mut Parser) {
    let m = p.start();

    while p.at(IDENT) {
        paths::name_ref(p);
    }

    p.expect(ARROW);

    while p.at(IDENT) {
        paths::name_ref(p);
    }

    m.complete(p, FUN_DEP);
}

crate fn instance(p: &mut Parser, m: Marker) {
    p.expect(INSTANCE_KW);
    paths::path(p);

    while types::peek(p) {
        types::atom(p);
    }

    if p.eat(COLON) {
        types::constraint(p);

        while p.eat(COMMA) {
            types::constraint(p);
        }
    }

    if p.eat(EQUALS) {
        p.expect(LYT_START);

        while !p.at(EOF) && !p.at(LYT_END) {
            assoc_item(p);

            if !p.at(LYT_END) {
                p.expect(LYT_SEP);
            }
        }

        p.expect(LYT_END);
    }

    m.complete(p, ITEM_INSTANCE);
}

crate fn assoc_item(p: &mut Parser) {
    let m = p.start();

    while p.at(AT) {
        attributes::attr(p);
        p.eat(LYT_SEP);
    }

    match p.current() {
        | FUN_KW => fun(p, m),
        | STATIC_KW => static_(p, m),
        | _ => {
            p.error("expected an associated item");
            m.abandon(p);
        },
    }
}
