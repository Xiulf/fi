use super::*;
use crate::parser::{Marker, Parser};
use crate::syntax_kind::*;

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
        | DEF_KW => {
            def(p, m);
        },
        | STATIC_KW => {
            static_(p, m);
        },
        | CONST_KW => {
            const_(p, m);
        },
        | TYPE_KW => {
            type_(p, m);
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
    p.bump(IMPORT_KW);
    paths::import_path(p);

    if p.eat(SLASH) {
        let items = p.start();

        match p.current() {
            | STAR => {
                p.bump(STAR);
                items.complete(p, IMPORT_ITEMS);
            },
            | SYMBOL | IDENT => {
                paths::name_or_symbol_ref(p);

                while p.eat(COMMA) {
                    paths::name_or_symbol_ref(p);
                }

                items.complete(p, IMPORT_ITEMS);
            },
            | _ => {
                p.error("expected '*', an identifier or a symbol");
                items.abandon(p);
            },
        }
    }

    m.complete(p, IMPORT);
}

crate fn fixity(p: &mut Parser, m: Marker) {
    if p.at(INFIX_KW) || p.at(INFIXL_KW) || p.at(INFIXR_KW) {
        p.bump_any();
        p.expect(INT);
        paths::name_ref(p);
        p.expect(AS_KW);
        paths::symbol(p);
        m.complete(p, ITEM_FIXITY);
    } else {
        p.error("expected 'infix', 'infixl' or 'infixr'");
        m.abandon(p);
    }
}

crate fn foreign(p: &mut Parser, m: Marker) {
    if p.eat(FOREIGN_KW) {
        match p.current() {
            | DEF_KW | STATIC_KW => p.bump_any(),
            | _ => {
                p.error("expected 'def' or 'static'");
                m.abandon(p);
                return;
            },
        }

        paths::name(p);
        p.expect(DBL_COLON);
        types::func(p);
        m.complete(p, ITEM_FOREIGN);
    } else {
        p.error("expected 'foreign'");
        m.abandon(p);
    }
}

crate fn def(p: &mut Parser, m: Marker) {
    if p.eat(DEF_KW) {
        paths::name(p);

        while !p.at(EOF) && !p.at(EQUALS) {
            patterns::atom(p);
        }

        p.expect(EQUALS);
        exprs::block(p);
        m.complete(p, ITEM_DEF);
    } else {
        p.error("expected 'def'");
        m.abandon(p);
    }
}

crate fn static_(p: &mut Parser, m: Marker) {
    if p.eat(STATIC_KW) {
        paths::name(p);
        p.expect(EQUALS);
        exprs::expr(p);
        m.complete(p, ITEM_STATIC);
    } else {
        p.error("expected 'static'");
        m.abandon(p);
    }
}

crate fn const_(p: &mut Parser, m: Marker) {
    if p.eat(CONST_KW) {
        paths::name(p);
        p.expect(EQUALS);
        exprs::expr(p);
        m.complete(p, ITEM_CONST);
    } else {
        p.error("expected 'const'");
        m.abandon(p);
    }
}

crate fn type_(p: &mut Parser, m: Marker) {
    if p.eat(TYPE_KW) {
        paths::name(p);

        while p.at(L_PAREN) || p.at(IDENT) {
            types::type_var(p);
        }

        if p.eat(DBL_COLON) {
            types::func(p);
        }

        if p.eat(EQUALS) {
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
    } else {
        p.error("expected 'type'");
        m.abandon(p);
    }
}

crate fn ctor(p: &mut Parser, m: Marker) {
    paths::name(p);

    while types::peek(p) {
        types::atom(p);
    }

    m.complete(p, ITEM_CTOR);
}

crate fn class(p: &mut Parser, m: Marker) {
    if p.eat(CLASS_KW) {
        paths::name(p);

        while p.at(L_PAREN) || p.at(IDENT) {
            types::type_var(p);
        }

        if p.eat(EQUALS) {
            p.expect(LYT_START);

            while !p.at(EOF) && !p.at(LYT_END) {
                class_item(p);

                if !p.at(LYT_END) {
                    p.expect(LYT_SEP);
                }
            }

            p.expect(LYT_END);
        }

        m.complete(p, ITEM_CLASS);
    } else {
        p.error("expected 'class'");
        m.abandon(p);
    }
}

crate fn instance(p: &mut Parser, m: Marker) {
    if p.eat(INSTANCE_KW) {
        paths::name_ref(p);

        while types::peek(p) {
            types::atom(p);
        }

        if p.eat(EQUALS) {
            p.expect(LYT_START);

            while !p.at(EOF) && !p.at(LYT_END) {
                instance_item(p);

                if !p.at(LYT_END) {
                    p.expect(LYT_SEP);
                }
            }

            p.expect(LYT_END);
        }

        m.complete(p, ITEM_INSTANCE);
    } else {
        p.error("expected 'instance'");
        m.abandon(p);
    }
}

crate fn class_item(p: &mut Parser) {
    let m = p.start();

    while p.at(AT) {
        attributes::attr(p);
    }

    match p.current() {
        | DEF_KW => class_def(p, m),
        | STATIC_KW => class_static(p, m),
        | _ => {
            p.error("expected a class item");
            m.abandon(p);
        },
    }
}

crate fn class_def(p: &mut Parser, m: Marker) {
    if p.eat(DEF_KW) {
        paths::name(p);
        p.expect(DBL_COLON);
        types::func(p);
        m.complete(p, CLASS_DEF);
    } else {
        p.error("expected 'def'");
        m.abandon(p);
    }
}

crate fn class_static(p: &mut Parser, m: Marker) {
    if p.eat(STATIC_KW) {
        paths::name(p);
        p.expect(DBL_COLON);
        types::func(p);
        m.complete(p, CLASS_STATIC);
    } else {
        p.error("expected 'static'");
        m.abandon(p);
    }
}

crate fn instance_item(p: &mut Parser) {
    let m = p.start();

    while p.at(AT) {
        attributes::attr(p);
    }

    match p.current() {
        | DEF_KW => def(p, m),
        | STATIC_KW => static_(p, m),
        | _ => {
            p.error("expected an instance item");
            m.abandon(p);
        },
    }
}
