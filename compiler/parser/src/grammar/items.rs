use super::*;
use crate::parser::{Marker, Parser};
use crate::syntax_kind::*;
use crate::token_set::TokenSet;

pub(crate) fn any_item(p: &mut Parser) {
    let m = p.start();

    while p.at(AT) {
        attributes::attr(p);
        p.eat(LYT_SEP);
    }

    match p.current() {
        | MODULE_KW => {
            module(p, m);
        },
        | IMPORT_KW => {
            import(p, m);
        },
        | INFIX_KW | INFIXL_KW | INFIXR_KW | POSTFIX_KW | PREFIX_KW => {
            fixity(p, m);
        },
        | FOREIGN_KW => {
            foreign(p, m);
        },
        | IDENT => {
            funcs(p, m, true);
        },
        | STATIC_KW => {
            statics(p, m, true);
        },
        | CONST_KW => {
            consts(p, m, true);
        },
        | TYPE_KW => {
            types(p, m, true);
        },
        | CLASS_KW => {
            class(p, m);
        },
        | DERIVE_KW | MEMBER_KW => {
            member(p, m);
        },
        | _ => {
            m.abandon(p);
            p.error("expected an item");

            while !p.at_ts(TokenSet::new(&[EOF, LYT_SEP, LYT_END])) {
                p.bump_any();
            }
        },
    }
}

pub(crate) fn module(p: &mut Parser, m: Marker) {
    p.expect(MODULE_KW);
    paths::module_name(p);

    if p.at(L_PAREN) {
        exports(p);
    }

    p.expect(EQUALS);
    p.expect(LYT_START);
    p.eat(LYT_SEP);

    while !p.at(EOF) && !p.at(LYT_END) {
        items::any_item(p);

        if !p.at(LYT_END) && !p.at(EOF) {
            p.expect(LYT_SEP);
        }
    }

    p.expect(LYT_END);
    m.complete(p, MODULE);
}

pub(crate) fn import(p: &mut Parser, m: Marker) {
    p.expect(IMPORT_KW);
    paths::module_name_ref(p);

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

pub(crate) fn fixity(p: &mut Parser, m: Marker) {
    if p.at(INFIX_KW) || p.at(INFIXL_KW) || p.at(INFIXR_KW) {
        p.bump_any();
        p.expect(INT);
        paths::path(p);
        p.expect(AS_KW);
        paths::symbol(p);
        m.complete(p, ITEM_FIXITY);
    } else if p.eat(POSTFIX_KW) {
        paths::path(p);
        p.expect(AS_KW);
        paths::symbol(p);
        m.complete(p, ITEM_FIXITY);
    } else if p.eat(PREFIX_KW) {
        paths::path(p);
        p.expect(AS_KW);
        paths::symbol(p);
        m.complete(p, ITEM_FIXITY);
    } else {
        p.error("expected 'infix', 'infixl', 'infixr', 'postfix' or 'prefix'");
        m.abandon(p);
    }
}

pub(crate) fn foreign(p: &mut Parser, m: Marker) {
    p.expect(FOREIGN_KW);

    match p.current() {
        | IDENT => funcs(p, m, false),
        | STATIC_KW => statics(p, m, false),
        | TYPE_KW => types(p, m, false),
        | _ => {
            p.error("expected 'fn', 'static' or 'type'");
            m.abandon(p);
            return;
        },
    }
}

pub(crate) fn funcs(p: &mut Parser, m: Marker, multiple: bool) {
    let name = p.current_text().to_string();
    let f = p.start();

    func(p, f);

    if multiple {
        while p.at(LYT_SEP) && p.nth_at(1, IDENT) && !p.nth_at(2, DBL_COLON) && p.nth_text(1) == name {
            p.bump(LYT_SEP);
            let f = p.start();

            func(p, f);
        }
    }

    m.complete(p, ITEM_FUNC);
}

pub(crate) fn func(p: &mut Parser, m: Marker) {
    paths::name(p);

    if p.eat(DBL_COLON) {
        types::ty(p);
        m.complete(p, ONE_FUNC);
    } else {
        while !p.at_ts(TokenSet::new(&[EOF, LYT_SEP, LYT_END, EQUALS, IF_KW])) {
            patterns::atom(p);
        }

        if p.eat(EQUALS) {
            let body = p.start();

            exprs::block(p, false);
            body.complete(p, EXPR_DO);
        } else if p.at(IF_KW) {
            let guarded = p.start();

            exprs::case_guard(p, EQUALS);

            while p.at_ts(IF_KW | ELSE_KW) {
                exprs::case_guard(p, EQUALS);
            }

            guarded.complete(p, CASE_GUARDED);
        }

        m.complete(p, ONE_FUNC);
    }
}

pub(crate) fn statics(p: &mut Parser, m: Marker, multiple: bool) {
    let name = p.nth_text(1).to_string();
    let f = p.start();

    static_(p, f);

    if multiple {
        while p.at(LYT_SEP)
            && p.nth_at(1, STATIC_KW)
            && p.nth_at(2, IDENT)
            && !p.nth_at(3, DBL_COLON)
            && p.nth_text(2) == name
        {
            p.bump(LYT_SEP);
            let f = p.start();

            static_(p, f);
        }
    }

    m.complete(p, ITEM_STATIC);
}

pub(crate) fn static_(p: &mut Parser, m: Marker) {
    p.expect(STATIC_KW);
    paths::name(p);

    if p.eat(DBL_COLON) {
        types::ty(p);
        m.complete(p, ONE_STATIC);
    } else {
        p.expect(EQUALS);
        exprs::expr(p);
        m.complete(p, ONE_STATIC);
    }
}

pub(crate) fn consts(p: &mut Parser, m: Marker, multiple: bool) {
    let name = p.nth_text(1).to_string();
    let f = p.start();

    const_(p, f);

    if multiple {
        while p.at(LYT_SEP)
            && p.nth_at(1, CONST_KW)
            && p.nth_at(2, IDENT)
            && !p.nth_at(3, DBL_COLON)
            && p.nth_text(2) == name
        {
            p.bump(LYT_SEP);
            let f = p.start();

            const_(p, f);
        }
    }

    m.complete(p, ITEM_CONST);
}

pub(crate) fn const_(p: &mut Parser, m: Marker) {
    p.expect(CONST_KW);
    paths::name(p);

    if p.eat(DBL_COLON) {
        types::ty(p);
        m.complete(p, ONE_CONST);
    } else {
        p.expect(EQUALS);
        exprs::expr(p);
        m.complete(p, ONE_CONST);
    }
}

pub(crate) fn types(p: &mut Parser, m: Marker, multiple: bool) {
    let name = p.nth_text(1).to_string();
    let f = p.start();

    type_(p, f);

    if multiple {
        while p.at(LYT_SEP)
            && p.nth_at(1, TYPE_KW)
            && p.nth_at(2, IDENT)
            && !p.nth_at(3, DBL_COLON)
            && p.nth_text(2) == name
        {
            p.bump(LYT_SEP);
            let f = p.start();

            type_(p, f);
        }
    }

    m.complete(p, ITEM_TYPE);
}

pub(crate) fn type_(p: &mut Parser, m: Marker) {
    p.expect(TYPE_KW);
    paths::name(p);

    if p.eat(DBL_COLON) {
        types::ty(p);
    } else {
        type_vars(p);
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

    m.complete(p, ONE_TYPE);
}

pub(crate) fn ctor(p: &mut Parser, m: Marker) {
    paths::name(p);

    while types::peek(p) {
        types::atom(p);
    }

    m.complete(p, ITEM_CTOR);
}

pub(crate) fn class(p: &mut Parser, m: Marker) {
    p.expect(CLASS_KW);
    paths::name(p);
    type_vars(p);

    if p.eat(PIPE) {
        fun_dep(p);

        while p.eat(COMMA) {
            fun_dep(p);
        }
    }

    if p.at(WHERE_KW) {
        where_clause(p);
    }

    if p.eat(EQUALS) {
        p.expect(LYT_START);
        p.eat(LYT_SEP);

        while !p.at_ts(TokenSet::new(&[EOF, LYT_END])) {
            assoc_item(p);

            if !p.at(LYT_END) {
                p.expect(LYT_SEP);
            }
        }

        p.expect(LYT_END);
    }

    m.complete(p, ITEM_CLASS);
}

pub(crate) fn where_clause(p: &mut Parser) {
    let m = p.start();

    p.expect(WHERE_KW);
    p.expect(LYT_START);
    where_clause_item(p);

    while p.eat(COMMA) || p.eat(LYT_SEP) {
        where_clause_item(p);
    }

    p.expect(LYT_END);
    m.complete(p, WHERE_CLAUSE);
}

pub(crate) fn where_clause_item(p: &mut Parser) {
    let m = p.start();

    if p.at(IDENT) && p.nth_at(1, DBL_COLON) {
        let t = p.start();
        paths::path(p);
        t.complete(p, TYPE_PATH);
        p.bump(DBL_COLON);
        types::infix(p, COMMA);
        m.complete(p, TYPE_VAR_KIND);
    } else {
        paths::path(p);

        while types::peek(p) {
            types::atom(p);
        }

        m.complete(p, CONSTRAINT);
    }
}

pub(crate) fn type_vars(p: &mut Parser) {
    let m = p.start();

    while p.at(IDENT) {
        paths::name(p);
    }

    m.complete(p, TYPE_VARS);
}

pub(crate) fn fun_dep(p: &mut Parser) {
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

pub(crate) fn member(p: &mut Parser, m: Marker) {
    p.eat(DERIVE_KW);
    p.expect(MEMBER_KW);
    types::atom(p);

    while !p.at(OF_KW) {
        types::atom(p);
    }

    p.expect(OF_KW);
    paths::path(p);

    if p.at(WHERE_KW) {
        where_clause(p);
    }

    if p.eat(EQUALS) {
        p.expect(LYT_START);
        p.eat(LYT_SEP);

        while !p.at(EOF) && !p.at(LYT_END) {
            assoc_item(p);

            if !p.at(LYT_END) {
                p.expect(LYT_SEP);
            }
        }

        p.expect(LYT_END);
    }

    m.complete(p, ITEM_MEMBER);
}

pub(crate) fn assoc_item(p: &mut Parser) {
    let m = p.start();

    while p.at(AT) {
        attributes::attr(p);
        p.eat(LYT_SEP);
    }

    match p.current() {
        // | FN_KW => fun(p, m),
        | IDENT => funcs(p, m, true),
        | STATIC_KW => statics(p, m, true),
        | _ => {
            p.error("expected an associated item");
            m.abandon(p);

            while !p.at_ts(TokenSet::new(&[EOF, LYT_SEP, LYT_END])) {
                p.bump_any();
            }
        },
    }
}
