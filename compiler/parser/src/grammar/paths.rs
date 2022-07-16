use crate::parser::Parser;
use crate::syntax_kind::*;

pub(crate) fn path(p: &mut Parser) {
    let path = p.start();

    path_segment(p);

    while p.at(PATH_SEP) {
        if p.nth_at(1, IDENT) {
            p.bump(PATH_SEP);
            path_segment(p);
        } else if p.nth_at(1, SYMBOL) {
            p.bump(PATH_SEP);
            path_segment(p);
            break;
        } else {
            break;
        }
    }

    path.complete(p, PATH);
}

pub(crate) fn module_name(p: &mut Parser) {
    let name = p.start();

    p.expect(IDENT);

    while p.at(PATH_SEP) && p.nth_at(1, IDENT) {
        p.bump(PATH_SEP);
        p.bump(IDENT);
    }

    name.complete(p, NAME);
}

pub(crate) fn module_name_ref(p: &mut Parser) {
    let name = p.start();

    p.expect(IDENT);

    while p.at(PATH_SEP) && p.nth_at(1, IDENT) {
        p.bump(PATH_SEP);
        p.bump(IDENT);
    }

    name.complete(p, NAME_REF);
}

pub(crate) fn path_segment(p: &mut Parser) {
    let m = p.start();

    match p.current() {
        | IDENT | SYMBOL => {
            name_or_symbol_ref(p);
        },
        | _ => {
            p.error("expected an identifier or a symbol");
        },
    }

    m.complete(p, PATH_SEGMENT);
}

pub(crate) fn name(p: &mut Parser) {
    if p.at(IDENT) {
        let m = p.start();

        p.bump(IDENT);
        m.complete(p, NAME);
    } else {
        p.error("expected a name");
        p.bump_any();
    }
}

pub(crate) fn symbol(p: &mut Parser) {
    if p.at(SYMBOL) {
        let m = p.start();

        p.bump(SYMBOL);
        m.complete(p, NAME);
    } else {
        p.error("expected a name");
        p.bump_any();
    }
}

pub(crate) fn name_ref(p: &mut Parser) {
    if p.at(IDENT) {
        let m = p.start();

        p.bump(IDENT);
        m.complete(p, NAME_REF);
    } else {
        p.error("expected an identifier");
        p.bump_any();
    }
}

pub(crate) fn name_or_symbol_ref(p: &mut Parser) {
    if p.at(IDENT) {
        let m = p.start();

        p.bump(IDENT);
        m.complete(p, NAME_REF);
    } else if p.at(SYMBOL) {
        let m = p.start();

        p.bump(SYMBOL);
        m.complete(p, NAME_REF);
    } else {
        p.error("expected an identifier or a symbol");
        p.bump_any();
    }
}
