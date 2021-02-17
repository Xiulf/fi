use crate::parser::Parser;
use crate::syntax_kind::*;

crate fn path(p: &mut Parser) {
    let path = p.start();

    path_segment(p);

    let mut qual = path.complete(p, PATH);

    loop {
        let import_seg = matches!(p.nth(1), STAR | SYMBOL);

        if p.at(SLASH) && !import_seg {
            let path = qual.precede(p);

            p.bump(SLASH);
            path_segment(p);

            qual = path.complete(p, PATH);
        } else {
            break;
        }
    }
}

crate fn import_path(p: &mut Parser) {
    let path = p.start();

    path_segment(p);

    while p.at(SLASH) && p.nth_at(1, IDENT) && p.nth_at(2, SLASH) {
        p.bump(SLASH);
        path_segment(p);
    }

    path.complete(p, PATH);
}

fn path_segment(p: &mut Parser) {
    let m = p.start();

    match p.current() {
        | IDENT => {
            name_ref(p);
        },
        | _ => {
            p.error("expected an identifier");
        },
    }

    m.complete(p, PATH_SEGMENT);
}

crate fn name(p: &mut Parser) {
    if p.at(IDENT) {
        let m = p.start();

        p.bump(IDENT);
        m.complete(p, NAME);
    } else {
        p.error("expected a name");
        p.bump_any();
    }
}

crate fn symbol(p: &mut Parser) {
    if p.at(SYMBOL) {
        let m = p.start();

        p.bump(SYMBOL);
        m.complete(p, NAME);
    } else {
        p.error("expected a name");
        p.bump_any();
    }
}

crate fn name_ref(p: &mut Parser) {
    if p.at(IDENT) {
        let m = p.start();

        p.bump(IDENT);
        m.complete(p, NAME_REF);
    } else {
        p.error("expected an identifier");
        p.bump_any();
    }
}

crate fn name_or_symbol_ref(p: &mut Parser) {
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
