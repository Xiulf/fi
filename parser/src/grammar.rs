mod attributes;
mod exprs;
mod items;
mod paths;
mod patterns;
mod types;

use crate::parser::Parser;
use crate::syntax_kind::*;

crate fn root(p: &mut Parser) {
    let m = p.start();

    p.eat(LYT_SEP);

    while !p.at(EOF) {
        items::any_item(p);

        if !p.at(EOF) {
            if !p.eat(LYT_SEP) {
                p.error("expected a newline");
            }
        }
    }

    p.expect(EOF);
    m.complete(p, SOURCE_FILE);
}

crate mod fragments {
    use super::*;

    crate fn attr(p: &mut Parser) {
        attributes::attr(p);
    }
}
