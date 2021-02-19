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

    while !p.at(EOF) && p.at(AT) {
        attributes::attr(p);
        p.eat(LYT_SEP);
    }

    p.eat(LYT_SEP);
    items::module(p, m, true);
}

crate mod fragments {
    use super::*;

    crate fn attr(p: &mut Parser) {
        attributes::attr(p);
    }
}
