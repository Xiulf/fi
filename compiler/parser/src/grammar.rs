mod attributes;
mod exprs;
mod items;
mod paths;
mod patterns;
mod types;

use crate::parser::Parser;
use crate::syntax_kind::*;

crate fn root(p: &mut Parser) {
    items::module(p);
}

crate mod fragments {
    use super::*;

    crate fn attr(p: &mut Parser) {
        attributes::attr(p);
    }
}
