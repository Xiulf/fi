pub use diagnostics::Span;
use std::collections::BTreeMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Id(pub(crate) u64);

pub struct Package {
    pub items: BTreeMap<Id, Item>,
    pub exprs: BTreeMap<Id, Expr>,
    pub types: BTreeMap<Id, Type>,
}

pub struct Item {
    pub span: Span,
    pub id: Id,
}

pub struct Expr {
    pub span: Span,
    pub id: Id,
}

pub struct Type {
    pub span: Span,
    pub id: Id,
}
