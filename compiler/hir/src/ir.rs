pub use codespan::Span;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LibId(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct DefIndex(u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct DefId {
    pub lib: LibId,
    pub index: DefIndex,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct HirId {
    pub owner: DefId,
    pub local_id: LocalId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LocalId(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BodyId(pub HirId);

pub struct Body {
    pub params: Vec<Param>,
    pub value: Expr,
}

pub struct Param {
    pub id: HirId,
}

pub struct Expr {
    pub id: HirId,
    pub span: Span,
    pub kind: ExprKind,
}

pub enum ExprKind {}

impl LibId {
    pub const LOCAL: Self = LibId(0);
}

impl DefId {
    pub fn new(lib: LibId, index: DefIndex) -> Self {
        DefId { lib, index }
    }

    pub fn local(index: DefIndex) -> Self {
        DefId::new(LibId::LOCAL, index)
    }
}

impl DefIndex {
    pub fn from_path(module: &str, name: &str) -> Self {
        let id = source::hash::hash(&(module, name));

        DefIndex(id)
    }
}
