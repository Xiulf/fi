pub use codespan::Span;
use data_structures::stable_hasher;
pub use source::LibId;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct DefIndex(u64, u64);

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

impl DefId {
    pub fn new(lib: LibId, index: DefIndex) -> Self {
        DefId { lib, index }
    }
}

impl DefIndex {
    pub fn from_path(module: &str, name: &str) -> Self {
        use stable_hasher::HashStable;
        let mut hasher = stable_hasher::StableHasher::new();
        (module, name).hash_stable(&mut (), &mut hasher);
        hasher.finish()
    }
}

impl stable_hasher::StableHasherResult for DefIndex {
    fn finish(hasher: stable_hasher::StableHasher) -> Self {
        let (_0, _1) = hasher.finalize();
        DefIndex(_0, _1)
    }
}
