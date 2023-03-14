use hir_def::id::ImplId;
use hir_ty::ty::Ty;

use crate::ir::MirValueId;

#[salsa::interned]
pub struct Instance {
    pub value: MirValueId,
    #[return_ref]
    pub subst: Option<Subst>,
}

#[salsa::interned]
pub struct ImplInstance {
    pub value: ImplId,
    #[return_ref]
    pub subst: Option<Subst>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Subst {
    pub types: Vec<Ty>,
    pub impls: Vec<ImplInstance>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ImplSource {
    Instance(ImplInstance),
}
