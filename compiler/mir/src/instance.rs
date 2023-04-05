use hir_def::id::ImplId;
use hir_ty::ty::Ty;

use crate::ir::{Body, MirValueId};

#[salsa::interned]
pub struct Instance {
    pub id: InstanceId,
    #[return_ref]
    pub subst: Option<Subst>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum InstanceId {
    MirValueId(MirValueId),
    VtableMethod(MirValueId, usize, usize),
    Body(Body),
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
    Param(usize),
}

impl Subst {
    pub fn is_empty(&self) -> bool {
        self.types.is_empty() && self.impls.is_empty()
    }
}

ra_ap_stdx::impl_from!(MirValueId, Body for InstanceId);
