use std::sync::Arc;

use base_db::Upcast;
use hir::db::HirDatabase;
use hir::id::DefWithBodyId;
use hir::ty::Ty;

use crate::repr::{Repr, Signature};
use crate::syntax::{Body, BodyData};

#[salsa::query_group(MirDatabaseStorage)]
pub trait MirDatabase: HirDatabase + Upcast<dyn HirDatabase> {
    #[salsa::interned]
    fn intern_body(&self, body: Arc<BodyData>) -> Body;

    #[salsa::invoke(crate::lower::lower_body)]
    fn body_mir(&self, def: DefWithBodyId) -> Body;

    #[salsa::invoke(crate::repr::repr_of_query)]
    #[salsa::cycle(crate::repr::repr_of_cycle)]
    fn repr_of(&self, ty: Ty) -> Repr;

    #[salsa::invoke(crate::repr::func_signature_query)]
    fn func_signature(&self, func: hir::Func) -> Signature;
}
