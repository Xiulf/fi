#![feature(crate_visibility_modifier)]

pub mod ctx;
pub mod error;
pub mod infer;
pub mod skolem;
pub mod subsume;
pub mod ty;
pub mod unify;

use hir::ir;
use std::collections::HashMap;
use std::sync::Arc;

#[salsa::query_group(TypeDatabaseStorage)]
pub trait TypeDatabase: hir::HirDatabase + InferDb {
    fn typecheck(&self, id: ir::DefId) -> Arc<TypecheckResult>;
}

pub trait InferDb {
    fn to_ty_db(&self) -> &dyn TypeDatabase;
}

#[derive(Debug, PartialEq, Eq)]
pub struct TypecheckResult {
    pub ty: ty::Ty,
    pub tys: HashMap<ir::HirId, ty::Ty>,
}

fn typecheck(db: &dyn TypeDatabase, id: ir::DefId) -> Arc<TypecheckResult> {
    unimplemented!();
}
