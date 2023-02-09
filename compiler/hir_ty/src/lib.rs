#![feature(trait_upcasting)]

use hir_def::id::ValueId;
use triomphe::Arc;

pub mod ctx;
pub mod expr;
pub mod ty;
pub mod unify;

pub trait Db: hir_def::Db + salsa::DbWithJar<Jar> {
    fn type_cache(&self) -> &ctx::Cache;
}

#[salsa::jar(db = Db)]
pub struct Jar(ty::Ty, infer);

#[salsa::tracked]
pub fn infer(db: &dyn Db, value: ValueId) -> Arc<ctx::InferResult> {
    use salsa::debug::DebugWithDb;
    let body = hir_def::body::query(db, value);
    let mut ctx = ctx::Ctx::new(db, value.into());
    let mut bcx = ctx.with_body(body.clone());
    let ty = bcx.infer_expr(body.body_expr(), ctx::Expectation::None);

    tracing::debug!("{:?}", ty.debug(db));
    ctx.finish()
}
