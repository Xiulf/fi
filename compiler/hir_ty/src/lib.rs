#![feature(trait_upcasting)]

use hir_def::expr::ExprId;
use hir_def::id::{TypedItemId, ValueId};
use hir_def::pat::PatId;
use syntax::TextRange;
use triomphe::Arc;
use vfs::InFile;

pub mod ctx;
pub mod diagnostics;
pub mod expr;
pub mod ty;
pub mod unify;

pub trait Db: hir_def::Db + salsa::DbWithJar<Jar> {
    fn type_cache(&self) -> &ctx::Cache;
}

#[salsa::jar(db = Db)]
pub struct Jar(ty::Ty, infer);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TyOrigin {
    ExprId(ExprId),
    PatId(PatId),
}

#[salsa::tracked]
pub fn infer(db: &dyn Db, value: ValueId) -> Arc<ctx::InferResult> {
    use salsa::debug::DebugWithDb;
    let body = hir_def::body::query(db, value).0;
    let mut ctx = ctx::Ctx::new(db, value.into());
    let mut bcx = ctx.with_body(body.clone());
    let ty = bcx.infer_expr(body.body_expr(), ctx::Expectation::None);

    tracing::debug!("{:?}", ty.debug(db));
    ctx.finish()
}

impl TyOrigin {
    pub fn to_text_range(self, db: &dyn Db, owner: TypedItemId) -> InFile<TextRange> {
        match self {
            | Self::ExprId(id) => {
                let value = owner.as_value_id().unwrap();
                let (_, src_map) = hir_def::body::query(db, value);
                let src = src_map.expr_src(id);

                InFile::new(src_map.file(), src.text_range())
            },
            | Self::PatId(id) => {
                let value = owner.as_value_id().unwrap();
                let (_, src_map) = hir_def::body::query(db, value);
                let src = src_map.pat_src(id);

                InFile::new(src_map.file(), src.text_range())
            },
        }
    }
}

ra_ap_stdx::impl_from!(ExprId, PatId for TyOrigin);
