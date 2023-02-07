#![feature(trait_upcasting)]

pub mod ctx;
pub mod ty;

pub trait Db: hir_def::Db + salsa::DbWithJar<Jar> {
    fn type_cache(&self) -> &ctx::Cache;
}

#[salsa::jar(db = Db)]
pub struct Jar(ty::Ty);
