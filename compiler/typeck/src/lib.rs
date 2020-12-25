#![feature(crate_visibility_modifier)]

pub mod ctx;
pub mod error;
pub mod skolem;
pub mod subsume;
pub mod ty;
pub mod unify;

#[salsa::query_group(TypeDatabaseStorage)]
pub trait TypeDatabase: hir::HirDatabase + InferDb {}

pub trait InferDb {
    fn to_ty_db(&self) -> &dyn TypeDatabase;
}
