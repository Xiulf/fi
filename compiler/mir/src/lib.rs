#![feature(trait_upcasting)]

pub mod builder;
pub mod instance;
pub mod ir;
pub mod repr;

pub trait Db: hir_ty::Db + salsa::DbWithJar<Jar> {}

#[salsa::jar(db = Db)]
pub struct Jar(
    instance::Instance,
    instance::ImplInstance,
    repr::repr_of,
    ir::ValueDef,
    ir::Body,
);
