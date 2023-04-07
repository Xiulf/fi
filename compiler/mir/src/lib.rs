#![feature(trait_upcasting)]

pub mod builder;
pub mod display;
pub mod instance;
pub mod ir;
pub mod lower;
pub mod repr;
pub mod visitor;

pub trait Db: hir::Db + salsa::DbWithJar<Jar> {}

impl<T: hir::Db + salsa::DbWithJar<Jar>> Db for T {
}

#[salsa::jar(db = Db)]
pub struct Jar(
    instance::Instance,
    instance::ImplInstance,
    repr::repr_of,
    ir::ValueDef,
    ir::Body,
    lower::value_mir,
    lower::ctor_mir,
);
