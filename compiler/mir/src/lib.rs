pub mod syntax;

pub trait Db: hir_ty::Db + salsa::DbWithJar<Jar> {}

#[salsa::jar(db = Db)]
pub struct Jar(syntax::ValueDef, syntax::Body);
