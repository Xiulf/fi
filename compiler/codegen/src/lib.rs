pub mod ctx;
pub mod target;

pub trait Db: mir::Db + salsa::DbWithJar<Jar> {
    fn target(&self) -> &target::Target;
}

#[salsa::jar(db = Db)]
pub struct Jar(codegen_lib);

#[salsa::tracked]
pub fn codegen_lib(db: &dyn Db, _lib: hir::id::LibId) {
    ctx::with_codegen_ctx(db, |_ctx| {
        tracing::debug!("codegen!");
    });
}
