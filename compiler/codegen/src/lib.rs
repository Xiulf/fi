#![feature(trait_upcasting, drain_filter)]

pub use inkwell::OptimizationLevel;
use triomphe::Arc;

pub mod abi;
pub mod assembly;
pub mod body;
pub mod ctx;
pub mod layout;
pub mod linker;
pub mod local;
pub mod operand;
pub mod place;
pub mod ssa;
pub mod target;
pub mod ty;

pub trait Db: mir::Db + salsa::DbWithJar<Jar> {
    fn target(&self) -> &target::Target;
    fn target_dir(&self) -> &std::path::Path;
    fn optimization_level(&self) -> OptimizationLevel;
}

#[salsa::jar(db = Db)]
pub struct Jar(codegen_lib, codegen_module, layout::layout_of);

#[salsa::tracked]
pub fn codegen_lib(db: &dyn Db, lib: hir::id::LibId) -> Arc<assembly::Assembly> {
    let mut objects = Vec::new();

    ctx::with_codegen_ctx(db, lib.name(db), |mut ctx| {
        for module in hir::Lib::from(lib).modules(db) {
            if module.is_virtual(db) {
                continue;
            }

            // objects.push(codegen_module(db, module.id()));
            ctx.codegen(module);
        }

        let file = tempfile::Builder::new()
            .suffix(".o")
            .tempfile_in(db.target_dir())
            .unwrap()
            .into_temp_path();

        ctx.write(&file);
        objects.push(Arc::new(assembly::ObjectFile::new(file)));
    });

    Arc::new(assembly::Assembly::new(lib, objects))
}

#[salsa::tracked]
pub fn codegen_module(db: &dyn Db, module: hir::id::ModuleId) -> Arc<assembly::ObjectFile> {
    let file = tempfile::Builder::new()
        .suffix(".o")
        .tempfile_in(db.target_dir())
        .unwrap()
        .into_temp_path();

    ctx::with_codegen_ctx(db, module.name(db).as_str(db), |mut ctx| {
        ctx.codegen(hir::Module::from(module));
        ctx.write(&file);
    });

    Arc::new(assembly::ObjectFile::new(file))
}
