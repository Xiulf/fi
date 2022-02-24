pub mod assembly;
pub mod db;
pub mod linker;

use std::sync::Arc;

pub(crate) fn build_assembly(db: &dyn db::CodegenDatabase, lib: hir::Lib) -> Arc<assembly::Assembly> {
    let mut objects = Vec::new();

    for module in lib.modules(db.upcast()) {
        if module.is_virtual(db.upcast()) {
            continue;
        }

        objects.push(db.codegen_module(module));
    }

    Arc::new(assembly::Assembly::new(lib, objects))
}

pub(crate) fn codegen_module(db: &dyn db::CodegenDatabase, module: hir::Module) -> Arc<assembly::ObjectFile> {
    let ir = db.module_ir(module);
    println!("{}", ir.display(db.upcast()));
    let file = ir_codegen::compile_module(db.upcast(), &ir, ir_codegen::Backend::Clif);

    Arc::new(assembly::ObjectFile::new(file))
}
