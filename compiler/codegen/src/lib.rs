pub mod assembly;
pub mod db;
pub mod linker;

use std::sync::Arc;

use base_db::target::CompilerTarget;

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
    let mut file = tempfile::Builder::new()
        .suffix(".o")
        .tempfile_in(db.target_dir())
        .unwrap();

    match db.target() {
        | CompilerTarget::Javascript => backend_js::codegen(db.upcast(), module, &mut file),
        | CompilerTarget::Native(_) => backend_llvm::codegen(db.upcast(), module, &mut file),
    }

    Arc::new(assembly::ObjectFile::new(file))
}
