use std::sync::Arc;

use base_db::Upcast;
use mir::db::MirDatabase;

use crate::assembly::{Assembly, ObjectFile};
use crate::backend::Backend;
use crate::CompilerTarget;

#[salsa::query_group(CodegenDatabaseStorage)]
pub trait CodegenDatabase: MirDatabase + Upcast<dyn MirDatabase> {
    #[salsa::input]
    fn target(&self) -> CompilerTarget;

    #[salsa::invoke(crate::backend::backend)]
    fn backend(&self) -> Backend;

    #[salsa::invoke(crate::build_assembly)]
    fn lib_assembly(&self, lib: hir::Lib) -> Arc<Assembly>;

    #[salsa::invoke(crate::codegen_module)]
    fn codegen_module(&self, module: hir::Module) -> Arc<ObjectFile>;
}
