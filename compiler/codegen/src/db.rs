use crate::assembly::Assembly;
use base_db::Upcast;
use mir::db::MirDatabase;
use std::sync::Arc;

#[salsa::query_group(CodegenDatabaseStorage)]
pub trait CodegenDatabase: MirDatabase + Upcast<dyn MirDatabase> {
    #[salsa::invoke(crate::assembly::build_assembly)]
    fn module_assembly(&self, module: hir::Module) -> Arc<Assembly>;
}
