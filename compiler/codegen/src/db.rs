use crate::assembly::Assembly;
use base_db::Upcast;
use mir::db::MirDatabase;
use std::sync::Arc;

#[salsa::query_group(CodegenDatabaseStorage)]
pub trait CodegenDatabase: MirDatabase + Upcast<dyn MirDatabase> {
    #[salsa::invoke(crate::assembly::build_assembly)]
    fn lib_assembly(&self, lib: hir::Lib) -> Arc<Assembly>;
}
