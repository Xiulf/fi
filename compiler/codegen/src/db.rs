use crate::assembly::Assembly;
use lower::db::LowerDatabase;
use std::sync::Arc;

#[salsa::query_group(CodegenDatabaseStorage)]
pub trait CodegenDatabase: LowerDatabase {
    #[salsa::invoke(crate::build_assembly)]
    fn lib_assembly(&self, lib: hir::Lib) -> Arc<Assembly>;
}
