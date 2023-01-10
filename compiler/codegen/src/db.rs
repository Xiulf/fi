use std::sync::Arc;

use base_db::Upcast;
use mir::db::MirDatabase;
use paths::AbsPathBuf;

use crate::assembly::{Assembly, ObjectFile};

#[salsa::query_group(CodegenDatabaseStorage)]
pub trait CodegenDatabase: MirDatabase + Upcast<dyn MirDatabase> {
    #[salsa::input]
    fn target_dir(&self) -> AbsPathBuf;

    #[salsa::invoke(crate::build_assembly)]
    fn lib_assembly(&self, lib: hir::Lib) -> Arc<Assembly>;

    #[salsa::invoke(crate::codegen_module)]
    fn codegen_module(&self, module: hir::Module) -> Arc<ObjectFile>;
}
