pub mod db;
mod types;

use std::sync::Arc;

pub fn module_ir(db: &dyn db::LowerDatabase, module: hir::Module) -> Arc<ir::Module> {
    todo!()
}

pub fn func_ir(db: &dyn db::LowerDatabase, func: hir::Func) -> ir::FuncId {
    todo!()
}

pub fn body_ir(db: &dyn db::LowerDatabase, body: hir::id::DefWithBodyId) -> ir::BodyId {
    todo!()
}
