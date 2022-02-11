use std::sync::Arc;

use base_db::Upcast;
use hir::id::TypeCtorId;

#[salsa::query_group(LowerDatabaseStorage)]
pub trait LowerDatabase:
    ir::db::IrDatabase + hir::db::HirDatabase + Upcast<dyn ir::db::IrDatabase> + Upcast<dyn hir::db::HirDatabase>
{
    #[salsa::invoke(crate::module_ir)]
    fn module_ir(&self, module: hir::Module) -> Arc<ir::Module>;

    #[salsa::invoke(crate::func_ir)]
    fn func_ir(&self, func: hir::Func) -> ir::FuncId;

    #[salsa::invoke(crate::body_ir)]
    fn body_ir(&self, body: hir::id::DefWithBodyId) -> ir::BodyId;

    #[salsa::invoke(crate::types::is_boxed)]
    fn is_boxed(&self, ty: TypeCtorId) -> bool;
}
