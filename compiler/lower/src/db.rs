use base_db::Upcast;
use hir::id::TypeCtorId;

#[salsa::query_group(LowerDatabaseStorage)]
pub trait LowerDatabase: ir::db::IrDatabase + hir::db::HirDatabase + Upcast<dyn hir::db::HirDatabase> {
    #[salsa::invoke(crate::types::is_boxed)]
    fn is_boxed(&self, ty: TypeCtorId) -> bool;
}
