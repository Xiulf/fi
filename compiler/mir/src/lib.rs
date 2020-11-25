#![allow(incomplete_features)]
#![feature(generic_associated_types)]

pub mod builder;
pub mod convert;
pub mod ir;

use std::sync::Arc;

#[salsa::query_group(MirDatabaseStorage)]
pub trait MirDatabase: check::TypeDatabase {
    #[salsa::invoke(convert::convert)]
    fn module_mir(&self, lib: hir::ir::LibId, id: hir::ir::ModuleId) -> Arc<ir::Module>;

    fn type_info(&self, ty: ir::Ty) -> ir::Operand;
}

fn type_info(db: &dyn MirDatabase, ty: ir::Ty) -> ir::Operand {
    let ptr_ty = db.lang_items().ptr_ty();
    let ptr_ty = ir::Ty::data(ptr_ty.owner);
    let type_info = db.lang_items().type_info();
    let type_info = db.typecheck(type_info.owner).ty.clone();
    let ptr_ty = ir::Ty::app(ptr_ty, vec![type_info].into());

    ir::Operand::Const(ir::Const::Undefined, ptr_ty)
}
