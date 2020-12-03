#![allow(incomplete_features)]
#![feature(generic_associated_types)]

pub mod builder;
pub mod convert;
pub mod ir;

use std::sync::Arc;

#[salsa::query_group(MirDatabaseStorage)]
pub trait MirDatabase: layout::LayoutDatabase + layout::ToLayoutDb {
    #[salsa::invoke(convert::convert)]
    fn module_mir(&self, lib: hir::ir::LibId, id: hir::ir::ModuleId) -> Arc<ir::Module>;

    fn type_info(&self, lib: hir::ir::LibId, ty: ir::Ty) -> ir::Operand;
}

pub trait ToMirDb {
    fn to_mir_db(&self) -> &dyn MirDatabase;
}

impl<T: MirDatabase> ToMirDb for T {
    fn to_mir_db(&self) -> &dyn MirDatabase {
        self
    }
}

fn type_info(db: &dyn MirDatabase, lib: hir::ir::LibId, ty: ir::Ty) -> ir::Operand {
    let ptr_ty = db.lang_items().ptr_ty();
    let ptr_ty = ir::Ty::data(ptr_ty.owner);
    let type_info = db.lang_items().type_info();
    let type_info = db.typecheck(type_info.owner).ty.clone();
    let ptr_ty = ir::Ty::app(ptr_ty.clone(), ptr_ty, vec![type_info].into());
    let basic_copy = db.lang_items().basic_copy();
    let basic_drop = db.lang_items().basic_drop();
    let basic_vwt = ir::Const::Tuple(vec![
        ir::Const::FuncAddr(basic_copy.owner),
        ir::Const::FuncAddr(basic_copy.owner),
        ir::Const::FuncAddr(basic_drop.owner),
    ]);

    let layout = db.layout_of(lib, ty);

    let info = ir::Const::Tuple(vec![
        ir::Const::Scalar(layout.size.bytes() as u128),
        ir::Const::Scalar(layout.align.bytes() as u128),
        ir::Const::Scalar(layout.stride.bytes() as u128),
        ir::Const::Ref(Box::new(basic_vwt)),
    ]);

    let info = ir::Const::Tuple(vec![info]);

    ir::Operand::Const(ir::Const::Ref(Box::new(info)), ptr_ty)
}
