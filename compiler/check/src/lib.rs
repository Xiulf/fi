#![feature(extern_types)]

pub mod list;
pub mod ty;

// #[salsa::query_group(TypeDatabaseStorage)]
// pub trait TypeDatabase<'tcx>: hir::HirDatabase {
//     fn type_of(&self, id: hir::ir::DefId) -> ty::Ty<'tcx>;
// }
