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
}
