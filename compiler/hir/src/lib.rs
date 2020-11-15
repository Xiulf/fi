#![feature(drain_filter, vec_remove_item)]

pub mod convert;
pub mod ir;
pub mod module_tree;
pub mod resolve;

use module_tree::ModuleTree;
use std::sync::Arc;

#[salsa::query_group(HirDatabaseStorage)]
pub trait HirDatabase: syntax::SyntaxDatabase {
    #[salsa::invoke(ModuleTree::query)]
    fn module_tree(&self, lib: source::LibId) -> Arc<ModuleTree>;

    #[salsa::invoke(convert::convert)]
    fn module_hir(&self, file: source::FileId) -> Arc<ir::Module>;
}
