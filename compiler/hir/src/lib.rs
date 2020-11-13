#![feature(drain_filter, vec_remove_item)]

pub mod ir;
pub mod module_tree;

use module_tree::ModuleTree;
use std::sync::Arc;

#[salsa::query_group(HirDatabaseStorage)]
pub trait HirDatabase: syntax::SyntaxDatabase {
    #[salsa::invoke(ModuleTree::query)]
    fn module_tree(&self, lib: source::LibId) -> Arc<ModuleTree>;
}
