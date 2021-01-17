#![feature(drain_filter, vec_remove_item, or_patterns)]

pub mod convert;
pub mod ir;
pub mod lang;
pub mod module_tree;
pub mod resolve;

use module_tree::ModuleTree;
use std::sync::Arc;

#[salsa::query_group(HirDatabaseStorage)]
pub trait HirDatabase: syntax::SyntaxDatabase {
    #[salsa::invoke(module_tree::load_external)]
    fn external_modules(&self, lib: source::LibId) -> Arc<Vec<module_tree::ExternalModuleData>>;

    #[salsa::invoke(ModuleTree::query)]
    fn module_tree(&self, lib: source::LibId) -> Arc<ModuleTree>;

    #[salsa::invoke(convert::convert)]
    fn module_hir(&self, file: source::FileId) -> Arc<ir::Module>;

    #[salsa::invoke(lang::LangItems::collect)]
    fn lang_items(&self) -> Arc<lang::LangItems>;
}
