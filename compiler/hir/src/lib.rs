pub mod ir;

#[salsa::query_group(HirDatabaseStorage)]
pub trait HirDatabse: syntax::SyntaxDatabase {
    // fn module_tree(&self, package: PackageId) -> Arc<ModuleTree>;
}
