use base_db::input::File;
use base_db::libs::LibId;
use syntax::ast::NameOwner;

use crate::id::ModuleId;
use crate::name::AsName;
use crate::{Db, Module};

#[salsa::tracked]
pub struct DefMap {
    #[return_ref]
    pub modules: Vec<Module>,
}

#[salsa::tracked]
pub fn query(db: &dyn Db, lib: LibId) -> DefMap {
    let mut modules = Vec::new();

    for file in lib.source_root(db).iter(db) {
        if let Some(module) = lower_file(db, lib, file) {
            modules.push(module);
        }
    }

    DefMap::new(db, modules)
}

pub fn lower_file(db: &dyn Db, lib: LibId, file: File) -> Option<Module> {
    let tree = base_db::parse(db, file).tree();
    let module = tree.module()?;
    let name = module.name()?.as_name(db);
    let id = ModuleId::new(db, lib, name);

    Some(Module::new(db, id))
}
