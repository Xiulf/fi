#![feature(trait_upcasting)]

pub mod ast_id;
pub mod data;
pub mod def_map;
pub mod diagnostics;
pub mod id;
pub mod item_tree;
pub mod name;
pub mod path;
pub mod per_ns;

use base_db::libs::LibId;

pub trait Db: base_db::Db + salsa::DbWithJar<Jar> {}

impl<T: ?Sized + base_db::Db + salsa::DbWithJar<Jar>> Db for T {
}

#[salsa::jar(db = Db)]
pub struct Jar(
    id::ModuleId,
    id::FixityId,
    id::ValueId,
    id::TypeAliasId,
    id::TypeCtorId,
    id::CtorId,
    id::TraitId,
    id::ImplId,
    data::ModuleData,
    data::FixityData,
    data::ValueData,
    data::TypeAliasData,
    data::TypeCtorData,
    data::CtorData,
    data::TraitData,
    data::ImplData,
    name::Name,
    ast_id::query,
    item_tree::query,
    def_map::DefMap,
    def_map::query,
);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Lib {
    id: LibId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Module {
    data: data::ModuleData,
}

impl Lib {
    pub fn deps(self, db: &dyn Db) -> Vec<Lib> {
        self.id.deps(db).into_iter().map(|&l| l.into()).collect()
    }

    pub fn modules(self, db: &dyn Db) -> impl Iterator<Item = Module> + '_ {
        let def_map = def_map::query(db, self.id);

        def_map.modules(db).into_iter().map(|(_, &data)| Module { data })
    }
}

impl From<LibId> for Lib {
    fn from(id: LibId) -> Self {
        Self { id }
    }
}

impl From<data::ModuleData> for Module {
    fn from(data: data::ModuleData) -> Self {
        Self { data }
    }
}
