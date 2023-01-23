#![feature(trait_upcasting)]

pub mod def_map;
pub mod id;
pub mod name;

use base_db::libs::LibId;

pub trait Db: base_db::Db + salsa::DbWithJar<Jar> {}

#[salsa::jar(db = Db)]
pub struct Jar(id::ModuleId, name::Name, def_map::DefMap, def_map::query, Module);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Lib {
    id: LibId,
}

#[salsa::tracked]
pub struct Module {
    pub name: id::ModuleId,
}

impl Lib {
    pub fn deps(self, db: &dyn Db) -> Vec<Lib> {
        self.id.deps(db).into_iter().map(|&l| l.into()).collect()
    }

    pub fn modules(self, db: &dyn Db) -> Vec<Module> {
        let def_map = def_map::query(db, self.id);

        def_map.modules(db).clone()
    }
}

impl From<LibId> for Lib {
    fn from(id: LibId) -> Self {
        Self { id }
    }
}
