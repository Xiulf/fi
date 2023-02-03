#![feature(trait_upcasting, let_chains)]

pub mod ast_id;
pub mod body;
pub mod data;
pub mod def_map;
pub mod diagnostics;
pub mod expr;
pub mod id;
pub mod item_tree;
pub mod name;
pub mod pat;
pub mod path;
pub mod per_ns;
pub mod source;

use ::diagnostics::{DiagnosticSink, Diagnostics};
use base_db::libs::LibId;
use id::ItemId;

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
    id::FieldId,
    id::TraitId,
    id::ImplId,
    data::ModuleData,
    data::FixityData,
    data::fixity_data,
    data::ValueData,
    data::value_data,
    data::TypeAliasData,
    data::TypeCtorData,
    data::CtorData,
    data::TraitData,
    data::ImplData,
    data::impl_data,
    name::Name,
    ast_id::query,
    item_tree::query,
    def_map::query,
    body::query,
    lib_diagnostics,
);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Lib {
    id: LibId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Module {
    data: data::ModuleData,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Item {
    Value(Value),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Value {
    data: data::ValueData,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Impl {
    data: data::ImplData,
}

#[salsa::tracked]
fn lib_diagnostics(db: &dyn Db, id: LibId) {
    let lib = Lib { id };

    for module in lib.modules(db) {
        module.run_all_queries(db);
    }
}

impl Lib {
    pub fn deps(self, db: &dyn Db) -> Vec<Lib> {
        self.id.deps(db).into_iter().map(|&l| l.into()).collect()
    }

    pub fn modules(self, db: &dyn Db) -> Vec<Module> {
        let def_map = def_map::query(db, self.id);

        def_map.modules().map(|(_, data)| Module { data }).collect()
    }

    pub fn diagnostics(self, db: &dyn Db, sink: &mut dyn DiagnosticSink) {
        lib_diagnostics(db, self.id);

        for diag in lib_diagnostics::accumulated::<Diagnostics>(db, self.id) {
            sink.add_diagnostic(diag);
        }
    }
}

impl Module {
    pub fn items(self, db: &dyn Db) -> impl Iterator<Item = Item> + '_ {
        self.data.scope(db).items().filter_map(|it| match it {
            | ItemId::ValueId(id) => Some(Item::Value(data::value_data(db, id).into())),
            | _ => None,
        })
    }

    pub fn impls(self, db: &dyn Db) -> impl Iterator<Item = Impl> + '_ {
        self.data
            .scope(db)
            .impls()
            .map(|id| Impl::from(data::impl_data(db, id)))
    }

    fn run_all_queries(self, db: &dyn Db) {
        for item in self.items(db) {
            item.run_all_queries(db);
        }

        for impl_ in self.impls(db) {
            impl_.run_all_queries(db);
        }
    }
}

impl Item {
    fn run_all_queries(self, db: &dyn Db) {
        match self {
            | Self::Value(it) => it.run_all_queries(db),
        }
    }
}

impl Value {
    fn run_all_queries(self, db: &dyn Db) {
        body::query(db, self.data.id(db));
    }
}

impl Impl {
    pub fn items(self, db: &dyn Db) -> impl Iterator<Item = Value> + '_ {
        self.data.items(db).values().map(|&id| data::value_data(db, id).into())
    }

    fn run_all_queries(self, db: &dyn Db) {
        for item in self.items(db) {
            item.run_all_queries(db);
        }
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

impl From<data::ValueData> for Value {
    fn from(data: data::ValueData) -> Self {
        Self { data }
    }
}

impl From<data::ImplData> for Impl {
    fn from(data: data::ImplData) -> Self {
        Self { data }
    }
}
