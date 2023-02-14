#![feature(trait_upcasting, let_chains, iter_intersperse)]

pub mod ast_id;
pub mod attrs;
pub mod body;
pub mod data;
pub mod def_map;
pub mod diagnostics;
pub mod display;
pub mod expr;
pub mod id;
pub mod item_tree;
pub mod lang_item;
pub mod name;
pub mod pat;
pub mod path;
pub mod per_ns;
pub mod source;
pub mod type_ref;

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
    id::TypeVarId,
    id::ITypedItemId,
    data::ModuleData,
    data::FixityData,
    data::fixity_data,
    data::ValueData,
    data::value_data,
    data::TypeAliasData,
    data::TypeCtorData,
    data::type_ctor_data,
    data::CtorData,
    data::ctor_data,
    data::TraitData,
    data::trait_data,
    data::ImplData,
    data::impl_data,
    name::Name,
    ast_id::query,
    item_tree::query,
    def_map::query,
    body::query,
    type_ref::query,
    lang_item::lib_lang_items,
    lang_item::query,
);
