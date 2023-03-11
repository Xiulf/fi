use std::sync::Arc;

use base_db::libs::LibId;
use rustc_hash::FxHashMap;
use smol_str::SmolStr;

use crate::id::{ItemId, TraitId, TypeAliasId, TypeCtorId, ValueId};
use crate::Db;

pub const KIND_KIND: &'static str = "kind-kind";
pub const TYPE_KIND: &'static str = "type-kind";
pub const INT_TAG_KIND: &'static str = "int-tag-kind";
pub const FLOAT_TAG_KIND: &'static str = "float-tag-kind";

pub const UNIT_TYPE: &'static str = "unit-type";
pub const BOOL_TYPE: &'static str = "bool-type";
pub const PAIR_TYPE: &'static str = "pair-type";
pub const INT_TYPE: &'static str = "int-type";
pub const FLOAT_TYPE: &'static str = "int-type";

pub const BIND_FN: &'static str = "bind-fn";

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LangItem {
    ValueId(ValueId),
    TypeAliasId(TypeAliasId),
    TypeCtorId(TypeCtorId),
    TraitId(TraitId),
}

#[derive(Default, Debug, PartialEq, Eq)]
pub struct LangItems {
    items: FxHashMap<SmolStr, LangItem>,
}

impl LangItem {
    pub fn as_value(self) -> Option<ValueId> {
        match self {
            | Self::ValueId(id) => Some(id),
            | _ => None,
        }
    }

    pub fn as_type_alias(self) -> Option<TypeAliasId> {
        match self {
            | Self::TypeAliasId(id) => Some(id),
            | _ => None,
        }
    }

    pub fn as_type_ctor(self) -> Option<TypeCtorId> {
        match self {
            | Self::TypeCtorId(id) => Some(id),
            | _ => None,
        }
    }

    pub fn as_trait(self) -> Option<TraitId> {
        match self {
            | Self::TraitId(id) => Some(id),
            | _ => None,
        }
    }
}

impl LangItems {
    pub fn target(&self, item: &'static str) -> Option<LangItem> {
        self.items.get(item).copied()
    }

    fn collect<T: Into<ItemId> + Copy>(&mut self, db: &dyn Db, item: T, ctor: fn(T) -> LangItem) {
        if let Some(lang_item_name) = lang_attr(db, item.into()) {
            self.items.entry(lang_item_name).or_insert_with(|| ctor(item));
        }
    }
}

fn lang_attr(db: &dyn Db, item: ItemId) -> Option<SmolStr> {
    let attrs = crate::attrs::query(db, item);
    let mut strings = attrs.by_key("lang").string_value();

    strings.next().map(|s| s.into())
}

#[salsa::tracked]
pub(crate) fn lib_lang_items(db: &dyn Db, lib: LibId) -> Arc<LangItems> {
    let mut lang_items = LangItems::default();
    let def_map = crate::def_map::query(db, lib);

    for (_, module) in def_map.modules() {
        for item in module.scope(db).items() {
            match item {
                | ItemId::ValueId(id) => lang_items.collect(db, id.into(), LangItem::ValueId),
                | ItemId::TypeAliasId(id) => lang_items.collect(db, id.into(), LangItem::TypeAliasId),
                | ItemId::TypeCtorId(id) => lang_items.collect(db, id.into(), LangItem::TypeCtorId),
                | ItemId::TraitId(id) => {
                    lang_items.collect(db, id.into(), LangItem::TraitId);

                    for (_, &child) in crate::data::trait_data(db, id).items(db) {
                        lang_items.collect(db, child.into(), LangItem::ValueId);
                    }
                },
                | _ => {},
            }
        }
    }

    Arc::new(lang_items)
}

#[salsa::tracked]
pub fn query(db: &dyn Db, lib: LibId, item: &'static str) -> Option<LangItem> {
    let lang_items = lib_lang_items(db, lib);
    let target = lang_items.items.get(item);

    if let Some(&target) = target {
        return Some(target);
    }

    // TODO: check deps
    None
}
