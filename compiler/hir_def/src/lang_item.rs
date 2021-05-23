use crate::db::DefDatabase;
use crate::id::{AttrDefId, ClassId, FuncId, ModuleDefId, StaticId, TypeAliasId, TypeCtorId};
use base_db::libs::LibId;
use rustc_hash::FxHashMap;
use smol_str::SmolStr;
use std::sync::Arc;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LangItem {
    FuncId(FuncId),
    StaticId(StaticId),
    TypeAliasId(TypeAliasId),
    TypeCtorId(TypeCtorId),
    ClassId(ClassId),
}

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct LangItems {
    items: FxHashMap<SmolStr, LangItem>,
}

impl LangItem {
    pub fn as_func(self) -> Option<FuncId> {
        match self {
            | LangItem::FuncId(id) => Some(id),
            | _ => None,
        }
    }

    pub fn as_static(self) -> Option<StaticId> {
        match self {
            | LangItem::StaticId(id) => Some(id),
            | _ => None,
        }
    }

    pub fn as_type_alias(self) -> Option<TypeAliasId> {
        match self {
            | LangItem::TypeAliasId(id) => Some(id),
            | _ => None,
        }
    }

    pub fn as_type_ctor(self) -> Option<TypeCtorId> {
        match self {
            | LangItem::TypeCtorId(id) => Some(id),
            | _ => None,
        }
    }

    pub fn as_class(self) -> Option<ClassId> {
        match self {
            | LangItem::ClassId(id) => Some(id),
            | _ => None,
        }
    }
}

impl LangItems {
    pub fn target(&self, item: &str) -> Option<LangItem> {
        self.items.get(item).copied()
    }

    pub(crate) fn lib_lang_items_query(db: &dyn DefDatabase, lib: LibId) -> Arc<LangItems> {
        let mut lang_items = LangItems::default();
        let def_map = db.def_map(lib);

        for (_, module) in def_map.modules() {
            for def in module.scope.declarations() {
                match def {
                    | ModuleDefId::FuncId(id) => lang_items.collect_lang_item(db, id, LangItem::FuncId),
                    | ModuleDefId::StaticId(id) => lang_items.collect_lang_item(db, id, LangItem::StaticId),
                    | ModuleDefId::TypeAliasId(id) => lang_items.collect_lang_item(db, id, LangItem::TypeAliasId),
                    | ModuleDefId::TypeCtorId(id) => lang_items.collect_lang_item(db, id, LangItem::TypeCtorId),
                    | ModuleDefId::ClassId(id) => lang_items.collect_lang_item(db, id, LangItem::ClassId),
                    | _ => {},
                }
            }
        }

        Arc::new(lang_items)
    }

    pub(crate) fn lang_item_query(db: &dyn DefDatabase, lib: LibId, item: SmolStr) -> Option<LangItem> {
        let lang_items = db.lib_lang_items(lib);
        let target = lang_items.items.get(&item);

        if let Some(target) = target {
            return Some(*target);
        }

        db.libs()[lib]
            .deps
            .iter()
            .find_map(|dep| db.lang_item(*dep, item.clone()))
    }

    fn collect_lang_item<T>(&mut self, db: &dyn DefDatabase, item: T, ctor: fn(T) -> LangItem)
    where
        T: Into<AttrDefId> + Copy,
    {
        if let Some(lang_item_name) = lang_attr(db, item) {
            self.items.entry(lang_item_name).or_insert_with(|| ctor(item));
        }
    }
}

fn lang_attr(db: &dyn DefDatabase, item: impl Into<AttrDefId> + Copy) -> Option<SmolStr> {
    let attrs = db.attrs(item.into());
    let mut strings = attrs.by_key("lang").string_value();

    strings.next().map(|s| s.into())
}
