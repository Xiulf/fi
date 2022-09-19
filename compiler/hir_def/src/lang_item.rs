use std::sync::Arc;

use base_db::libs::LibId;
use rustc_hash::FxHashMap;
use smol_str::SmolStr;

use crate::db::DefDatabase;
use crate::id::{AttrDefId, ClassId, FixityId, FuncId, ModuleDefId, StaticId, TypeAliasId, TypeCtorId};

pub const TYPE_KIND: &'static str = "type-kind";
pub const FIGURE_KIND: &'static str = "figure-kind";
pub const SYMBOL_KIND: &'static str = "symbol-kind";
pub const ROW_KIND: &'static str = "row-kind";

pub const NEVER_TYPE: &'static str = "never-type";
pub const CHAR_TYPE: &'static str = "char-type";
pub const STR_TYPE: &'static str = "str-type";
pub const FN_TYPE: &'static str = "fn-type";
pub const ARRAY_TYPE: &'static str = "array-type";
pub const RECORD_TYPE: &'static str = "record-type";
pub const UNIT_TYPE: &'static str = "unit-type";
pub const BOOL_TYPE: &'static str = "bool-type";
pub const PROXY_TYPE: &'static str = "proxy-type";
pub const PAIR_TYPE: &'static str = "pair-type";

pub const INTEGER_CLASS: &'static str = "integer-clss";
pub const DECIMAL_CLASS: &'static str = "decimal-class";
pub const TERMINATION_CLASS: &'static str = "termination-class";
pub const TRY_CLASS: &'static str = "try-class";

pub const PAIR_OPERATOR: &'static str = "pair-operator";

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LangItem {
    FixityId(FixityId),
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
    pub fn as_fixity(self) -> Option<FixityId> {
        match self {
            | LangItem::FixityId(id) => Some(id),
            | _ => None,
        }
    }

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
                    | ModuleDefId::FixityId(id) => lang_items.collect_lang_item(db, id, LangItem::FixityId),
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

    pub(crate) fn lang_item_query(db: &dyn DefDatabase, lib: LibId, item: &'static str) -> Option<LangItem> {
        let lang_items = db.lib_lang_items(lib);
        let target = lang_items.items.get(item);

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
