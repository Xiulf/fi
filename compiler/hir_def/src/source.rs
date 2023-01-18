use arena::{ArenaMap, Idx};

use crate::db::DefDatabase;
use crate::id::{
    AssocItemLoc, DefWithBodyId, ItemLoc, LocalCtorId, Lookup, ModuleDefId, TypeCtorId, TypeVarOwner, TypedDefId,
};
use crate::in_file::InFile;
use crate::item_tree::ItemTreeNode;

pub trait HasSource {
    type Value;

    fn source(&self, db: &dyn DefDatabase) -> InFile<Self::Value>;
}

pub trait HasChildSource<ChildId> {
    type Value;

    fn child_source(&self, db: &dyn DefDatabase) -> InFile<ArenaMap<ChildId, Self::Value>>;
}

impl<N: ItemTreeNode> HasSource for AssocItemLoc<N> {
    type Value = N::Source;

    fn source(&self, db: &dyn DefDatabase) -> InFile<Self::Value> {
        let tree = db.item_tree(self.id.file_id);
        let ast_id_map = db.ast_id_map(self.id.file_id);
        let root = db.parse(self.id.file_id).syntax_node();
        let node = &tree[self.id.value];

        InFile::new(self.id.file_id, ast_id_map.get(node.ast_id()).to_node(&root))
    }
}

impl<N: ItemTreeNode> HasSource for ItemLoc<N> {
    type Value = N::Source;

    fn source(&self, db: &dyn DefDatabase) -> InFile<Self::Value> {
        let tree = db.item_tree(self.id.file_id);
        let ast_id_map = db.ast_id_map(self.id.file_id);
        let root = db.parse(self.id.file_id).syntax_node();
        let node = &tree[self.id.value];

        InFile::new(self.id.file_id, ast_id_map.get(node.ast_id()).to_node(&root))
    }
}

impl HasSource for ModuleDefId {
    type Value = syntax::ast::Item;

    fn source(&self, db: &dyn DefDatabase) -> InFile<Self::Value> {
        match self {
            | Self::ModuleId(id) => {
                let def_map = db.def_map(id.lib);
                def_map[id.local_id].origin.declaration(db, &def_map).map(Into::into)
            },
            | Self::FixityId(id) => id.lookup(db).source(db).map(Into::into),
            | Self::FuncId(id) => id.lookup(db).source(db).map(Into::into),
            | Self::StaticId(id) => id.lookup(db).source(db).map(Into::into),
            | Self::ConstId(id) => id.lookup(db).source(db).map(Into::into),
            | Self::TypeAliasId(id) => id.lookup(db).source(db).map(Into::into),
            | Self::TypeCtorId(id) => id.lookup(db).source(db).map(Into::into),
            | Self::CtorId(id) => id.parent.lookup(db).source(db).map(Into::into),
            | Self::ClassId(id) => id.lookup(db).source(db).map(Into::into),
        }
    }
}

impl HasSource for TypedDefId {
    type Value = syntax::ast::Item;

    fn source(&self, db: &dyn DefDatabase) -> InFile<Self::Value> {
        match self {
            | TypedDefId::FuncId(id) => id.lookup(db).source(db).map(Into::into),
            | TypedDefId::StaticId(id) => id.lookup(db).source(db).map(Into::into),
            | TypedDefId::ConstId(id) => id.lookup(db).source(db).map(Into::into),
            | TypedDefId::TypeAliasId(id) => id.lookup(db).source(db).map(Into::into),
            | TypedDefId::TypeCtorId(id) => id.lookup(db).source(db).map(Into::into),
            | TypedDefId::CtorId(id) => id.parent.lookup(db).source(db).map(Into::into),
            | TypedDefId::ClassId(id) => id.lookup(db).source(db).map(Into::into),
            | TypedDefId::MemberId(id) => id.lookup(db).source(db).map(Into::into),
        }
    }
}

impl HasSource for DefWithBodyId {
    type Value = syntax::ast::Item;

    fn source(&self, db: &dyn DefDatabase) -> InFile<Self::Value> {
        match self {
            | DefWithBodyId::FuncId(id) => id.lookup(db).source(db).map(Into::into),
            | DefWithBodyId::StaticId(id) => id.lookup(db).source(db).map(Into::into),
            | DefWithBodyId::ConstId(id) => id.lookup(db).source(db).map(Into::into),
        }
    }
}

impl HasSource for TypeVarOwner {
    type Value = syntax::ast::Item;

    fn source(&self, db: &dyn DefDatabase) -> InFile<Self::Value> {
        match self {
            | TypeVarOwner::DefWithBodyId(def) => def.source(db),
            | TypeVarOwner::TypedDefId(def) => def.source(db),
        }
    }
}

impl HasChildSource<LocalCtorId> for TypeCtorId {
    type Value = syntax::ast::Ctor;

    fn child_source(&self, db: &dyn DefDatabase) -> InFile<ArenaMap<LocalCtorId, Self::Value>> {
        let loc = self.lookup(db);
        let src = loc.source(db);
        let lib = loc.module.lib;
        let libs = db.libs();
        let cfg_opts = &libs[lib].cfg_options;
        let item_tree = db.item_tree(src.file_id);
        let it = &item_tree[loc.id.value];
        let mut map = ArenaMap::default();
        let mut items = src.value.iter();
        let first = items.next().unwrap();
        let next = items.next();
        let ctors = next.as_ref().map(|it| it.ctors()).unwrap_or_else(|| first.ctors());

        for (ctor, ctor_id) in ctors.zip(it.ctors.clone()) {
            if let Some(cfg) = item_tree.attrs(ctor_id.into()).cfg() {
                if !cfg.is_enabled(cfg_opts) {
                    continue;
                }
            }

            let ctor_id = Idx::from_raw(ctor_id.into_raw());
            map.insert(ctor_id, ctor);
        }

        InFile::new(src.file_id, map)
    }
}
