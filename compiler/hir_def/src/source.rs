use arena::ArenaMap;
use syntax::ast::AstNode;
use vfs::InFile;

use crate::id::{CtorId, FieldId, FixityId, ImplId, ModuleId, TraitId, TypeAliasId, TypeCtorId, ValueId};
use crate::item_tree::{self, ItemTreeNode};
use crate::{ast_id, Db};

pub trait HasSource {
    type Value;

    fn source(&self, db: &dyn Db) -> InFile<Self::Value>;
}

pub trait HasChildSource<ChildId> {
    type Value;

    fn child_source(&self, db: &dyn Db) -> InFile<ArenaMap<ChildId, Self::Value>>;
}

impl HasSource for ModuleId {
    type Value = <item_tree::Module as ItemTreeNode>::Source;

    fn source(&self, _db: &dyn Db) -> InFile<Self::Value> {
        // let it = self.it(db);
        // let tree = item_tree::query(db, it.file);
        // let ast_map = ast_id::query(db, it.file);
        // let root = base_db::parse(db, it.file);
        // let node = &tree[it.value];

        // it.with_value(ast_map.get(node.ast_id).to_node(root.syntax()))
        todo!()
    }
}

impl HasSource for FixityId {
    type Value = <item_tree::Fixity as ItemTreeNode>::Source;

    fn source(&self, db: &dyn Db) -> InFile<Self::Value> {
        let it = self.it(db);
        let tree = item_tree::query(db, it.file);
        let ast_map = ast_id::query(db, it.file);
        let root = base_db::parse(db, it.file);
        let node = &tree[it.value];

        it.with_value(ast_map.get(node.ast_id).to_node(root.syntax()))
    }
}

impl HasSource for ValueId {
    type Value = <item_tree::Value as ItemTreeNode>::Source;

    fn source(&self, db: &dyn Db) -> InFile<Self::Value> {
        let it = self.it(db);
        let tree = item_tree::query(db, it.file);
        let ast_map = ast_id::query(db, it.file);
        let root = base_db::parse(db, it.file);
        let node = &tree[it.value];

        it.with_value(ast_map.get(node.ast_id).to_node(root.syntax()))
    }
}

impl HasSource for TypeAliasId {
    type Value = <item_tree::TypeAlias as ItemTreeNode>::Source;

    fn source(&self, db: &dyn Db) -> InFile<Self::Value> {
        let it = self.it(db);
        let tree = item_tree::query(db, it.file);
        let ast_map = ast_id::query(db, it.file);
        let root = base_db::parse(db, it.file);
        let node = &tree[it.value];

        it.with_value(ast_map.get(node.ast_id).to_node(root.syntax()))
    }
}

impl HasSource for TypeCtorId {
    type Value = <item_tree::TypeCtor as ItemTreeNode>::Source;

    fn source(&self, db: &dyn Db) -> InFile<Self::Value> {
        let it = self.it(db);
        let tree = item_tree::query(db, it.file);
        let ast_map = ast_id::query(db, it.file);
        let root = base_db::parse(db, it.file);
        let node = &tree[it.value];

        it.with_value(ast_map.get(node.ast_id).to_node(root.syntax()))
    }
}

impl HasSource for CtorId {
    type Value = syntax::ast::Ctor;

    fn source(&self, _db: &dyn Db) -> InFile<Self::Value> {
        // let it = self.it(db);
        // let tree = item_tree::query(db, it.file);
        // let ast_map = ast_id::query(db, it.file);
        // let root = base_db::parse(db, it.file);
        // let node = &tree[it.value];

        // it.with_value(ast_map.get(node.ast_id).to_node(root.syntax()))
        todo!()
    }
}

impl HasSource for FieldId {
    type Value = syntax::ast::CtorField;

    fn source(&self, _db: &dyn Db) -> InFile<Self::Value> {
        // let it = self.it(db);
        // let tree = item_tree::query(db, it.file);
        // let ast_map = ast_id::query(db, it.file);
        // let root = base_db::parse(db, it.file);
        // let node = &tree[it.value];

        // it.with_value(ast_map.get(node.ast_id).to_node(root.syntax()))
        todo!()
    }
}

impl HasSource for TraitId {
    type Value = <item_tree::Trait as ItemTreeNode>::Source;

    fn source(&self, db: &dyn Db) -> InFile<Self::Value> {
        let it = self.it(db);
        let tree = item_tree::query(db, it.file);
        let ast_map = ast_id::query(db, it.file);
        let root = base_db::parse(db, it.file);
        let node = &tree[it.value];

        it.with_value(ast_map.get(node.ast_id).to_node(root.syntax()))
    }
}

impl HasSource for ImplId {
    type Value = <item_tree::Impl as ItemTreeNode>::Source;

    fn source(&self, db: &dyn Db) -> InFile<Self::Value> {
        let it = self.it(db);
        let tree = item_tree::query(db, it.file);
        let ast_map = ast_id::query(db, it.file);
        let root = base_db::parse(db, it.file);
        let node = &tree[it.value];

        it.with_value(ast_map.get(node.ast_id).to_node(root.syntax()))
    }
}
