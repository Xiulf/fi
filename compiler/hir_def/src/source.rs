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

    fn source(&self, db: &dyn Db) -> InFile<Self::Value> {
        let type_ctor = self.type_ctor(db);
        let local_id = self.local_id(db);
        let it = type_ctor.it(db);
        let tree = item_tree::query(db, it.file);
        let ast_map = ast_id::query(db, it.file);
        let root = base_db::parse(db, it.file);
        let node = &tree[it.value];
        let idx = node.ctors.iter().position(|&c| c == local_id).unwrap();
        let node = ast_map.get(node.ast_id).to_node(root.syntax());
        let node = node.ctors().nth(idx).unwrap();

        it.with_value(node)
    }
}

impl HasSource for FieldId {
    type Value = syntax::ast::CtorField;

    fn source(&self, db: &dyn Db) -> InFile<Self::Value> {
        let ctor = self.ctor(db);
        let type_ctor = ctor.type_ctor(db);
        let local_id = self.local_id(db);
        let it = type_ctor.it(db);
        let item_tree = crate::item_tree::query(db, it.file);
        let ctor_data = &item_tree[ctor.local_id(db)];
        let ctor_source = ctor.source(db);
        let fields = ctor_data.fields.as_deref().unwrap_or(&[]);
        let idx = fields.iter().position(|&f| f == local_id).unwrap();
        let node = ctor_source.value.record().unwrap().fields().nth(idx).unwrap();

        ctor_source.with_value(node)
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
