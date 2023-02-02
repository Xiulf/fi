use arena::ArenaMap;
use syntax::ast::AstNode;
use vfs::InFile;

use crate::id::ValueId;
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
