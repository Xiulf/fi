use std::sync::Arc;

use syntax::ast::NameOwner;
use vfs::File;

use super::*;
use crate::ast_id::AstIdMap;
use crate::name::AsName;
use crate::path::Path;
use crate::Db;

#[salsa::tracked]
pub fn query(db: &dyn Db, file: File) -> Arc<ItemTree> {
    let mut ctx = Ctx {
        db,
        tree: ItemTree {
            file,
            items: Vec::new(),
            data: ItemTreeData::default(),
        },
        ast_map: crate::ast_id::query(db, file),
    };

    let source_file = base_db::parse(db, file);

    ctx.lower_source_file(source_file);

    Arc::new(ctx.tree)
}

fn id<N: ItemTreeNode>(index: Idx<N>) -> LocalItemTreeId<N> {
    LocalItemTreeId {
        index,
        _marker: PhantomData,
    }
}

struct Ctx<'a> {
    db: &'a dyn Db,
    tree: ItemTree,
    ast_map: Arc<AstIdMap>,
}

impl Ctx<'_> {
    fn lower_source_file(&mut self, source_file: ast::SourceFile) {
        self.tree.items = source_file
            .module()
            .map(|m| self.lower_items(m.items()))
            .unwrap_or_default();
    }

    fn lower_items(&mut self, items: impl Iterator<Item = ast::Item>) -> Vec<Item> {
        items
            .flat_map(|item| self.lower_item(item))
            .flat_map(std::convert::identity)
            .collect()
    }

    fn lower_item(&mut self, item: ast::Item) -> Option<Vec<Item>> {
        let items = match item {
            | ast::Item::Module(it) => self.lower_module(it),
            | ast::Item::Value(it) => self.lower_value(it),
            | _ => todo!(),
        };

        items
    }

    fn lower_module(&mut self, module: ast::ItemModule) -> Option<Vec<Item>> {
        let ast_id = self.ast_map.ast_id(&module);
        let name = Path::from_ast(self.db, module.name()?).as_name()?;
        let items = self.lower_items(module.items()).into_boxed_slice();
        let data = Module { ast_id, name, items };

        Some(vec![id(self.tree.data.modules.alloc(data)).into()])
    }

    fn lower_value(&mut self, value: ast::ItemValue) -> Option<Vec<Item>> {
        let ast_id = self.ast_map.ast_id(&value);
        let name = value.name()?.as_name(self.db);
        let data = Value { ast_id, name };

        Some(vec![id(self.tree.data.values.alloc(data)).into()])
    }
}
