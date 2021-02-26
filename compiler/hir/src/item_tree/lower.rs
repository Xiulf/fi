use crate::arena::Idx;
use crate::ast_id::AstIdMap;
use crate::db::DefDatabase;
use crate::in_file::InFile;
use crate::item_tree::{Import, Item, ItemTree, ItemTreeNode, LocalItemTreeId, Type};
use crate::name::AsName;
use crate::path::ModPath;
use base_db::input::FileId;
use std::marker::PhantomData;
use std::sync::Arc;
use syntax::ast::{self, NameOwner};

fn id<N: ItemTreeNode>(index: Idx<N>) -> LocalItemTreeId<N> {
    LocalItemTreeId {
        index,
        _marker: PhantomData,
    }
}

pub(super) struct Ctx {
    tree: ItemTree,
    file_id: FileId,
    ast_id_map: Arc<AstIdMap>,
}

struct Items(Vec<Item>);

impl<T: Into<Item>> From<T> for Items {
    fn from(id: T) -> Self {
        Items(vec![id.into()])
    }
}

impl Ctx {
    pub fn new(db: &dyn DefDatabase, file_id: FileId) -> Self {
        Ctx {
            tree: ItemTree::default(),
            ast_id_map: db.ast_id_map(file_id),
            file_id,
        }
    }

    pub fn lower_items(mut self, module: &ast::Module) -> ItemTree {
        self.tree.top_level = module
            .items()
            .flat_map(|item| self.lower_item(&item))
            .flat_map(|item| item.0)
            .collect();
        self.tree
    }

    fn lower_item(&mut self, item: &ast::Item) -> Option<Items> {
        let items = match item {
            | ast::Item::Import(ast) => Some(Items(self.lower_import(ast).into_iter().map(Into::into).collect())),
            | ast::Item::Type(ast) => self.lower_type(ast).map(Into::into),
            | _ => return None,
        };

        items
    }

    fn lower_import(&mut self, import_item: &ast::ItemImport) -> Vec<LocalItemTreeId<Import>> {
        let ast_id = self.ast_id_map.ast_id(import_item);
        let mut imports = Vec::new();

        ModPath::expand_import(
            InFile::new(self.file_id, import_item.clone()),
            |path, _, is_glob, alias| {
                imports.push(id(self.tree.data.imports.alloc(Import {
                    index: imports.len(),
                    ast_id,
                    is_glob,
                    path,
                    alias,
                })));
            },
        );

        imports
    }

    fn lower_type(&mut self, type_item: &ast::ItemType) -> Option<LocalItemTreeId<Type>> {
        let name = type_item.name()?.as_name();
        let ast_id = self.ast_id_map.ast_id(type_item);

        Some(id(self.tree.data.types.alloc(Type { ast_id, name })))
    }
}
