use std::marker::PhantomData;
use std::sync::Arc;

use arena::{Idx, RawIdx};
use base_db::input::FileId;
use syntax::ast::{self, NameOwner};

use crate::ast_id::AstIdMap;
use crate::db::DefDatabase;
use crate::in_file::InFile;
use crate::item_tree::*;
use crate::name::AsName;
use crate::path::Path;

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

#[derive(Debug)]
struct Items(Vec<Item>);

impl<T: Into<Item>> From<T> for Items {
    fn from(id: T) -> Self {
        Items(vec![id.into()])
    }
}

impl Ctx {
    pub fn new(db: &dyn DefDatabase, file_id: FileId) -> Self {
        Ctx {
            tree: ItemTree::new(file_id),
            ast_id_map: db.ast_id_map(file_id),
            file_id,
        }
    }

    pub fn lower_source_file(mut self, source_file: &ast::SourceFile) -> ItemTree {
        self.tree.top_level = source_file
            .module()
            .map(|m| self.lower_items(m.items()))
            .unwrap_or_default();
        self.tree
    }

    pub fn lower_items(&mut self, items: ast::AstChildren<ast::Item>) -> Vec<Item> {
        items
            .flat_map(|item| self.lower_item(&item))
            .flat_map(|item| item.0)
            .collect::<Vec<_>>()
    }

    fn lower_item(&mut self, item: &ast::Item) -> Option<Items> {
        let attrs = RawAttrs::new(item);
        let items = match item {
            | ast::Item::Module(ast) => self.lower_module(ast).map(Into::into),
            | ast::Item::Import(ast) => Some(Items(self.lower_import(ast).into_iter().map(Into::into).collect())),
            | ast::Item::Fixity(ast) => self.lower_fixity(ast).map(Into::into),
            | ast::Item::Func(ast) => self.lower_func(ast).map(Into::into),
            | ast::Item::Static(ast) => self.lower_static(ast).map(Into::into),
            | ast::Item::Const(ast) => self.lower_const(ast).map(Into::into),
            | ast::Item::Type(ast) => self.lower_type(ast),
            | ast::Item::Class(ast) => self.lower_class(ast).map(Into::into),
            | ast::Item::Member(ast) => self.lower_member(ast).map(Into::into),
        };

        if !attrs.is_empty() {
            for item in items.iter().flat_map(|items| &items.0) {
                self.tree.attrs.insert((*item).into(), attrs.clone());
            }
        }

        items
    }

    pub fn lower_module(&mut self, module: &ast::ItemModule) -> Option<LocalItemTreeId<Module>> {
        let ast_id = self.ast_id_map.ast_id(module);
        let name = module.name()?.as_name();
        let items = self.lower_items(module.items()).into_boxed_slice();

        Some(id(self.tree.data.modules.alloc(Module { ast_id, name, items })))
    }

    fn lower_import(&mut self, import_item: &ast::ItemImport) -> Vec<LocalItemTreeId<Import>> {
        let ast_id = self.ast_id_map.ast_id(import_item);
        let mut imports = Vec::new();

        Path::expand_import(
            InFile::new(self.file_id, import_item.clone()),
            |path, _, is_glob, alias, qualify| {
                imports.push(id(self.tree.data.imports.alloc(Import {
                    index: imports.len(),
                    ast_id,
                    is_glob,
                    path,
                    alias,
                    qualify,
                })));
            },
        );

        imports
    }

    fn lower_fixity(&mut self, item: &ast::ItemFixity) -> Option<LocalItemTreeId<Fixity>> {
        let ast_id = self.ast_id_map.ast_id(item);
        let name = item.name()?.as_name();
        let func = Path::lower(item.func()?);
        let kind = if item.is_postfix() {
            FixityKind::Postfix
        } else if item.is_prefix() {
            FixityKind::Prefix
        } else {
            FixityKind::Infix {
                prec: item.prec()?,
                assoc: item.assoc()?,
            }
        };

        Some(id(self.tree.data.fixities.alloc(Fixity {
            ast_id,
            name,
            func,
            kind,
        })))
    }

    fn lower_func(&mut self, item: &ast::ItemFunc) -> Option<LocalItemTreeId<Func>> {
        let mut iter = item.iter();
        let first = iter.next()?;
        let ast_id = self.ast_id_map.ast_id(item);
        let name = first.name()?.as_name();
        let has_body = first.body().is_some()
            || first.guarded().is_some()
            || iter.any(|i| i.body().is_some() || i.guarded().is_some());
        let is_foreign = item.is_foreign();

        Some(id(self.tree.data.funcs.alloc(Func {
            name,
            ast_id,
            has_body,
            is_foreign,
        })))
    }

    fn lower_static(&mut self, item: &ast::ItemStatic) -> Option<LocalItemTreeId<Static>> {
        let mut iter = item.iter();
        let first = iter.next()?;
        let ast_id = self.ast_id_map.ast_id(item);
        let name = first.name()?.as_name();
        let is_foreign = item.is_foreign();

        Some(id(self.tree.data.statics.alloc(Static {
            name,
            ast_id,
            is_foreign,
        })))
    }

    fn lower_const(&mut self, item: &ast::ItemConst) -> Option<LocalItemTreeId<Const>> {
        let mut iter = item.iter();
        let first = iter.next()?;
        let ast_id = self.ast_id_map.ast_id(item);
        let name = first.name()?.as_name();

        Some(id(self.tree.data.consts.alloc(Const { name, ast_id })))
    }

    fn lower_type(&mut self, item: &ast::ItemType) -> Option<Items> {
        let mut iter = item.iter();
        let first = iter.next()?;
        let next = iter.next();
        let ast_id = self.ast_id_map.ast_id(item);
        let name = first.name()?.as_name();

        if let Some(_) = first.alias().or_else(|| next.as_ref()?.alias()) {
            let talias = TypeAlias { ast_id, name };

            Some(id(self.tree.data.type_aliases.alloc(talias)).into())
        } else {
            let start = self.next_ctor_idx();
            let ctors = next.as_ref().map(|it| it.ctors()).unwrap_or_else(|| first.ctors());

            for ctor in ctors {
                self.lower_ctor(&ctor);
            }

            let end = self.next_ctor_idx();
            let ctors = IdRange::new(start..end);
            let is_foreign = item.is_foreign();
            let tctor = TypeCtor {
                ast_id,
                name,
                ctors,
                is_foreign,
            };

            Some(id(self.tree.data.type_ctors.alloc(tctor)).into())
        }
    }

    fn lower_ctor(&mut self, ctor: &ast::Ctor) -> Option<Idx<Ctor>> {
        let ast_id = self.ast_id_map.ast_id(ctor);
        let name = ctor.name()?.as_name();
        let res = Ctor { ast_id, name };

        Some(self.tree.data.ctors.alloc(res))
    }

    fn lower_class(&mut self, item: &ast::ItemClass) -> Option<LocalItemTreeId<Class>> {
        let ast_id = self.ast_id_map.ast_id(item);
        let name = item.name()?.as_name();
        let fundeps = item.fundeps().filter_map(|f| self.lower_fun_dep(f)).collect();
        let items = item.items().filter_map(|item| self.lower_assoc_item(item)).collect();

        Some(id(self.tree.data.classes.alloc(Class {
            name,
            ast_id,
            fundeps,
            items,
        })))
    }

    fn lower_fun_dep(&mut self, fundep: ast::FunDep) -> Option<FunDep> {
        let determiners = fundep.determiners().map(|n| n.as_name()).collect::<Box<[_]>>();
        let determined = fundep.determined().map(|n| n.as_name()).collect::<Box<[_]>>();

        if determiners.is_empty() && determined.is_empty() {
            return None;
        }

        Some(FunDep {
            determiners,
            determined,
        })
    }

    fn lower_member(&mut self, item: &ast::ItemMember) -> Option<LocalItemTreeId<Member>> {
        let ast_id = self.ast_id_map.ast_id(item);
        let class = Path::lower(item.class()?);
        let items = item.items().filter_map(|item| self.lower_assoc_item(item)).collect();

        Some(id(self.tree.data.members.alloc(Member { ast_id, class, items })))
    }

    fn lower_assoc_item(&mut self, item: ast::AssocItem) -> Option<AssocItem> {
        match item {
            | ast::AssocItem::Func(it) => self.lower_func(&it).map(AssocItem::Func),
            | ast::AssocItem::Static(it) => self.lower_static(&it).map(AssocItem::Static),
        }
    }

    fn next_ctor_idx(&self) -> Idx<Ctor> {
        Idx::from_raw(RawIdx::from(self.tree.data.ctors.len() as u32))
    }
}
