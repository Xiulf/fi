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

    pub fn lower_modules(mut self, source_file: &ast::SourceFile) -> ItemTree {
        self.tree.top_level = source_file.modules().map(|module| self.lower_items(&module)).collect();
        self.tree
    }

    pub fn lower_items(&mut self, module: &ast::Module) -> Vec<Item> {
        let items = module
            .item_groups()
            .flat_map(|(item, rest)| self.lower_item(&item, &rest))
            .flat_map(|item| item.0)
            .collect::<Vec<_>>();

        let mut values = FxHashMap::<Name, Item>::default();
        let mut types = FxHashMap::<Name, Item>::default();
        let mut diagnostics = Vec::new();

        for &item in &items {
            let (name, is_type) = match item {
                | Item::Fixity(it) => (Some(&self.tree[it].name), false),
                | Item::Func(it) => (Some(&self.tree[it].name), false),
                | Item::Static(it) => (Some(&self.tree[it].name), false),
                | Item::Const(it) => (Some(&self.tree[it].name), false),
                | Item::TypeCtor(it) => (Some(&self.tree[it].name), true),
                | Item::TypeAlias(it) => (Some(&self.tree[it].name), true),
                | Item::Class(it) => (Some(&self.tree[it].name), true),
                | _ => (None, false),
            };

            if let Some(name) = name {
                let set = if is_type { &mut types } else { &mut values };

                if let Some(first_item) = set.get(name) {
                    diagnostics.push(ItemTreeDiagnostic::DuplicateDeclaration {
                        name: name.clone(),
                        first: *first_item,
                        second: item,
                    });
                } else {
                    set.insert(name.clone(), item);
                }
            }
        }

        self.tree.diagnostics.append(&mut diagnostics);
        items
    }

    fn lower_item(&mut self, item: &ast::Item, rest: &[ast::Item]) -> Option<Items> {
        let attrs = RawAttrs::new(item);
        let items = match item {
            | ast::Item::Import(ast) => Some(Items(self.lower_import(ast).into_iter().map(Into::into).collect())),
            | ast::Item::Fixity(ast) => self.lower_fixity(ast).map(Into::into),
            | ast::Item::Fun(ast) => self
                .lower_fun(
                    ast,
                    rest.iter().map(|it| match it {
                        | ast::Item::Fun(it) => it.clone(),
                        | _ => unreachable!(),
                    }),
                )
                .map(Into::into),
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

    fn lower_fun(
        &mut self,
        item: &ast::ItemFun,
        mut rest: impl Iterator<Item = ast::ItemFun>,
    ) -> Option<LocalItemTreeId<Func>> {
        let ast_id = self.ast_id_map.ast_id(item);
        let name = item.name()?.as_name();
        let has_body = item.body().is_some() || rest.any(|i| i.body().is_some());
        let is_foreign = item.is_foreign();

        Some(id(self.tree.data.funcs.alloc(Func {
            name,
            ast_id,
            has_body,
            is_foreign,
        })))
    }

    fn lower_static(&mut self, item: &ast::ItemStatic) -> Option<LocalItemTreeId<Static>> {
        let ast_id = self.ast_id_map.ast_id(item);
        let name = item.name()?.as_name();
        let is_foreign = item.is_foreign();

        Some(id(self.tree.data.statics.alloc(Static {
            name,
            ast_id,
            is_foreign,
        })))
    }

    fn lower_const(&mut self, item: &ast::ItemConst) -> Option<LocalItemTreeId<Const>> {
        let ast_id = self.ast_id_map.ast_id(item);
        let name = item.name()?.as_name();

        Some(id(self.tree.data.consts.alloc(Const { name, ast_id })))
    }

    fn lower_type(&mut self, item: &ast::ItemType) -> Option<Items> {
        let ast_id = self.ast_id_map.ast_id(item);
        let name = item.name()?.as_name();

        if let Some(_) = item.alias() {
            let talias = TypeAlias { ast_id, name };

            Some(id(self.tree.data.type_aliases.alloc(talias)).into())
        } else {
            let start = self.next_ctor_idx();

            for ctor in item.ctors() {
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
        let items = item
            .item_groups()
            .filter_map(|(item, group)| self.lower_assoc_item(item, group))
            .collect();

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
        let items = item
            .item_groups()
            .filter_map(|(item, group)| self.lower_assoc_item(item, group))
            .collect();

        Some(id(self.tree.data.members.alloc(Member { ast_id, class, items })))
    }

    fn lower_assoc_item(&mut self, item: ast::AssocItem, group: Vec<ast::AssocItem>) -> Option<AssocItem> {
        match item {
            | ast::AssocItem::Fun(it) => self
                .lower_fun(
                    &it,
                    group.into_iter().map(|it| match it {
                        | ast::AssocItem::Fun(it) => it,
                        | _ => unreachable!(),
                    }),
                )
                .map(AssocItem::Func),
            | ast::AssocItem::Static(it) => self.lower_static(&it).map(AssocItem::Static),
        }
    }

    fn next_ctor_idx(&self) -> Idx<Ctor> {
        Idx::from_raw(RawIdx::from(self.tree.data.ctors.len() as u32))
    }
}
