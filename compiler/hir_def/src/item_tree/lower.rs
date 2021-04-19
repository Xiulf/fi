use crate::arena::{Idx, RawIdx};
use crate::ast_id::AstIdMap;
use crate::db::DefDatabase;
use crate::in_file::InFile;
use crate::item_tree::*;
use crate::name::AsName;
use crate::path::Path;
use crate::type_ref::TypeRef;
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
            | ast::Item::Fixity(ast) => self.lower_fixity(ast).map(Into::into),
            | ast::Item::Foreign(ast) => self.lower_foreign(ast).map(Into::into),
            | ast::Item::Fun(ast) => self.lower_fun(ast).map(Into::into),
            | ast::Item::Static(ast) => self.lower_static(ast).map(Into::into),
            | ast::Item::Const(ast) => self.lower_const(ast).map(Into::into),
            | ast::Item::Type(ast) => self.lower_type(ast).map(Into::into),
            | ast::Item::Class(ast) => self.lower_class(ast).map(Into::into),
            // | ast::Item::Instance(ast) => self.lower_instance_chain(ast).map(Into::into),
            | ast::Item::Instance(ast) => self.lower_instance(ast).map(Into::into),
            | _ => return None,
        };

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
        let prec = item.prec()?;
        let assoc = item.assoc()?;

        Some(id(self.tree.data.fixities.alloc(Fixity {
            ast_id,
            name,
            func,
            prec,
            assoc,
        })))
    }

    fn lower_foreign(&mut self, item: &ast::ItemForeign) -> Option<LocalItemTreeId<Foreign>> {
        let ast_id = self.ast_id_map.ast_id(item);
        let name = item.name()?.as_name();
        let ty = TypeRef::from_ast(item.ty()?);
        let ty = self.tree.data.type_refs.intern(ty);
        let kind = match item.kind()? {
            | ast::ForeignKind::Fun => ForeignKind::Fun,
            | ast::ForeignKind::Static => ForeignKind::Static,
        };

        Some(id(self.tree.data.foreigns.alloc(Foreign { ast_id, name, kind, ty })))
    }

    fn lower_fun(&mut self, item: &ast::ItemFun) -> Option<LocalItemTreeId<Func>> {
        let ast_id = self.ast_id_map.ast_id(item);
        let name = item.name()?.as_name();

        Some(id(self.tree.data.funcs.alloc(Func { name, ast_id })))
    }

    fn lower_static(&mut self, item: &ast::ItemStatic) -> Option<LocalItemTreeId<Static>> {
        let ast_id = self.ast_id_map.ast_id(item);
        let name = item.name()?.as_name();

        Some(id(self.tree.data.statics.alloc(Static { name, ast_id })))
    }

    fn lower_const(&mut self, item: &ast::ItemConst) -> Option<LocalItemTreeId<Const>> {
        let ast_id = self.ast_id_map.ast_id(item);
        let name = item.name()?.as_name();

        Some(id(self.tree.data.consts.alloc(Const { name, ast_id })))
    }

    fn lower_type(&mut self, item: &ast::ItemType) -> Option<LocalItemTreeId<Type>> {
        let ast_id = self.ast_id_map.ast_id(item);
        let name = item.name()?.as_name();
        let kind = item.kind().map(|ty| self.lower_ty(ty));
        let (alias, ctors) = if let Some(alias) = item.alias() {
            (
                Some(self.lower_ty(alias)),
                IdRange::new(self.next_ctor_idx()..self.next_ctor_idx()),
            )
        } else {
            let start = self.next_ctor_idx();

            for ctor in item.ctors() {
                self.lower_ctor(&ctor);
            }

            let end = self.next_ctor_idx();

            (None, IdRange::new(start..end))
        };

        Some(id(self.tree.data.types.alloc(Type {
            ast_id,
            name,
            ctors,
            alias,
            kind,
        })))
    }

    fn lower_ctor(&mut self, ctor: &ast::Ctor) -> Option<Idx<Ctor>> {
        let name = ctor.name()?.as_name();
        let types = Vec::new();
        let res = Ctor { name, types };

        Some(self.tree.data.ctors.alloc(res))
    }

    fn lower_class(&mut self, item: &ast::ItemClass) -> Option<LocalItemTreeId<Class>> {
        let ast_id = self.ast_id_map.ast_id(item);
        let name = item.name()?.as_name();

        Some(id(self.tree.data.classes.alloc(Class { name, ast_id })))
    }

    // fn lower_instance_chain(&mut self, item: &ast::ItemInstanceChain) -> Option<LocalItemTreeId<InstanceChain>> {
    //     let ast_id = self.ast_id_map.ast_id(item);
    //     let chain = item
    //         .instances()
    //         .enumerate()
    //         .filter_map(|(i, inst)| self.lower_instance(i, inst))
    //         .collect();
    //
    //     let id = id(self.tree.data.instance_chains.alloc(InstanceChain {
    //         ast_id,
    //         chain: chain.clone(),
    //     }));
    //
    //     for id in chain {
    //         self.tree.data.instances[id].chain = id;
    //     }
    //
    //     Some(id)
    // }

    fn lower_instance(&mut self, /*index: usize,*/ item: &ast::ItemInstance) -> Option<LocalItemTreeId<Instance>> {
        let ast_id = self.ast_id_map.ast_id(item);
        let class = Path::lower(item.class()?);
        let types = item.types().map(|t| self.lower_ty(t)).collect();

        Some(id(self.tree.data.instances.alloc(Instance {
            ast_id,
            class,
            types,
            // index,
            // chain: Box::new([]),
        })))
    }

    fn lower_ty(&mut self, ty: ast::Type) -> Idx<TypeRef> {
        let ty = TypeRef::from_ast(ty);

        self.tree.data.type_refs.intern(ty)
    }

    fn next_ctor_idx(&self) -> Idx<Ctor> {
        Idx::from_raw(RawIdx::from(self.tree.data.ctors.len() as u32))
    }
}
