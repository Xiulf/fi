use crate::arena::{Idx, RawIdx};
use crate::ast_id::AstIdMap;
use crate::db::DefDatabase;
use crate::id::Intern;
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

pub(super) struct Ctx<'db> {
    db: &'db dyn DefDatabase,
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

impl<'db> Ctx<'db> {
    pub fn new(db: &'db dyn DefDatabase, file_id: FileId) -> Self {
        Ctx {
            tree: ItemTree::default(),
            ast_id_map: db.ast_id_map(file_id),
            file_id,
            db,
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
            | ast::Item::Fun(ast) => self.lower_fun(ast).map(Into::into),
            | ast::Item::Static(ast) => self.lower_static(ast).map(Into::into),
            | ast::Item::Const(ast) => self.lower_const(ast).map(Into::into),
            | ast::Item::Type(ast) => self.lower_type(ast),
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

    fn lower_fun(&mut self, item: &ast::ItemFun) -> Option<LocalItemTreeId<Func>> {
        let ast_id = self.ast_id_map.ast_id(item);
        let name = item.name()?.as_name();
        let ty = item.ty().map(|t| self.lower_ty(t));
        let has_body = item.body().is_some();
        let is_foreign = item.is_foreign();
        let (vars, constraints) = if let Some(gen) = item.generics() {
            (
                gen.vars().filter_map(|v| self.lower_ty_var(v)).collect::<Box<[_]>>(),
                gen.constraints()
                    .filter_map(|c| self.lower_constraint(c))
                    .collect::<Box<[_]>>(),
            )
        } else {
            (Box::new([]) as Box<[_]>, Box::new([]) as Box<[_]>)
        };

        Some(id(self.tree.data.funcs.alloc(Func {
            name,
            ast_id,
            ty,
            has_body,
            is_foreign,
            vars,
            constraints,
        })))
    }

    fn lower_static(&mut self, item: &ast::ItemStatic) -> Option<LocalItemTreeId<Static>> {
        let ast_id = self.ast_id_map.ast_id(item);
        let name = item.name()?.as_name();
        let ty = item.ty().map(|t| self.lower_ty(t));
        let is_foreign = item.is_foreign();

        Some(id(self.tree.data.statics.alloc(Static {
            name,
            ast_id,
            ty,
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
        let kind = item.kind().map(|ty| self.lower_ty(ty));
        let vars = item.vars().filter_map(|t| self.lower_ty_var(t)).collect();

        if let Some(alias) = item.alias() {
            let alias = self.lower_ty(alias);
            let talias = TypeAlias {
                ast_id,
                name,
                alias,
                vars,
            };

            Some(id(self.tree.data.type_aliases.alloc(talias)).into())
        } else {
            let start = self.next_ctor_idx();

            for ctor in item.ctors() {
                self.lower_ctor(&ctor);
            }

            let end = self.next_ctor_idx();
            let ctors = IdRange::new(start..end);
            let tctor = TypeCtor {
                ast_id,
                name,
                ctors,
                kind,
                vars,
            };

            Some(id(self.tree.data.type_ctors.alloc(tctor)).into())
        }
    }

    fn lower_ctor(&mut self, ctor: &ast::Ctor) -> Option<Idx<Ctor>> {
        let name = ctor.name()?.as_name();
        let types = ctor.types().map(|t| self.lower_ty(t)).collect();
        let res = Ctor { name, types };

        Some(self.tree.data.ctors.alloc(res))
    }

    fn lower_class(&mut self, item: &ast::ItemClass) -> Option<LocalItemTreeId<Class>> {
        let ast_id = self.ast_id_map.ast_id(item);
        let name = item.name()?.as_name();
        let fundeps = item.fundeps().filter_map(|f| self.lower_fun_dep(f)).collect();
        let constraints = item.constraints().filter_map(|c| self.lower_constraint(c)).collect();
        let items = item.items().filter_map(|item| self.lower_assoc_item(item)).collect();

        Some(id(self.tree.data.classes.alloc(Class {
            name,
            ast_id,
            fundeps,
            constraints,
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
        let constraints = item.constraints().filter_map(|c| self.lower_constraint(c)).collect();
        let items = item.items().filter_map(|item| self.lower_assoc_item(item)).collect();

        Some(id(self.tree.data.instances.alloc(Instance {
            ast_id,
            class,
            types,
            constraints,
            items,
            // index,
            // chain: Box::new([]),
        })))
    }

    fn lower_assoc_item(&mut self, item: ast::AssocItem) -> Option<AssocItem> {
        match item {
            | ast::AssocItem::Fun(it) => self.lower_fun(&it).map(AssocItem::Func),
            | ast::AssocItem::Static(it) => self.lower_static(&it).map(AssocItem::Static),
        }
    }

    fn lower_ty(&mut self, ty: ast::Type) -> TypeRefId {
        TypeRef::from_ast(ty, self.db)
    }

    fn lower_ty_var(&mut self, tv: ast::TypeVar) -> Option<TypeVar> {
        let name = tv.name()?.as_name();
        let kind = tv.kind().map(|k| TypeRef::from_ast(k, self.db));

        Some(TypeVar { name, kind })
    }

    fn lower_constraint(&mut self, ctnt: ast::Constraint) -> Option<Constraint> {
        let class = Path::lower(ctnt.class()?);
        let types = ctnt.types().map(|t| TypeRef::from_ast(t, self.db)).collect();

        Some(Constraint { class, types })
    }

    fn next_ctor_idx(&self) -> Idx<Ctor> {
        Idx::from_raw(RawIdx::from(self.tree.data.ctors.len() as u32))
    }
}
