use crate::db::DefDatabase;
use crate::id::*;
use crate::item_tree::{AssocItem, ItemTreeId};
pub use crate::item_tree::{FixityKind, FunDep};
use crate::name::Name;
use crate::path::Path;
use crate::resolver::{HasResolver, Resolver};
use crate::type_ref::{LocalTypeRefId, LocalTypeVarId, TypeMap, TypeMapBuilder, TypeRef, TypeSourceMap, WhereClause};
use arena::Arena;
use base_db::input::FileId;
use rustc_hash::FxHashSet;
use std::sync::Arc;
use syntax::ast;

#[derive(Debug, PartialEq, Eq)]
pub struct FixityData {
    pub name: Name,
    pub func: Path,
    pub kind: FixityKind,
}

#[derive(Debug, PartialEq, Eq)]
pub struct FuncData {
    pub name: Name,
    pub ty: Option<LocalTypeRefId>,
    pub type_vars: Box<[LocalTypeVarId]>,
    pub has_body: bool,
    pub is_foreign: bool,
    type_map: TypeMap,
    type_source_map: TypeSourceMap,
}

#[derive(Debug, PartialEq, Eq)]
pub struct StaticData {
    pub name: Name,
    pub ty: Option<LocalTypeRefId>,
    pub is_foreign: bool,
    type_map: TypeMap,
    type_source_map: TypeSourceMap,
}

#[derive(Debug, PartialEq, Eq)]
pub struct ConstData {
    pub name: Name,
    pub ty: Option<LocalTypeRefId>,
    type_map: TypeMap,
    type_source_map: TypeSourceMap,
}

#[derive(Debug, PartialEq, Eq)]
pub struct TypeAliasData {
    pub name: Name,
    pub type_vars: Box<[LocalTypeVarId]>,
    pub alias: LocalTypeRefId,
    type_map: TypeMap,
    type_source_map: TypeSourceMap,
}

#[derive(Debug, PartialEq, Eq)]
pub struct TypeCtorData {
    pub name: Name,
    pub kind: Option<LocalTypeRefId>,
    pub type_vars: Box<[LocalTypeVarId]>,
    pub ctors: Arena<CtorData>,
    pub is_foreign: bool,
    type_map: TypeMap,
    type_source_map: TypeSourceMap,
}

#[derive(Debug, PartialEq, Eq)]
pub struct CtorData {
    pub name: Name,
    pub types: Box<[LocalTypeRefId]>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct ClassData {
    pub name: Name,
    pub type_vars: Box<[LocalTypeVarId]>,
    pub fundeps: Box<[FunDep]>,
    pub where_clause: WhereClause,
    pub items: Box<[(Name, AssocItemId)]>,
    type_map: TypeMap,
    type_source_map: TypeSourceMap,
}

#[derive(Debug, PartialEq, Eq)]
pub struct MemberData {
    pub class: Path,
    pub type_vars: Box<[LocalTypeVarId]>,
    pub types: Box<[LocalTypeRefId]>,
    pub where_clause: WhereClause,
    pub items: Box<[(Name, AssocItemId)]>,
    type_map: TypeMap,
    type_source_map: TypeSourceMap,
}

impl FixityData {
    pub fn query(db: &dyn DefDatabase, id: FixityId) -> Arc<Self> {
        let loc = id.lookup(db);
        let item_tree = db.item_tree(loc.id.file_id);
        let it = &item_tree[loc.id.value];

        Arc::new(FixityData {
            name: it.name.clone(),
            func: it.func.clone(),
            kind: it.kind.clone(),
        })
    }
}

fn register_type_vars(
    db: &dyn DefDatabase,
    resolver: Resolver,
    type_builder: &mut TypeMapBuilder,
) -> Box<[LocalTypeVarId]> {
    let types = type_builder.iter().map(|(id, t)| (id, t.clone())).collect::<Vec<_>>();
    let mut defined = FxHashSet::default();
    let mut vars = Vec::new();

    for &(id, ref ty) in &types {
        match ty {
            | TypeRef::Path(path) if path.segments().len() == 1 => {
                let name = path.as_ident().unwrap();

                if name.is_lowercase() && !defined.contains(name) && resolver.resolve_type(db, path).is_none() {
                    let var = type_builder.alloc_type_var_from_ty(name.clone(), id);

                    vars.push(var);
                    defined.insert(name);
                }
            },
            | _ => {},
        }
    }

    vars.into_boxed_slice()
}

impl FuncData {
    pub fn query(db: &dyn DefDatabase, id: FuncId) -> Arc<Self> {
        let loc = id.lookup(db);
        let item_tree = db.item_tree(loc.id.file_id);
        let it = &item_tree[loc.id.value];
        let src = loc.source(db);
        let mut type_builder = TypeMap::builder();
        let ty = src.value.ty().map(|t| type_builder.alloc_type_ref(t));
        let type_vars = register_type_vars(db, loc.container.resolver(db), &mut type_builder);
        let (type_map, type_source_map) = type_builder.finish();

        Arc::new(FuncData {
            name: it.name.clone(),
            has_body: it.has_body,
            is_foreign: it.is_foreign,
            ty,
            type_vars,
            type_map,
            type_source_map,
        })
    }

    pub fn type_map(&self) -> &TypeMap {
        &self.type_map
    }

    pub fn type_source_map(&self) -> &TypeSourceMap {
        &self.type_source_map
    }
}

impl StaticData {
    pub fn query(db: &dyn DefDatabase, id: StaticId) -> Arc<Self> {
        let loc = id.lookup(db);
        let item_tree = db.item_tree(loc.id.file_id);
        let it = &item_tree[loc.id.value];
        let src = loc.source(db);
        let mut type_builder = TypeMap::builder();
        let ty = src.value.ty().map(|t| type_builder.alloc_type_ref(t));
        let (type_map, type_source_map) = type_builder.finish();

        Arc::new(StaticData {
            name: it.name.clone(),
            is_foreign: it.is_foreign,
            ty,
            type_map,
            type_source_map,
        })
    }

    pub fn type_map(&self) -> &TypeMap {
        &self.type_map
    }

    pub fn type_source_map(&self) -> &TypeSourceMap {
        &self.type_source_map
    }
}

impl ConstData {
    pub fn query(db: &dyn DefDatabase, id: ConstId) -> Arc<Self> {
        let loc = id.lookup(db);
        let item_tree = db.item_tree(loc.id.file_id);
        let it = &item_tree[loc.id.value];
        let src = loc.source(db);
        let mut type_builder = TypeMap::builder();
        let ty = src.value.ty().map(|t| type_builder.alloc_type_ref(t));
        let (type_map, type_source_map) = type_builder.finish();

        Arc::new(ConstData {
            name: it.name.clone(),
            ty,
            type_map,
            type_source_map,
        })
    }

    pub fn type_map(&self) -> &TypeMap {
        &self.type_map
    }

    pub fn type_source_map(&self) -> &TypeSourceMap {
        &self.type_source_map
    }
}

impl TypeAliasData {
    pub fn query(db: &dyn DefDatabase, id: TypeAliasId) -> Arc<Self> {
        let loc = id.lookup(db);
        let item_tree = db.item_tree(loc.id.file_id);
        let it = &item_tree[loc.id.value];
        let src = loc.source(db);
        let mut type_builder = TypeMap::builder();
        let type_vars = match src.value.vars() {
            | Some(vars) => vars
                .type_vars()
                .map(|t| type_builder.alloc_type_var(t))
                .collect::<Box<[LocalTypeVarId]>>(),
            | None => Box::new([]),
        };

        let alias = type_builder.alloc_type_ref_opt(src.value.alias());
        let (type_map, type_source_map) = type_builder.finish();

        Arc::new(TypeAliasData {
            name: it.name.clone(),
            type_vars,
            alias,
            type_map,
            type_source_map,
        })
    }

    pub fn type_map(&self) -> &TypeMap {
        &self.type_map
    }

    pub fn type_source_map(&self) -> &TypeSourceMap {
        &self.type_source_map
    }
}

impl TypeCtorData {
    pub fn query(db: &dyn DefDatabase, id: TypeCtorId) -> Arc<Self> {
        use crate::name::AsName;
        use ast::NameOwner;
        let loc = id.lookup(db);
        let item_tree = db.item_tree(loc.id.file_id);
        let it = &item_tree[loc.id.value];
        let src = loc.source(db);
        let mut type_builder = TypeMap::builder();
        let mut ctors = Arena::new();
        let kind = src.value.kind().map(|t| type_builder.alloc_type_ref(t));
        let type_vars = match src.value.vars() {
            | Some(vars) => vars
                .type_vars()
                .map(|t| type_builder.alloc_type_var(t))
                .collect::<Box<[LocalTypeVarId]>>(),
            | None => Box::new([]),
        };

        for ctor in src.value.ctors() {
            ctors.alloc(CtorData {
                name: ctor.name().unwrap().as_name(),
                types: ctor.types().map(|t| type_builder.alloc_type_ref(t)).collect(),
            });
        }

        let (type_map, type_source_map) = type_builder.finish();

        Arc::new(TypeCtorData {
            name: it.name.clone(),
            is_foreign: it.is_foreign,
            kind,
            type_vars,
            ctors,
            type_map,
            type_source_map,
        })
    }

    pub fn ctor(&self, name: &Name) -> Option<LocalCtorId> {
        let (id, _) = self.ctors.iter().find(|(_, data)| &data.name == name)?;
        Some(id)
    }

    pub fn type_map(&self) -> &TypeMap {
        &self.type_map
    }

    pub fn type_source_map(&self) -> &TypeSourceMap {
        &self.type_source_map
    }
}

impl ClassData {
    pub fn query(db: &dyn DefDatabase, id: ClassId) -> Arc<Self> {
        let loc = id.lookup(db);
        let item_tree = db.item_tree(loc.id.file_id);
        let it = &item_tree[loc.id.value];
        let src = loc.source(db);
        let mut type_builder = TypeMap::builder();
        let type_vars = match src.value.vars() {
            | Some(vars) => vars
                .type_vars()
                .map(|t| type_builder.alloc_type_var(t))
                .collect::<Box<[LocalTypeVarId]>>(),
            | None => Box::new([]),
        };

        let where_clause = type_builder.lower_where_clause(src.value.where_clause());
        let container = ContainerId::Class(id);
        let items = collect_assoc_items(db, loc.id.file_id, it.items.iter().copied(), container);
        let (type_map, type_source_map) = type_builder.finish();

        Arc::new(ClassData {
            name: it.name.clone(),
            fundeps: it.fundeps.clone(),
            items: items.into(),
            where_clause,
            type_vars,
            type_map,
            type_source_map,
        })
    }

    pub fn type_map(&self) -> &TypeMap {
        &self.type_map
    }

    pub fn type_source_map(&self) -> &TypeSourceMap {
        &self.type_source_map
    }

    pub fn item(&self, name: &Name) -> Option<AssocItemId> {
        self.items
            .iter()
            .find_map(|(n, id)| if n == name { Some(*id) } else { None })
    }
}

impl MemberData {
    pub fn query(db: &dyn DefDatabase, id: MemberId) -> Arc<Self> {
        let loc = id.lookup(db);
        let item_tree = db.item_tree(loc.id.file_id);
        let it = &item_tree[loc.id.value];
        let src = loc.source(db);
        let mut type_builder = TypeMap::builder();
        let types = src.value.types().map(|t| type_builder.alloc_type_ref(t)).collect();
        let type_vars = register_type_vars(db, loc.module.resolver(db), &mut type_builder);
        let where_clause = type_builder.lower_where_clause(src.value.where_clause());
        let container = ContainerId::Member(id);
        let items = collect_assoc_items(db, loc.id.file_id, it.items.iter().copied(), container);
        let (type_map, type_source_map) = type_builder.finish();

        Arc::new(MemberData {
            class: it.class.clone(),
            items: items.into(),
            where_clause,
            types,
            type_vars,
            type_map,
            type_source_map,
        })
    }

    pub fn type_map(&self) -> &TypeMap {
        &self.type_map
    }

    pub fn type_source_map(&self) -> &TypeSourceMap {
        &self.type_source_map
    }

    pub fn item(&self, name: &Name) -> Option<AssocItemId> {
        self.items
            .iter()
            .find_map(|(n, id)| if n == name { Some(*id) } else { None })
    }
}

fn collect_assoc_items(
    db: &dyn DefDatabase,
    file_id: FileId,
    assoc_items: impl Iterator<Item = AssocItem>,
    container: ContainerId,
) -> Vec<(Name, AssocItemId)> {
    let item_tree = db.item_tree(file_id);
    let mut items = Vec::new();

    for item in assoc_items {
        match item {
            | AssocItem::Func(id) => {
                let it = &item_tree[id];
                let def = FuncLoc {
                    container,
                    id: ItemTreeId::new(file_id, id),
                }
                .intern(db);

                items.push((it.name.clone(), AssocItemId::FuncId(def)));
            },
            | AssocItem::Static(id) => {
                let it = &item_tree[id];
                let def = StaticLoc {
                    container,
                    id: ItemTreeId::new(file_id, id),
                }
                .intern(db);

                items.push((it.name.clone(), AssocItemId::StaticId(def)));
            },
        }
    }

    items
}
