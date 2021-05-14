use crate::arena::Arena;
use crate::db::DefDatabase;
use crate::id::*;
pub use crate::item_tree::{Assoc, FunDep, Prec};
use crate::item_tree::{AssocItem, ItemTreeId};
use crate::name::Name;
use crate::path::Path;
use crate::type_ref::{Constraint, LocalTypeRefId, LocalTypeVarId, TypeMap, TypeRef, TypeSourceMap};
use base_db::input::FileId;
use std::sync::Arc;
use syntax::{ast, AstPtr};

#[derive(Debug, PartialEq, Eq)]
pub struct FixityData {
    pub name: Name,
    pub func: Path,
    pub prec: Prec,
    pub assoc: Assoc,
}

#[derive(Debug, PartialEq, Eq)]
pub struct FuncData {
    pub name: Name,
    pub ty: Option<LocalTypeRefId>,
    pub vars: Box<[LocalTypeVarId]>,
    pub constraints: Box<[Constraint]>,
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
}

#[derive(Debug, PartialEq, Eq)]
pub struct TypeAliasData {
    pub name: Name,
    pub vars: Box<[LocalTypeVarId]>,
    pub alias: LocalTypeRefId,
    type_map: TypeMap,
    type_source_map: TypeSourceMap,
}

#[derive(Debug, PartialEq, Eq)]
pub struct TypeCtorData {
    pub name: Name,
    pub kind: Option<LocalTypeRefId>,
    pub vars: Box<[LocalTypeVarId]>,
    pub ctors: Arena<CtorData>,
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
    pub vars: Box<[LocalTypeVarId]>,
    pub fundeps: Box<[FunDep]>,
    pub constraints: Box<[Constraint]>,
    pub items: Box<[(Name, AssocItemId)]>,
    type_map: TypeMap,
    type_source_map: TypeSourceMap,
}

#[derive(Debug, PartialEq, Eq)]
pub struct InstanceData {
    pub class: Path,
    pub vars: Box<[LocalTypeVarId]>,
    pub types: Box<[LocalTypeRefId]>,
    pub constraints: Box<[Constraint]>,
    pub items: Box<[AssocItemId]>,
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
            prec: it.prec,
            assoc: it.assoc,
        })
    }
}

impl FuncData {
    pub fn query(db: &dyn DefDatabase, id: FuncId) -> Arc<Self> {
        let loc = id.lookup(db);
        let item_tree = db.item_tree(loc.id.file_id);
        let it = &item_tree[loc.id.value];
        let src = loc.source(db);
        let mut type_builder = TypeMap::builder();
        let ty = src.value.ty().map(|t| type_builder.alloc_type_ref(t));
        let (vars, constraints) = if let Some(gen) = src.value.generics() {
            let vars = gen.vars().filter_map(|t| type_builder.alloc_type_var(t)).collect();
            let constraints = gen
                .constraints()
                .filter_map(|c| type_builder.lower_constraint(c))
                .collect();

            (vars, constraints)
        } else {
            (Box::new([]) as Box<[_]>, Box::new([]) as Box<[_]>)
        };

        let (type_map, type_source_map) = type_builder.finish();

        Arc::new(FuncData {
            name: it.name.clone(),
            has_body: it.has_body,
            is_foreign: it.is_foreign,
            ty,
            vars,
            constraints,
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

        Arc::new(ConstData { name: it.name.clone() })
    }
}

impl TypeAliasData {
    pub fn query(db: &dyn DefDatabase, id: TypeAliasId) -> Arc<Self> {
        let loc = id.lookup(db);
        let item_tree = db.item_tree(loc.id.file_id);
        let it = &item_tree[loc.id.value];
        let src = loc.source(db);
        let mut type_builder = TypeMap::builder();
        let vars = src
            .value
            .vars()
            .filter_map(|t| type_builder.alloc_type_var(t))
            .collect();

        let alias = type_builder.alloc_type_ref_opt(src.value.alias());
        let (type_map, type_source_map) = type_builder.finish();

        Arc::new(TypeAliasData {
            name: it.name.clone(),
            vars,
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
        let vars = src
            .value
            .vars()
            .filter_map(|t| type_builder.alloc_type_var(t))
            .collect();

        for ctor in src.value.ctors() {
            ctors.alloc(CtorData {
                name: ctor.name().unwrap().as_name(),
                types: ctor.types().map(|t| type_builder.alloc_type_ref(t)).collect(),
            });
        }

        let (type_map, type_source_map) = type_builder.finish();

        Arc::new(TypeCtorData {
            name: it.name.clone(),
            kind,
            vars,
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
        let vars = src
            .value
            .vars()
            .filter_map(|t| type_builder.alloc_type_var(t))
            .collect();

        let constraints = src
            .value
            .constraints()
            .filter_map(|c| type_builder.lower_constraint(c))
            .collect();

        let container = ContainerId::Class(id);
        let items = collect_assoc_items(db, loc.id.file_id, it.items.iter().copied(), container);
        let (type_map, type_source_map) = type_builder.finish();

        Arc::new(ClassData {
            name: it.name.clone(),
            fundeps: it.fundeps.clone(),
            items: items.into(),
            vars,
            constraints,
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

impl InstanceData {
    pub fn query(db: &dyn DefDatabase, id: InstanceId) -> Arc<Self> {
        use crate::resolver::HasResolver;
        let loc = id.lookup(db);
        let item_tree = db.item_tree(loc.id.file_id);
        let it = &item_tree[loc.id.value];
        let src = loc.source(db);
        let mut type_builder = TypeMap::builder();
        let types = src.value.types().map(|t| type_builder.alloc_type_ref(t)).collect();
        let resolver = loc.module.resolver(db);
        let vars = type_builder
            .iter()
            .map(|(id, ty)| (id, ty.clone()))
            .collect::<Vec<_>>()
            .into_iter()
            .filter_map(|(id, ty)| {
                if let TypeRef::Path(path) = ty {
                    if let None = resolver.resolve_type(db, &path) {
                        type_builder.alloc_type_var_from_ty(id, &path)
                    } else {
                        None
                    }
                } else {
                    None
                }
            })
            .collect();

        let constraints = src
            .value
            .constraints()
            .filter_map(|c| type_builder.lower_constraint(c))
            .collect();

        let container = ContainerId::Instance(id);
        let items = collect_assoc_items(db, loc.id.file_id, it.items.iter().copied(), container);
        let (type_map, type_source_map) = type_builder.finish();

        Arc::new(InstanceData {
            class: it.class.clone(),
            items: items.into_iter().map(|(_, it)| it).collect(),
            types,
            vars,
            constraints,
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
