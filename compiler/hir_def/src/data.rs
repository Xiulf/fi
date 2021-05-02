use crate::arena::Arena;
use crate::db::DefDatabase;
use crate::id::*;
pub use crate::item_tree::{Assoc, FunDep, Prec};
use crate::item_tree::{AssocItem, ItemTreeId};
use crate::name::Name;
use crate::path::Path;
use crate::type_ref::{Constraint, TypeRefId};
use base_db::input::FileId;
use std::sync::Arc;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FixityData {
    pub name: Name,
    pub func: Path,
    pub prec: Prec,
    pub assoc: Assoc,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FuncData {
    pub name: Name,
    pub ty: Option<TypeRefId>,
    pub has_body: bool,
    pub is_foreign: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StaticData {
    pub name: Name,
    pub ty: Option<TypeRefId>,
    pub is_foreign: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ConstData {
    pub name: Name,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeAliasData {
    pub name: Name,
    pub alias: TypeRefId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeCtorData {
    pub name: Name,
    pub ctors: Arena<CtorData>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CtorData {
    pub name: Name,
    pub types: Box<[TypeRefId]>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ClassData {
    pub name: Name,
    pub fundeps: Box<[FunDep]>,
    pub constraints: Box<[Constraint]>,
    pub items: Box<[(Name, AssocItemId)]>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InstanceData {
    pub class: Path,
    pub types: Box<[TypeRefId]>,
    pub constraints: Box<[Constraint]>,
    pub items: Box<[AssocItemId]>,
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

        Arc::new(FuncData {
            name: it.name.clone(),
            ty: it.ty,
            has_body: it.has_body,
            is_foreign: it.is_foreign,
        })
    }
}

impl StaticData {
    pub fn query(db: &dyn DefDatabase, id: StaticId) -> Arc<Self> {
        let loc = id.lookup(db);
        let item_tree = db.item_tree(loc.id.file_id);
        let it = &item_tree[loc.id.value];

        Arc::new(StaticData {
            name: it.name.clone(),
            ty: it.ty,
            is_foreign: it.is_foreign,
        })
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

        Arc::new(TypeAliasData {
            name: it.name.clone(),
            alias: it.alias,
        })
    }
}

impl TypeCtorData {
    pub fn query(db: &dyn DefDatabase, id: TypeCtorId) -> Arc<Self> {
        let loc = id.lookup(db);
        let item_tree = db.item_tree(loc.id.file_id);
        let it = &item_tree[loc.id.value];
        let mut ctors = Arena::new();

        for id in it.ctors.clone() {
            let ctor = &item_tree[id];

            ctors.alloc(CtorData {
                name: ctor.name.clone(),
                types: ctor.types.clone(),
            });
        }

        Arc::new(TypeCtorData {
            name: it.name.clone(),
            ctors,
        })
    }

    pub fn ctor(&self, name: &Name) -> Option<LocalCtorId> {
        let (id, _) = self.ctors.iter().find(|(_, data)| &data.name == name)?;
        Some(id)
    }
}

impl ClassData {
    pub fn query(db: &dyn DefDatabase, id: ClassId) -> Arc<Self> {
        let loc = id.lookup(db);
        let item_tree = db.item_tree(loc.id.file_id);
        let it = &item_tree[loc.id.value];
        let container = ContainerId::Class(id);
        let items = collect_assoc_items(db, loc.id.file_id, it.items.iter().copied(), container);

        Arc::new(ClassData {
            name: it.name.clone(),
            fundeps: it.fundeps.clone(),
            constraints: it.constraints.clone(),
            items: items.into(),
        })
    }
}

impl InstanceData {
    pub fn query(db: &dyn DefDatabase, id: InstanceId) -> Arc<Self> {
        let loc = id.lookup(db);
        let item_tree = db.item_tree(loc.id.file_id);
        let it = &item_tree[loc.id.value];
        let container = ContainerId::Instance(id);
        let items = collect_assoc_items(db, loc.id.file_id, it.items.iter().copied(), container);

        Arc::new(InstanceData {
            class: it.class.clone(),
            types: it.types.clone(),
            constraints: it.constraints.clone(),
            items: items.into_iter().map(|(_, it)| it).collect(),
        })
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
