use crate::db::DefDatabase;
use crate::id::*;
pub use crate::item_tree::{Assoc, Prec};
use crate::name::Name;
use crate::path::Path;
use crate::type_ref::TypeRefId;
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
    pub ty: Option<TypeRefId>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeData {
    pub name: Name,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CtorData {
    pub name: Name,
}

impl FixityData {
    pub fn query(db: &dyn DefDatabase, id: FixityId) -> Arc<Self> {
        let loc = id.lookup(db);
        let item_tree = db.item_tree(loc.id.file_id);
        let fixity = &item_tree[loc.id.value];

        Arc::new(FixityData {
            name: fixity.name.clone(),
            func: fixity.func.clone(),
            prec: fixity.prec,
            assoc: fixity.assoc,
        })
    }
}
