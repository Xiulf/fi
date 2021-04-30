use crate::db::DefDatabase;
use crate::id::*;
pub use crate::item_tree::{Assoc, Prec};
use crate::name::Name;
use crate::path::Path;
use std::sync::Arc;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FixityData {
    pub name: Name,
    pub func: Path,
    pub prec: Prec,
    pub assoc: Assoc,
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
