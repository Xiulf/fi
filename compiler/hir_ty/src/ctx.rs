use arena::ArenaMap;
use hir_def::expr::ExprId;
use hir_def::pat::PatId;
use hir_def::type_ref::TypeRefId;
use parking_lot::RwLock;

use crate::ty::{Kind, Ty};
use crate::Db;

pub struct Ctx<'db> {
    db: &'db dyn Db,
    kind_of_ty: ArenaMap<TypeRefId, Kind>,
    type_of_expr: ArenaMap<ExprId, Ty>,
    type_of_pat: ArenaMap<PatId, Ty>,
}

#[derive(Default, Debug)]
pub struct Cache(RwLock<CacheInner>);

#[derive(Default, Debug)]
pub struct CacheInner {}

impl Cache {
    pub fn clear(&self) {
        self.0.write().clear();
    }
}

impl CacheInner {
    fn clear(&mut self) {
    }
}
