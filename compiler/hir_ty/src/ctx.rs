use arena::ArenaMap;
use hir_def::body::Body;
use hir_def::expr::ExprId;
use hir_def::id::{HasModule, TypeCtorId, TypedItemId};
use hir_def::lang_item::{self, LangItem};
use hir_def::pat::PatId;
use hir_def::type_ref::TypeRefId;
use parking_lot::RwLock;
use triomphe::Arc;

use crate::ty::{Ty, TyKind};
use crate::unify::{Substitution, UnkLevel};
use crate::Db;

pub struct Ctx<'db> {
    pub(crate) db: &'db dyn Db,
    pub(crate) result: InferResult,
    pub(crate) subst: Substitution,
    pub(crate) owner: TypedItemId,
}

pub struct BodyCtx<'db, 'ctx> {
    pub(crate) ctx: &'ctx mut Ctx<'db>,
    pub(crate) body: Arc<Body>,
    pub(crate) level: UnkLevel,
}

#[derive(Default, Debug, PartialEq, Eq, Hash)]
pub struct InferResult {
    pub kind_of_ty: ArenaMap<TypeRefId, Ty>,
    pub type_of_expr: ArenaMap<ExprId, Ty>,
    pub type_of_pat: ArenaMap<PatId, Ty>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Expectation {
    None,
    HasType(Ty),
}

#[derive(Default, Debug)]
pub struct Cache(RwLock<CacheInner>);

#[derive(Default, Debug)]
pub struct CacheInner {}

impl<'db> Ctx<'db> {
    pub fn new(db: &'db dyn Db, owner: TypedItemId) -> Self {
        Self {
            db,
            owner,
            result: InferResult::default(),
            subst: Substitution::default(),
        }
    }

    pub fn with_body(&mut self, body: Arc<Body>) -> BodyCtx<'db, '_> {
        BodyCtx {
            level: UnkLevel(1),
            ctx: self,
            body,
        }
    }

    pub fn finish(self) -> Arc<InferResult> {
        Arc::new(self.result)
    }

    pub fn error(&self) -> Ty {
        Ty::new(self.db, TyKind::Error)
    }

    pub fn type_kind(&self) -> Ty {
        let type_kind = self.lang_ctor(lang_item::TYPE_KIND).unwrap();
        Ty::new(self.db, TyKind::Ctor(type_kind))
    }

    pub fn unit_type(&self) -> Ty {
        let unit_type = self.lang_ctor(lang_item::UNIT_TYPE).unwrap();
        Ty::new(self.db, TyKind::Ctor(unit_type))
    }

    pub fn bool_type(&self) -> Ty {
        let bool_type = self.lang_ctor(lang_item::BOOL_TYPE).unwrap();
        Ty::new(self.db, TyKind::Ctor(bool_type))
    }

    fn lang_item(&self, name: &'static str) -> Option<LangItem> {
        let lib = self.owner.module(self.db).lib(self.db);
        lang_item::query(self.db, lib, name)
    }

    fn lang_ctor(&self, name: &'static str) -> Option<TypeCtorId> {
        self.lang_item(name).and_then(LangItem::as_type_ctor)
    }
}

impl<'db> std::ops::Deref for BodyCtx<'db, '_> {
    type Target = Ctx<'db>;

    fn deref(&self) -> &Self::Target {
        self.ctx
    }
}

impl<'db> std::ops::DerefMut for BodyCtx<'db, '_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.ctx
    }
}

impl Cache {
    pub fn clear(&self) {
        self.0.write().clear();
    }
}

impl CacheInner {
    fn clear(&mut self) {
    }
}

impl Expectation {
    pub fn adjust_for_branches(self, db: &dyn Db) -> Self {
        match self {
            | Self::HasType(ty) => {
                if let TyKind::Unknown(_) = ty.kind(db) {
                    Self::None
                } else {
                    Self::HasType(ty)
                }
            },
            | _ => Self::None,
        }
    }

    pub fn to_option(self) -> Option<Ty> {
        match self {
            | Self::HasType(ty) => Some(ty),
            | _ => None,
        }
    }
}
