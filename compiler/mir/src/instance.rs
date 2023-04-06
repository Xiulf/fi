use hir_def::display::HirDisplay;
use hir_def::id::ImplId;
use hir_ty::ty::Ty;
use triomphe::Arc;

use crate::ir::{Body, MirValueId};
use crate::repr::{repr_of, Repr};
use crate::Db;

#[salsa::interned]
pub struct Instance {
    pub id: InstanceId,
    #[return_ref]
    pub subst: Option<Subst>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum InstanceId {
    MirValueId(MirValueId),
    VtableMethod(MirValueId, usize, usize),
    Body(Body),
}

#[salsa::interned]
pub struct ImplInstance {
    pub id: ImplId,
    #[return_ref]
    pub subst: Option<Subst>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Subst {
    pub types: Vec<Ty>,
    pub impls: Vec<ImplSource>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ImplSource {
    Instance(ImplInstance),
    Param(usize),
}

impl Instance {
    pub fn is_foreign(self, db: &dyn Db) -> bool {
        match self.id(db) {
            | InstanceId::MirValueId(id) => match id {
                | MirValueId::ValueId(id) => hir::Value::from(id).is_foreign(db),
                | _ => false,
            },
            | _ => false,
        }
    }

    pub fn is_exported(self, db: &dyn Db) -> bool {
        match self.id(db) {
            | InstanceId::MirValueId(id) => match id {
                | MirValueId::ValueId(id) => hir::Value::from(id).is_exported(db),
                | MirValueId::CtorId(id) => hir::Ctor::from(id).is_exported(db),
                // | MirValueId::FieldId(id) => hir::Field::from(id).is_exported(db),
                | _ => false,
            },
            | _ => false,
        }
    }

    pub fn is_func(self, db: &dyn Db) -> bool {
        match self.id(db) {
            | InstanceId::MirValueId(id) => match id {
                | MirValueId::ValueId(id) => hir::Value::from(id).is_func(db),
                | MirValueId::CtorId(id) => !hir::Ctor::from(id).types(db).is_empty(),
                // | MirValueId::FieldId(id) => hir::Field::from(id).is_exported(db),
                | _ => false,
            },
            | _ => false,
        }
    }

    pub fn repr(self, db: &dyn Db) -> Arc<Repr> {
        let ty = match self.id(db) {
            | InstanceId::MirValueId(id) => match id {
                | MirValueId::ValueId(id) => hir::Value::from(id).ty(db).ty(),
                | MirValueId::CtorId(id) => hir::Ctor::from(id).ty(db).ty(),
                // | MirValueId::FieldId(id) => hir::Field::from(id).is_exported(db),
                | _ => todo!(),
            },
            | _ => todo!(),
        };

        repr_of(db, ty)
    }

    pub fn link_name(self, db: &dyn Db) -> String {
        self.display(db).to_string()
    }
}

impl Subst {
    pub fn is_empty(&self) -> bool {
        self.types.is_empty() && self.impls.is_empty()
    }
}

ra_ap_stdx::impl_from!(MirValueId, Body for InstanceId);
