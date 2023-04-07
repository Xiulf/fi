use hir::id::TypeVarId;
use hir_def::display::HirDisplay;
use hir_def::id::ImplId;
use hir_ty::ty::Ty;
use ra_ap_stdx::hash::NoHashHashMap;
use triomphe::Arc;

use crate::ir::{Body, MirValueId};
use crate::repr::{repr_of, ArrayLen, Repr, Signature};
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

#[derive(Debug)]
pub struct InstanceData {
    pub id: InstanceId,
    pub types: NoHashHashMap<TypeVarId, Ty>,
    pub impls: Vec<ImplSource>,
}

impl InstanceId {
    pub fn type_vars(self, db: &dyn Db) -> Vec<hir::TypeVar> {
        match self {
            | Self::MirValueId(id) => match id {
                | MirValueId::ValueId(id) => hir::Value::from(id).type_vars(db),
                | MirValueId::CtorId(id) => hir::Ctor::from(id).type_ctor(db).type_vars(db),
                | _ => todo!(),
            },
            | _ => Vec::new(),
        }
    }
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

    pub fn has_body(self, db: &dyn Db) -> bool {
        match self.id(db) {
            | InstanceId::MirValueId(id) => match id {
                | MirValueId::ValueId(id) => hir::Value::from(id).has_body(db),
                | _ => true,
            },
            | InstanceId::Body(_) => true,
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

        self.data(db).subst_repr(db, &repr_of(db, ty))
    }

    pub fn link_name(self, db: &dyn Db) -> String {
        self.display(db).to_string()
    }

    pub fn data(self, db: &dyn Db) -> InstanceData {
        let id = self.id(db);
        let (types, impls) = match self.subst(db) {
            | Some(subst) => {
                let vars = id.type_vars(db);
                let types = subst
                    .types
                    .iter()
                    .zip(vars.into_iter())
                    .map(|(&t, v)| (v.id(), t))
                    .collect();

                (types, subst.impls.clone())
            },
            | None => (NoHashHashMap::default(), Vec::new()),
        };

        InstanceData { id, types, impls }
    }
}

impl InstanceData {
    pub fn subst_instance(&self, db: &dyn Db, inst: Instance) -> Instance {
        let id = match inst.id(db) {
            | InstanceId::VtableMethod(owner, vtable, method) => match self.impls[vtable] {
                | ImplSource::Instance(id) => {
                    let value = hir::Impl::from(id.id(db)).items(db)[method].id();
                    InstanceId::MirValueId(MirValueId::ValueId(value))
                },
                | ImplSource::Param(idx) => InstanceId::VtableMethod(owner, idx, method),
            },
            | id => id,
        };

        let replacements = self.types.iter().map(|(k, v)| (*k, *v)).collect();
        let subst = inst.subst(db).as_ref().map(|s| {
            let types = s.types.iter().map(|t| t.replace_vars(db, &replacements)).collect();
            let impls = s.impls.clone();

            Subst { types, impls }
        });

        Instance::new(db, id, subst)
    }

    pub fn subst_repr(&self, db: &dyn Db, repr: &Arc<Repr>) -> Arc<Repr> {
        match &**repr {
            | Repr::TypeVar(tv) => match self.find_var(tv) {
                | Some(ty) => repr_of(db, ty),
                | None => repr.clone(),
            },
            | Repr::ReprOf(ty) => self.subst_repr(db, &repr_of(db, *ty)),
            | Repr::Discr(repr) => Arc::new(Repr::Discr(self.subst_repr(db, repr))),
            | Repr::Ptr(to, fat, nn) => Arc::new(Repr::Ptr(self.subst_repr(db, to), *fat, *nn)),
            | Repr::Box(of) => Arc::new(Repr::Box(self.subst_repr(db, of))),
            | Repr::Struct(reprs) => Arc::new(Repr::Struct(reprs.iter().map(|r| self.subst_repr(db, r)).collect())),
            | Repr::Enum(reprs) => Arc::new(Repr::Enum(reprs.iter().map(|r| self.subst_repr(db, r)).collect())),
            | Repr::Array(len, of) => Arc::new(Repr::Array(self.subst_array_len(db, len), self.subst_repr(db, of))),
            | Repr::Func(sig, env) => Arc::new(Repr::Func(
                self.subst_signature(db, sig),
                env.as_ref()
                    .map(|e| self.subst_repr(db, e))
                    .filter(|e| !matches!(&**e, Repr::Struct(f) if f.is_empty())),
            )),
            | _ => repr.clone(),
        }
    }

    pub fn subst_array_len(&self, _db: &dyn Db, len: &ArrayLen) -> ArrayLen {
        match len {
            | ArrayLen::TypeVar(tv) => match self.find_var(tv) {
                | Some(_ty) => todo!(),
                | None => len.clone(),
            },
            | _ => len.clone(),
        }
    }

    pub fn subst_signature(&self, db: &dyn Db, sig: &Signature) -> Signature {
        Signature {
            params: sig.params.iter().map(|p| self.subst_repr(db, p)).collect(),
            ret: self.subst_repr(db, &sig.ret),
        }
    }

    fn find_var(&self, var: &TypeVarId) -> Option<Ty> {
        self.types.iter().find(|(v, _)| *v == var).map(|(_, t)| *t)
    }
}

impl Subst {
    pub fn is_empty(&self) -> bool {
        self.types.is_empty() && self.impls.is_empty()
    }
}

ra_ap_stdx::impl_from!(MirValueId, Body for InstanceId);
