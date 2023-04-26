use hir::id::TypeVarId;
use hir_def::display::HirDisplay;
use hir_def::id::ImplId;
use hir_ty::ty::Ty;
use ra_ap_stdx::hash::NoHashHashMap;

use crate::ir::{Body, MirValueId};
use crate::repr::{repr_of, ArrayLen, Repr, ReprKind, Signature};
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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
        if self.subst(db).is_some() {
            return false;
        }

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
            | InstanceId::VtableMethod(_, _, _) => true,
            | InstanceId::Body(body) => match body.repr(db).kind(db) {
                | ReprKind::Func(_, _) => true,
                | _ => false,
            },
        }
    }

    pub fn repr(self, db: &dyn Db) -> Repr {
        let ty = match self.id(db) {
            | InstanceId::MirValueId(id) => match id {
                | MirValueId::ValueId(id) => hir::Value::from(id).ty(db).ty(),
                | MirValueId::CtorId(id) => hir::Ctor::from(id).ty(db).ty(),
                // | MirValueId::FieldId(id) => hir::Field::from(id).is_exported(db),
                | _ => todo!(),
            },
            | InstanceId::VtableMethod(owner, table, method) => {
                let def = match owner {
                    | MirValueId::ValueId(id) => crate::lower::value_mir(db, id),
                    | MirValueId::Lambda(id, _) => crate::lower::value_mir(db, id),
                    | MirValueId::CtorId(id) => crate::lower::ctor_mir(db, id),
                    | _ => todo!(),
                };

                let body = def.body(db).unwrap();
                let trait_id = body.constraints(db)[table].trait_id;
                let method = hir::Trait::from(trait_id).items(db)[method];

                method.ty(db).ty()
            },
            | InstanceId::Body(body) => return body.repr(db),
        };

        self.data(db).subst_repr(db, repr_of(db, ty))
    }

    pub fn link_name(self, db: &dyn Db) -> String {
        if let InstanceId::MirValueId(MirValueId::ValueId(id)) = self.id(db) {
            let value = hir::Value::from(id);

            if let Some(link_name) = value.attrs(db).by_key("link_name").string_value().next() {
                return link_name.to_string();
            }

            if value.is_foreign(db) || value.attrs(db).by_key("no_mangle").exists() {
                return value.name(db).display(db).to_string();
            }
        }

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
        tracing::info!(
            "subst_instance({}, {:?}, {:?})",
            inst.display(db),
            self.types
                .iter()
                .map(|(_, t)| t.display(db).to_string())
                .collect::<Vec<_>>(),
            self.impls.iter().map(|i| i.display(db).to_string()).collect::<Vec<_>>()
        );
        let mut extra_impls = Vec::new();
        let id = match inst.id(db) {
            | InstanceId::VtableMethod(owner, vtable, method) => match self.impls[vtable] {
                | ImplSource::Instance(id) => {
                    let value = hir::Impl::from(id.id(db)).items(db)[method].id();
                    if let Some(subst) = id.subst(db) {
                        extra_impls = subst.impls.clone();
                    }
                    InstanceId::MirValueId(MirValueId::ValueId(value))
                },
                | ImplSource::Param(idx) => InstanceId::VtableMethod(owner, idx, method),
            },
            | id => id,
        };

        let subst = inst
            .subst(db)
            .as_ref()
            .map(|s| {
                let types = s.types.iter().map(|t| t.replace_vars(db, &self.types)).collect();
                let impls = s
                    .impls
                    .iter()
                    .map(|&s| self.subst_impl_source(db, s))
                    .chain(extra_impls.clone())
                    .collect();

                Subst { types, impls }
            })
            .or_else(|| {
                Some(Subst {
                    types: Vec::new(),
                    impls: extra_impls,
                })
                .filter(|s| !s.is_empty())
            });

        Instance::new(db, id, subst)
    }

    pub fn subst_impl_instance(&self, db: &dyn Db, inst: ImplInstance) -> ImplInstance {
        let subst = inst.subst(db).as_ref().map(|s| {
            let types = s.types.iter().map(|t| t.replace_vars(db, &self.types)).collect();
            let impls = s.impls.iter().map(|&s| self.subst_impl_source(db, s)).collect();

            Subst { types, impls }
        });

        ImplInstance::new(db, inst.id(db), subst)
    }

    pub fn subst_impl_source(&self, db: &dyn Db, src: ImplSource) -> ImplSource {
        match src {
            | ImplSource::Instance(i) => ImplSource::Instance(self.subst_impl_instance(db, i)),
            | ImplSource::Param(i) => self.impls[i],
        }
    }

    pub fn subst_repr(&self, db: &dyn Db, repr: Repr) -> Repr {
        let kind = match repr.kind(db) {
            | ReprKind::TypeVar(tv) => match self.find_var(tv) {
                | Some(ty) => return repr_of(db, ty),
                | None => return repr,
            },
            | ReprKind::ReprOf(ty) => ReprKind::ReprOf(ty.replace_vars(db, &self.types)),
            | ReprKind::Discr(repr) => ReprKind::Discr(self.subst_repr(db, *repr)),
            | ReprKind::Ptr(to, fat, nn) => ReprKind::Ptr(self.subst_repr(db, *to), *fat, *nn),
            | ReprKind::Box(of) => ReprKind::Box(self.subst_repr(db, *of)),
            | ReprKind::Struct(reprs) => ReprKind::Struct(reprs.iter().map(|&r| self.subst_repr(db, r)).collect()),
            | ReprKind::Enum(reprs) => ReprKind::Enum(reprs.iter().map(|&r| self.subst_repr(db, r)).collect()),
            | ReprKind::Array(len, of) => ReprKind::Array(self.subst_array_len(db, len), self.subst_repr(db, *of)),
            | ReprKind::Func(sig, env) => ReprKind::Func(
                self.subst_signature(db, sig),
                env.as_ref()
                    .map(|&e| self.subst_repr(db, e))
                    .filter(|e| !matches!(e.kind(db), ReprKind::Struct(f) if f.is_empty())),
            ),
            | _ => return repr,
        };

        Repr::new(db, kind)
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
            params: sig.params.iter().map(|&p| self.subst_repr(db, p)).collect(),
            ret: self.subst_repr(db, sig.ret),
            is_varargs: sig.is_varargs,
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
