use std::sync::Arc;

use hir::ty::{Ty, TyKind};
use hir::{DefWithBody, HirDisplay, MethodSource, Name};

use crate::db::MirDatabase;
use crate::repr::{ArrayLen, Repr, Signature};
use crate::syntax::Body;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Instance {
    pub def: InstanceDef,
    pub subst: Option<Arc<Subst>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum InstanceDef {
    Def(DefWithBody),
    Body(Body),
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct Subst {
    pub types: Vec<Ty>,
    pub methods: Vec<MethodSource>,
}

impl Instance {
    pub fn new(def: InstanceDef, types: Vec<Ty>, methods: Vec<MethodSource>) -> Self {
        Self {
            def,
            subst: Some(Arc::new(Subst { types, methods })),
        }
    }

    pub fn mono(def: InstanceDef) -> Self {
        Self { def, subst: None }
    }

    pub fn subst_repr(&self, db: &dyn MirDatabase, repr: &Repr) -> Repr {
        if let Some(subst) = &self.subst {
            subst.subst_repr(db, repr)
        } else {
            repr.clone()
        }
    }

    pub fn subst_instance(&self, db: &dyn MirDatabase, instance: &Instance) -> Instance {
        if let Some(subst) = &self.subst {
            subst.subst_instance(db, instance)
        } else {
            instance.clone()
        }
    }

    pub fn has_body(&self, db: &dyn MirDatabase) -> bool {
        match self.def {
            | InstanceDef::Def(def) => def.has_body(db.upcast()),
            | InstanceDef::Body(_) => true,
        }
    }

    pub fn body(&self, db: &dyn MirDatabase) -> Body {
        match self.def {
            | InstanceDef::Def(def) => db.body_mir(def.into()),
            | InstanceDef::Body(body) => body,
        }
    }

    pub fn link_name(&self, db: &dyn MirDatabase) -> String {
        use std::fmt::Write;
        let mut def_name = self.def.link_name(db);

        if self.is_foreign(db) {
            return def_name;
        }

        if let Some(subst) = &self.subst {
            let mut text = String::new();

            for ty in &subst.types {
                write!(text, "^{}", ty.display(db.upcast())).unwrap();
            }

            for &method in &subst.methods {
                match method {
                    | MethodSource::Member(m) => {
                        write!(text, "&{}", hir::Member::from(m).link_name(db.upcast())).unwrap()
                    },
                    | MethodSource::Record(i, _) => write!(text, "&${i}").unwrap(),
                }
            }

            def_name.push_str(&mangling::mangle(text.bytes()));
        }

        def_name
    }

    pub fn is_foreign(&self, db: &dyn MirDatabase) -> bool {
        match self.def {
            | InstanceDef::Def(DefWithBody::Func(d)) => d.is_foreign(db.upcast()),
            | InstanceDef::Def(DefWithBody::Static(d)) => d.is_foreign(db.upcast()),
            | InstanceDef::Def(DefWithBody::Const(_)) => false,
            | InstanceDef::Body(_) => false,
        }
    }

    pub fn is_exported(&self, db: &dyn MirDatabase) -> bool {
        match self.def {
            | InstanceDef::Def(DefWithBody::Func(d)) => d.is_exported(db.upcast()),
            | InstanceDef::Def(DefWithBody::Static(d)) => d.is_exported(db.upcast()),
            | InstanceDef::Def(DefWithBody::Const(_)) => false,
            | InstanceDef::Body(_) => false,
        }
    }
}

impl InstanceDef {
    pub fn link_name(self, db: &dyn MirDatabase) -> String {
        return match self {
            | Self::Def(def) => {
                let (name, mangle) = def_name(db, def);

                if mangle {
                    mangling::mangle(name.as_ref().bytes())
                } else {
                    name.to_string()
                }
            },
            | Self::Body(id) => {
                let origin = db.lookup_intern_body(id).origin;
                let (name, mut mangle) = def_name(db, origin.def.into());
                let mut name = name.to_string();

                if let Some(expr) = origin.expr {
                    name.push_str(&format!("#{}", u32::from(expr.into_raw())));
                    mangle = true;
                }

                if mangle {
                    name = mangling::mangle(name.bytes());
                }

                name
            },
        };

        fn def_name(db: &dyn MirDatabase, def: DefWithBody) -> (Name, bool) {
            match def {
                | DefWithBody::Func(f) => f.link_name(db.upcast()),
                | DefWithBody::Static(f) => f.link_name(db.upcast()),
                | DefWithBody::Const(f) => (f.name(db.upcast()), true),
            }
        }
    }
}

impl Subst {
    pub fn subst_instance(&self, db: &dyn MirDatabase, instance: &Instance) -> Instance {
        Instance {
            def: instance.def,
            subst: instance.subst.as_ref().map(|s| {
                let types = s
                    .types
                    .iter()
                    .map(|t| t.replace_local_vars(db.upcast(), &self.types))
                    .collect();
                let methods = s.methods.clone();

                Arc::new(Subst { types, methods })
            }),
        }
    }

    pub fn subst_repr(&self, db: &dyn MirDatabase, repr: &Repr) -> Repr {
        match repr {
            | Repr::TypeVar(tv) => db.repr_of(self.types[tv.idx() as usize]),
            | Repr::ReprOf(ty) => self.subst_repr(db, &db.repr_of(*ty)),
            | Repr::Ptr(to, fat, nn) => Repr::Ptr(Box::new(self.subst_repr(db, to)), *fat, *nn),
            | Repr::Box(of) => Repr::Box(Box::new(self.subst_repr(db, of))),
            | Repr::Struct(reprs) => Repr::Struct(reprs.iter().map(|r| self.subst_repr(db, r)).collect()),
            | Repr::Enum(reprs) => Repr::Enum(reprs.iter().map(|r| self.subst_repr(db, r)).collect()),
            | Repr::Array(len, of) => Repr::Array(self.subst_array_length(db, len), Box::new(self.subst_repr(db, of))),
            | Repr::Func(sig, fat) => Repr::Func(Box::new(self.subst_signature(db, sig)), *fat),
            | _ => repr.clone(),
        }
    }

    pub fn subst_array_length(&self, db: &dyn MirDatabase, len: &ArrayLen) -> ArrayLen {
        match len {
            | ArrayLen::TypeVar(tv) => match self.types[tv.idx() as usize].lookup(db.upcast()) {
                | TyKind::Figure(figure) => ArrayLen::Const(figure as usize),
                | _ => unreachable!(),
            },
            | _ => len.clone(),
        }
    }

    pub fn subst_signature(&self, db: &dyn MirDatabase, sig: &Signature) -> Signature {
        Signature {
            params: sig.params.iter().map(|p| self.subst_repr(db, p)).collect(),
            ret: self.subst_repr(db, &sig.ret),
        }
    }
}

impl From<hir::Func> for InstanceDef {
    fn from(f: hir::Func) -> Self {
        Self::Def(f.into())
    }
}

impl From<hir::Const> for InstanceDef {
    fn from(f: hir::Const) -> Self {
        Self::Def(f.into())
    }
}

impl From<hir::Static> for InstanceDef {
    fn from(f: hir::Static) -> Self {
        Self::Def(f.into())
    }
}

impl From<Body> for InstanceDef {
    fn from(f: Body) -> Self {
        Self::Body(f)
    }
}
