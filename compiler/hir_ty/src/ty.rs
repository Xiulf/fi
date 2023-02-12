use std::collections::HashMap;

use either::Either;
use hir_def::display::HirDisplay;
use hir_def::id::{TypeCtorId, TypeVarId};

use crate::Db;

#[salsa::interned]
pub struct Ty {
    #[return_ref]
    pub kind: TyKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Unknown(pub(crate) u32);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TyKind {
    Error,
    Unknown(Unknown),
    Var(TypeVarId),
    Ctor(TypeCtorId),
    App(Ty, Box<[Ty]>),
    Func(FuncType),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FuncType {
    pub params: Box<[Ty]>,
    pub ret: Ty,
    pub env: Ty,
    pub variadic: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum GeneralizedType {
    Mono(Ty),
    Poly(Box<[TypeVarId]>, Ty),
}

impl Ty {
    pub fn fold(self, db: &dyn Db, f: &mut dyn FnMut(Ty) -> Ty) -> Ty {
        match self.kind(db) {
            | TyKind::App(base, args) => {
                let base = base.fold(db, f);
                let args = args.iter().map(|a| a.fold(db, f)).collect();

                f(Ty::new(db, TyKind::App(base, args)))
            },
            | TyKind::Func(func) => {
                let params = func.params.iter().map(|p| p.fold(db, f)).collect();
                let ret = func.ret.fold(db, f);
                let env = func.env.fold(db, f);

                f(Ty::new(
                    db,
                    TyKind::Func(FuncType {
                        params,
                        ret,
                        env,
                        variadic: func.variadic,
                    }),
                ))
            },
            | _ => f(self),
        }
    }

    pub fn replace_vars(self, db: &dyn Db, replacements: &HashMap<TypeVarId, Ty>) -> Ty {
        self.fold(db, &mut |ty| match ty.kind(db) {
            | TyKind::Var(v) => match replacements.get(v) {
                | Some(t) => *t,
                | None => ty,
            },
            | _ => ty,
        })
    }
}

impl GeneralizedType {
    pub fn new(ty: Ty, vars: &[TypeVarId]) -> Self {
        if vars.is_empty() {
            Self::Mono(ty)
        } else {
            Self::Poly(vars.into(), ty)
        }
    }
}

impl HirDisplay for Ty {
    type Db<'a> = dyn crate::Db + 'a;

    fn hir_fmt(&self, f: &mut hir_def::display::HirFormatter<Self::Db<'_>>) -> std::fmt::Result {
        use std::fmt::Write as _;
        match self.kind(f.db) {
            | TyKind::Error => write!(f, "{{error}}"),
            | TyKind::Unknown(u) => write!(f, "?{}", u.0),
            | TyKind::Var(var) => {
                let owner = var.owner(f.db);
                let type_map = owner.type_map(f.db).0;
                let name = match var.local_id(f.db) {
                    | Either::Left(local_id) => type_map[local_id].name,
                    | Either::Right(name) => name,
                };

                write!(f, "{}", name.display(f.db))
            },
            | TyKind::Ctor(ctor) => {
                let it = ctor.it(f.db);
                let item_tree = hir_def::item_tree::query(f.db, it.file);
                write!(f, "{}", item_tree[it.value].name.display(f.db))
            },
            | TyKind::App(base, args) => {
                base.hir_fmt(f)?;
                for arg in args.iter() {
                    f.write_char(' ')?;
                    arg.hir_fmt(f)?;
                }
                Ok(())
            },
            | TyKind::Func(func) => {
                f.write_joined(func.params.iter(), ", ")?;
                if func.variadic {
                    f.write_str(", ..")?;
                }
                f.write_str(" -> ")?;
                func.ret.hir_fmt(f)
            },
        }
    }
}
