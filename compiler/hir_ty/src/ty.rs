use hir_def::display::HirDisplay;
use hir_def::expr::ExprId;
use hir_def::id::{ImplId, TraitId, TypeCtorId, TypeVarId};
use hir_def::pat::PatId;
use ra_ap_stdx::hash::NoHashHashMap;

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
    Unknown(Unknown, bool),
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

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Constraint {
    pub trait_id: TraitId,
    pub args: Box<[Ty]>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ConstraintOrigin {
    ExprId(ExprId),
    PatId(PatId),
    Impl(ImplId, usize),
}

ra_ap_stdx::impl_from!(ExprId, PatId for ConstraintOrigin);

pub type GeneralizedType = Generalized<Ty>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Generalized<T> {
    Mono(T),
    Poly(Box<[TypeVarId]>, T),
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

    pub fn traverse(self, db: &dyn Db, f: &mut dyn FnMut(Ty)) {
        match self.kind(db) {
            | TyKind::App(base, args) => {
                base.traverse(db, f);
                args.iter().for_each(|a| a.traverse(db, f));
            },
            | TyKind::Func(func) => {
                func.params.iter().for_each(|p| p.traverse(db, f));
                func.ret.traverse(db, f);
                func.env.traverse(db, f);
            },
            | _ => {},
        }

        f(self)
    }

    pub fn replace_vars(self, db: &dyn Db, replacements: &NoHashHashMap<TypeVarId, Ty>) -> Ty {
        self.fold(db, &mut |ty| match ty.kind(db) {
            | TyKind::Var(v) => match replacements.get(v) {
                | Some(t) => *t,
                | None => ty,
            },
            | _ => ty,
        })
    }
}

impl Constraint {
    pub fn replace_vars(&self, db: &dyn Db, replacements: &NoHashHashMap<TypeVarId, Ty>) -> Self {
        let args = self.args.iter().map(|t| t.replace_vars(db, replacements)).collect();

        Self {
            trait_id: self.trait_id,
            args,
        }
    }
}

impl<T> Generalized<T> {
    pub fn new(ty: T, vars: &Box<[TypeVarId]>) -> Self {
        if vars.is_empty() {
            Self::Mono(ty)
        } else {
            Self::Poly(vars.clone(), ty)
        }
    }
}

struct Parens(ParenMode, Ty);

enum ParenMode {
    Arg,
    App,
}

impl HirDisplay for Parens {
    type Db<'a> = dyn Db + 'a;

    fn hir_fmt(&self, f: &mut hir_def::display::HirFormatter<Self::Db<'_>>) -> std::fmt::Result {
        use std::fmt::Write as _;
        match (&self.0, self.1.kind(f.db)) {
            | (ParenMode::Arg, TyKind::Func(func)) => write!(f, "({})", func.display(f.db)),
            | (ParenMode::App, TyKind::Func(_) | TyKind::App(..)) => write!(f, "({})", self.1.display(f.db)),
            | _ => self.1.hir_fmt(f),
        }
    }
}

impl HirDisplay for Ty {
    type Db<'a> = dyn Db + 'a;

    fn hir_fmt(&self, f: &mut hir_def::display::HirFormatter<Self::Db<'_>>) -> std::fmt::Result {
        use std::fmt::Write as _;
        match self.kind(f.db) {
            | TyKind::Error => write!(f, "{{error}}"),
            | TyKind::Unknown(u, _) => write!(f, "?{}", u.0),
            | TyKind::Var(var) => f.with_upcast::<_, dyn hir_def::Db>(|d| d, |f| var.hir_fmt(f)),
            | TyKind::Ctor(ctor) => {
                let it = ctor.it(f.db);
                let item_tree = hir_def::item_tree::query(f.db, it.file);
                write!(f, "{}", item_tree[it.value].name.display(f.db))
            },
            | TyKind::App(base, args) => {
                Parens(ParenMode::App, *base).hir_fmt(f)?;
                for arg in args.iter() {
                    f.write_char(' ')?;
                    Parens(ParenMode::App, *arg).hir_fmt(f)?;
                }
                Ok(())
            },
            | TyKind::Func(func) => func.hir_fmt(f),
        }
    }
}

impl HirDisplay for FuncType {
    type Db<'a> = dyn Db + 'a;

    fn hir_fmt(&self, f: &mut hir_def::display::HirFormatter<Self::Db<'_>>) -> std::fmt::Result {
        use std::fmt::Write as _;
        f.write_char('{')?;
        self.env.hir_fmt(f)?;
        f.write_str("} ")?;
        f.write_joined(self.params.iter().map(|p| Parens(ParenMode::Arg, *p)), ", ")?;
        if self.variadic {
            f.write_str(", ..")?;
        }
        f.write_str(" -> ")?;
        self.ret.hir_fmt(f)
    }
}

impl HirDisplay for GeneralizedType {
    type Db<'a> = dyn Db + 'a;

    fn hir_fmt(&self, f: &mut hir_def::display::HirFormatter<Self::Db<'_>>) -> std::fmt::Result {
        use std::fmt::Write as _;
        match self {
            | Self::Mono(ty) => ty.hir_fmt(f),
            | Self::Poly(vars, ty) => {
                f.write_str("forall")?;
                f.with_upcast::<_, dyn hir_def::Db>(
                    |d| d,
                    |f| {
                        for var in vars.iter() {
                            f.write_char(' ')?;
                            var.hir_fmt(f)?;
                        }
                        Ok(())
                    },
                )?;
                f.write_str(". ")?;
                ty.hir_fmt(f)
            },
        }
    }
}

impl HirDisplay for Constraint {
    type Db<'a> = dyn Db + 'a;

    fn hir_fmt(&self, f: &mut hir_def::display::HirFormatter<Self::Db<'_>>) -> std::fmt::Result {
        use std::fmt::Write as _;
        let it = self.trait_id.it(f.db);
        let tree = hir_def::item_tree::query(f.db, it.file);
        let node = &tree[it.value];

        write!(f, "{}", node.name.display(f.db))?;

        for ty in self.args.iter() {
            f.write_char(' ')?;
            Parens(ParenMode::App, *ty).hir_fmt(f)?;
        }

        Ok(())
    }
}
