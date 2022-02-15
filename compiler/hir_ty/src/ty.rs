use crate::db::HirDatabase;
use crate::display::HirDisplay;
use hir_def::id::{ClassId, TypeCtorId};
use hir_def::name::Name;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Ty(salsa::InternId);

pub type List<T> = Box<[T]>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TyKind {
    Error,

    Unknown(Unknown),
    Skolem(TypeVar, Ty),
    TypeVar(TypeVar),

    Figure(i128),
    Symbol(String),
    Row(List<Field>, Option<Ty>),

    Ctor(TypeCtorId),
    App(Ty, List<Ty>),

    Tuple(List<Ty>),
    Func(List<Ty>, Ty),

    Ctnt(Constraint, Ty),
    ForAll(List<Ty>, Ty),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Field {
    pub name: Name,
    pub ty: Ty,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Constraint {
    pub class: ClassId,
    pub types: List<Ty>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Unknown(u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeVar(u32, DebruijnIndex);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct DebruijnIndex(u32);

impl Ty {
    pub fn lookup(self, db: &dyn HirDatabase) -> TyKind {
        db.lookup_intern_ty(self)
    }

    pub fn match_ctor(mut self, db: &dyn HirDatabase, id: TypeCtorId) -> Option<List<Ty>> {
        if let TyKind::App(ctor, args) = self.lookup(db) {
            if ctor.lookup(db) == TyKind::Ctor(id) {
                Some(args)
            } else {
                None
            }
        } else {
            None
        }
    }

    pub fn everywhere<F>(self, db: &dyn HirDatabase, f: &mut F) -> Ty
    where
        F: FnMut(Ty) -> Ty,
    {
        match self.lookup(db) {
            | TyKind::Skolem(sk, k) => {
                let k = k.everywhere(db, f);

                f(TyKind::Skolem(sk, k).intern(db))
            },
            | TyKind::Row(fields, tail) => {
                let fields = fields
                    .iter()
                    .map(|field| Field {
                        name: field.name.clone(),
                        ty: field.ty.everywhere(db, f),
                    })
                    .collect();

                let tail = tail.map(|t| t.everywhere(db, f));

                f(TyKind::Row(fields, tail).intern(db))
            },
            | TyKind::App(base, args) => {
                let base = base.everywhere(db, f);
                let args = args.iter().map(|t| t.everywhere(db, f)).collect();

                f(TyKind::App(base, args).intern(db))
            },
            | TyKind::Tuple(tys) => {
                let tys = tys.iter().map(|t| t.everywhere(db, f)).collect();

                f(TyKind::Tuple(tys).intern(db))
            },
            | TyKind::Func(args, ret) => {
                let args = args.iter().map(|t| t.everywhere(db, f)).collect();
                let ret = ret.everywhere(db, f);

                f(TyKind::Func(args, ret).intern(db))
            },
            | TyKind::Ctnt(ctnt, ty) => {
                let ctnt = Constraint {
                    class: ctnt.class,
                    types: ctnt.types.iter().map(|t| t.everywhere(db, f)).collect(),
                };

                let ty = ty.everywhere(db, f);

                f(TyKind::Ctnt(ctnt, ty).intern(db))
            },
            | TyKind::ForAll(k, t) => {
                let k = k.iter().map(|k| k.everywhere(db, f)).collect();
                let t = t.everywhere(db, f);

                f(TyKind::ForAll(k, t).intern(db))
            },
            | _ => f(self),
        }
    }

    pub fn everything<F>(self, db: &dyn HirDatabase, f: &mut F)
    where
        F: FnMut(Ty),
    {
        match self.lookup(db) {
            | TyKind::Skolem(_, k) => k.everything(db, f),
            | TyKind::Row(fields, tail) => {
                for field in fields.iter() {
                    field.ty.everything(db, f);
                }

                if let Some(tail) = tail {
                    tail.everything(db, f);
                }
            },
            | TyKind::App(base, args) => {
                base.everything(db, f);

                for ty in args.iter() {
                    ty.everything(db, f);
                }
            },
            | TyKind::Tuple(tys) => {
                for ty in tys.iter() {
                    ty.everything(db, f);
                }
            },
            | TyKind::Func(args, ret) => {
                for ty in args.iter() {
                    ty.everything(db, f);
                }

                ret.everything(db, f);
            },
            | TyKind::Ctnt(ctnt, ty) => {
                for ty in ctnt.types.iter() {
                    ty.everything(db, f);
                }

                ty.everything(db, f);
            },
            | TyKind::ForAll(k, t) => {
                for ty in k.iter() {
                    ty.everything(db, f);
                }

                t.everything(db, f);
            },
            | _ => {},
        }

        f(self)
    }

    pub fn normalize(self, db: &dyn HirDatabase) -> Ty {
        self.everywhere(db, &mut |ty| match ty.lookup(db) {
            // | TyKind::App(a, b) => match a.lookup(db) {
            //     | TyKind::ForAll(_, a) => a.replace_var(db, b),
            //     | _ => ty,
            // },
            | TyKind::Row(f1, Some(tail)) => match tail.lookup(db) {
                | TyKind::Row(f2, None) => {
                    let fields = f1.iter().cloned().chain(f2.iter().cloned()).collect();

                    TyKind::Row(fields, None).intern(db)
                },
                | TyKind::TypeVar(_) | TyKind::Unknown(_) | TyKind::Error => ty,
                | _ => unreachable!("{}", tail.display(db)),
            },
            | _ => ty,
        })
    }

    pub fn replace_vars(self, db: &dyn HirDatabase, with: &[Ty]) -> Ty {
        Self::replace_vars_impl(db, self, with, DebruijnIndex::INNER)
    }

    fn replace_vars_impl(db: &dyn HirDatabase, ty: Ty, with: &[Ty], depth: DebruijnIndex) -> Ty {
        match ty.lookup(db) {
            | TyKind::TypeVar(var) if var.debruijn() == depth => with[var.idx() as usize],
            | TyKind::Skolem(sk, k) => {
                let k = Self::replace_vars_impl(db, k, with, depth);

                TyKind::Skolem(sk, k).intern(db)
            },
            | TyKind::Row(fields, tail) => {
                let fields = fields
                    .iter()
                    .map(|f| Field {
                        name: f.name.clone(),
                        ty: Self::replace_vars_impl(db, f.ty, with, depth),
                    })
                    .collect();

                let tail = tail.map(|t| Self::replace_vars_impl(db, t, with, depth));

                TyKind::Row(fields, tail).intern(db)
            },
            | TyKind::App(base, args) => {
                let base = Self::replace_vars_impl(db, base, with, depth);
                let args = args
                    .iter()
                    .map(|&t| Self::replace_vars_impl(db, t, with, depth))
                    .collect();

                TyKind::App(base, args).intern(db)
            },
            | TyKind::Tuple(tys) => {
                let tys = tys
                    .iter()
                    .map(|&t| Self::replace_vars_impl(db, t, with, depth))
                    .collect();

                TyKind::Tuple(tys).intern(db)
            },
            | TyKind::Func(args, ret) => {
                let args = args
                    .iter()
                    .map(|&t| Self::replace_vars_impl(db, t, with, depth))
                    .collect();

                let ret = Self::replace_vars_impl(db, ret, with, depth);

                TyKind::Func(args, ret).intern(db)
            },
            | TyKind::Ctnt(ctnt, ty) => {
                let ctnt = Constraint {
                    class: ctnt.class,
                    types: ctnt
                        .types
                        .iter()
                        .map(|&t| Self::replace_vars_impl(db, t, with, depth))
                        .collect(),
                };

                let ty = Self::replace_vars_impl(db, ty, with, depth);

                TyKind::Ctnt(ctnt, ty).intern(db)
            },
            | TyKind::ForAll(k, inner) => {
                let k = k.iter().map(|&k| Self::replace_vars_impl(db, k, with, depth)).collect();
                let inner = Self::replace_vars_impl(db, inner, with, depth.shifted_in());

                TyKind::ForAll(k, inner).intern(db)
            },
            | _ => ty,
        }
    }

    pub fn to_row_list(self, db: &dyn HirDatabase) -> (List<Field>, Option<Ty>) {
        match self.lookup(db) {
            | TyKind::Row(fields, tail) => (fields.clone(), tail),
            | _ => (vec![].into(), None),
        }
    }

    pub fn align_rows_with<R>(
        db: &dyn HirDatabase,
        mut f: impl FnMut(Ty, Ty) -> R,
        t1: Ty,
        t2: Ty,
    ) -> (Vec<R>, ((List<Field>, Option<Ty>), (List<Field>, Option<Ty>))) {
        let (s1, tail1) = t1.to_row_list(db);
        let (s2, tail2) = t2.to_row_list(db);

        return go((db, &mut f, tail1, tail2), s1.iter().cloned(), s2.iter().cloned());

        fn go<R>(
            (db, f, t1, t2): (&dyn HirDatabase, &mut impl FnMut(Ty, Ty) -> R, Option<Ty>, Option<Ty>),
            mut s1: impl Iterator<Item = Field> + Clone,
            mut s2: impl Iterator<Item = Field> + Clone,
        ) -> (Vec<R>, ((List<Field>, Option<Ty>), (List<Field>, Option<Ty>))) {
            let lhs = s1.clone();
            let rhs = s2.clone();

            match (s1.next(), s2.next()) {
                | (None, _) => (Vec::new(), ((vec![].into(), t1), (rhs.collect(), t2))),
                | (_, None) => (Vec::new(), ((lhs.collect(), t1), (vec![].into(), t2))),
                | (Some(f1), Some(f2)) => {
                    if f1.name < f2.name {
                        let (vals, (mut lhs, rhs)) = go((db, f, t1, t2), s1, rhs);

                        lhs.0 = std::iter::once(f1).chain(lhs.0.iter().cloned()).collect();
                        (vals, (lhs, rhs))
                    } else if f2.name < f1.name {
                        let (vals, (lhs, mut rhs)) = go((db, f, t1, t2), lhs, s2);

                        rhs.0 = std::iter::once(f2).chain(rhs.0.iter().cloned()).collect();
                        (vals, (lhs, rhs))
                    } else {
                        let (mut vals, rest) = go((db, f, t1, t2), s1, s2);

                        vals.insert(0, f(f1.ty, f2.ty));
                        (vals, rest)
                    }
                },
            }
        }
    }
}

impl TyKind {
    pub fn intern(mut self, db: &dyn HirDatabase) -> Ty {
        if let TyKind::Row(fields, tail) = self {
            let mut fields = fields.to_vec();

            fields.sort_by_key(|f| f.name.clone());

            self = TyKind::Row(fields.into(), tail);
        }

        db.intern_ty(self)
    }
}

impl Constraint {
    pub fn new(class: ClassId, tys: impl IntoIterator<Item = Ty>) -> Self {
        Constraint {
            class,
            types: tys.into_iter().collect(),
        }
    }
}

impl Unknown {
    pub const fn from_raw(id: u32) -> Self {
        Unknown(id)
    }

    pub const fn raw(self) -> u32 {
        self.0
    }

    pub fn to_ty(self, db: &dyn HirDatabase) -> Ty {
        TyKind::Unknown(self).intern(db)
    }
}

impl TypeVar {
    pub const fn new(idx: u32, debruijn: DebruijnIndex) -> Self {
        Self(idx, debruijn)
    }

    pub fn to_ty(self, db: &dyn HirDatabase) -> Ty {
        TyKind::TypeVar(self).intern(db)
    }

    pub fn bound_within(self, debruijn: DebruijnIndex) -> bool {
        self.1.within(debruijn)
    }

    pub fn idx(self) -> u32 {
        self.0
    }

    pub fn debruijn(self) -> DebruijnIndex {
        self.1
    }

    #[must_use]
    pub fn shifted_in(self) -> Self {
        Self(self.0, self.1.shifted_in())
    }

    #[must_use]
    pub fn shifted_in_from(self, debruijn: DebruijnIndex) -> Self {
        Self(self.0, self.1.shifted_in_from(debruijn))
    }

    #[must_use]
    pub fn shifted_out(self) -> Option<Self> {
        Some(Self(self.0, self.1.shifted_out()?))
    }

    #[must_use]
    pub fn shifted_out_to(self, debruijn: DebruijnIndex) -> Option<Self> {
        Some(Self(self.0, self.1.shifted_out_to(debruijn)?))
    }
}

impl DebruijnIndex {
    pub const INNER: Self = Self(0);
    pub const ONE: Self = Self(1);

    pub const fn new(depth: u32) -> Self {
        Self(depth)
    }

    pub const fn depth(self) -> u32 {
        self.0
    }

    pub fn within(self, other: Self) -> bool {
        self.0 < other.0
    }

    #[must_use]
    pub fn shifted_in(self) -> Self {
        self.shifted_in_from(Self::ONE)
    }

    pub fn shift_in(&mut self) {
        *self = self.shifted_in();
    }

    #[must_use]
    pub fn shifted_in_from(self, other: Self) -> Self {
        Self(self.0 + other.0)
    }

    #[must_use]
    pub fn shifted_out(self) -> Option<Self> {
        self.shifted_out_to(Self::ONE)
    }

    pub fn shift_out(&mut self) {
        *self = self.shifted_out().unwrap();
    }

    #[must_use]
    pub fn shifted_out_to(self, other: Self) -> Option<Self> {
        if self.within(other) {
            None
        } else {
            Some(Self(self.0 - other.0))
        }
    }
}

impl salsa::InternKey for Ty {
    fn from_intern_id(v: salsa::InternId) -> Self {
        Self(v)
    }

    fn as_intern_id(&self) -> salsa::InternId {
        self.0
    }
}
