use crate::db::HirDatabase;
use crate::display::HirDisplay;
use hir_def::id::{ClassId, TypeCtorId};
use hir_def::name::Name;
use std::sync::Arc;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Ty(salsa::InternId);

pub type List<T> = Arc<[T]>;

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
    Tuple(List<Ty>),

    App(Ty, Ty),
    Ctnt(Constraint, Ty),
    ForAll(Ty, Ty),
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
pub struct TypeVar(DebruijnIndex);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct DebruijnIndex(u32);

impl Ty {
    pub fn lookup(self, db: &dyn HirDatabase) -> TyKind {
        db.lookup_intern_ty(self)
    }

    pub fn match_ctor<const A: usize>(mut self, db: &dyn HirDatabase, id: TypeCtorId) -> Option<[Ty; A]> {
        use std::mem::MaybeUninit;
        let mut res = [MaybeUninit::<Ty>::uninit(); A];

        for i in (0..A).rev() {
            if let TyKind::App(a, b) = self.lookup(db) {
                res[i] = MaybeUninit::new(b);
                self = a;
            } else {
                return None;
            };
        }

        if self.lookup(db) == TyKind::Ctor(id) {
            Some(unsafe { std::mem::transmute_copy(&res) })
        } else {
            None
        }
    }

    pub fn ret_type(mut self, db: &dyn HirDatabase, lib: base_db::libs::LibId) -> Ty {
        let func_type = db.lang_item(lib, "fn-type".into()).unwrap();
        let func_type = func_type.as_type_ctor().unwrap();

        while let Some([_, ret]) = self.match_ctor(db, func_type) {
            self = ret;
        }

        self
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
            | TyKind::Tuple(tys) => {
                let tys = tys.iter().map(|t| t.everywhere(db, f)).collect();

                f(TyKind::Tuple(tys).intern(db))
            },
            | TyKind::App(a, b) => {
                let a = a.everywhere(db, f);
                let b = b.everywhere(db, f);

                f(TyKind::App(a, b).intern(db))
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
                let k = k.everywhere(db, f);
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
            | TyKind::Tuple(tys) => {
                for ty in tys.iter() {
                    ty.everything(db, f);
                }
            },
            | TyKind::App(a, b) => {
                a.everything(db, f);
                b.everything(db, f);
            },
            | TyKind::Ctnt(ctnt, ty) => {
                for ty in ctnt.types.iter() {
                    ty.everything(db, f);
                }

                ty.everything(db, f);
            },
            | TyKind::ForAll(k, t) => {
                k.everything(db, f);
                t.everything(db, f);
            },
            | _ => {},
        }

        f(self)
    }

    pub fn normalize(self, db: &dyn HirDatabase) -> Ty {
        self.everywhere(db, &mut |ty| match ty.lookup(db) {
            | TyKind::App(a, b) => match a.lookup(db) {
                | TyKind::ForAll(_, a) => a.replace_var(db, b),
                | _ => ty,
            },
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

    pub fn replace_var(self, db: &dyn HirDatabase, with: Ty) -> Ty {
        Self::replace_var_impl(db, self, with, DebruijnIndex::INNER)
    }

    fn replace_var_impl(db: &dyn HirDatabase, ty: Ty, with: Ty, depth: DebruijnIndex) -> Ty {
        match ty.lookup(db) {
            | TyKind::TypeVar(var) if var.debruijn() == depth => with,
            | TyKind::Skolem(sk, k) => {
                let k = Self::replace_var_impl(db, k, with, depth);

                TyKind::Skolem(sk, k).intern(db)
            },
            | TyKind::Row(fields, tail) => {
                let fields = fields
                    .iter()
                    .map(|f| Field {
                        name: f.name.clone(),
                        ty: Self::replace_var_impl(db, f.ty, with, depth),
                    })
                    .collect();

                let tail = tail.map(|t| Self::replace_var_impl(db, t, with, depth));

                TyKind::Row(fields, tail).intern(db)
            },
            | TyKind::Tuple(tys) => {
                let tys = tys
                    .iter()
                    .map(|&t| Self::replace_var_impl(db, t, with, depth))
                    .collect();

                TyKind::Tuple(tys).intern(db)
            },
            | TyKind::App(a, b) => {
                let a = Self::replace_var_impl(db, a, with, depth);
                let b = Self::replace_var_impl(db, b, with, depth);

                TyKind::App(a, b).intern(db)
            },
            | TyKind::Ctnt(ctnt, ty) => {
                let ctnt = Constraint {
                    class: ctnt.class,
                    types: ctnt
                        .types
                        .iter()
                        .map(|&t| Self::replace_var_impl(db, t, with, depth))
                        .collect(),
                };

                let ty = Self::replace_var_impl(db, ty, with, depth);

                TyKind::Ctnt(ctnt, ty).intern(db)
            },
            | TyKind::ForAll(k, inner) => {
                let k = Self::replace_var_impl(db, k, with, depth);
                let inner = Self::replace_var_impl(db, inner, with, depth.shifted_in());

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
    pub const fn new(debruijn: DebruijnIndex) -> Self {
        Self(debruijn)
    }

    pub fn to_ty(self, db: &dyn HirDatabase) -> Ty {
        TyKind::TypeVar(self).intern(db)
    }

    pub fn bound_within(self, debruijn: DebruijnIndex) -> bool {
        self.0.within(debruijn)
    }

    pub fn debruijn(self) -> DebruijnIndex {
        self.0
    }

    #[must_use]
    pub fn shifted_in(self) -> Self {
        Self(self.0.shifted_in())
    }

    #[must_use]
    pub fn shifted_in_from(self, debruijn: DebruijnIndex) -> Self {
        Self(self.0.shifted_in_from(debruijn))
    }

    #[must_use]
    pub fn shifted_out(self) -> Option<Self> {
        self.0.shifted_out().map(Self)
    }

    #[must_use]
    pub fn shifted_out_to(self, debruijn: DebruijnIndex) -> Option<Self> {
        self.0.shifted_out_to(debruijn).map(Self)
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
