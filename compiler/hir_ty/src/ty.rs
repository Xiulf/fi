use crate::db::HirDatabase;
use crate::info::{CtntInfo, FieldInfo, FromInfo, Span, ToInfo, TyId, TyInfo, Types};
use hir_def::id::{ClassId, TypeCtorId};
use hir_def::name::Name;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Ty(salsa::InternId);

pub type List<T> = Box<[T]>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TyKind {
    Error,

    Figure(i128),
    Symbol(Box<str>),
    Row(List<Field>, Option<Ty>),

    Ctor(TypeCtorId),
    App(Ty, List<Ty>),

    Tuple(List<Ty>),
    Func(List<Ty>, Ty),

    Ctnt(Constraint, Ty),
    ForAll(List<Ty>, Ty),
    TypeVar(TypeVar),
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
pub struct TypeVar(u32, DebruijnIndex);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct DebruijnIndex(u32);

impl Ty {
    pub fn lookup(self, db: &dyn HirDatabase) -> TyKind {
        db.lookup_intern_ty(self)
    }

    pub fn match_ctor(self, db: &dyn HirDatabase, id: TypeCtorId) -> Option<List<Ty>> {
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
}

impl ToInfo for Ty {
    type Output = TyId;

    fn to_info(self, db: &dyn HirDatabase, types: &mut Types, span: Span) -> Self::Output {
        let info = match self.lookup(db) {
            | TyKind::Error => TyInfo::Error,
            | TyKind::Figure(f) => TyInfo::Figure(f),
            | TyKind::Symbol(s) => TyInfo::Symbol(s),
            | TyKind::Row(fields, tail) => {
                let fields = fields
                    .iter()
                    .map(|f| FieldInfo {
                        name: f.name.clone(),
                        ty: f.ty.to_info(db, types, span),
                    })
                    .collect();

                let tail = tail.map(|t| t.to_info(db, types, span));

                TyInfo::Row(fields, tail)
            },
            | TyKind::Ctor(id) => TyInfo::Ctor(id),
            | TyKind::App(base, args) => {
                let base = base.to_info(db, types, span);
                let args = args.iter().map(|a| a.to_info(db, types, span)).collect();

                TyInfo::App(base, args)
            },
            | TyKind::Tuple(tys) => {
                let tys = tys.iter().map(|t| t.to_info(db, types, span)).collect();

                TyInfo::Tuple(tys)
            },
            | TyKind::Func(args, ret) => {
                let args = args.iter().map(|a| a.to_info(db, types, span)).collect();
                let ret = ret.to_info(db, types, span);

                TyInfo::Func(args, ret)
            },
            | TyKind::Ctnt(ctnt, inner) => {
                let ctnt = CtntInfo {
                    class: ctnt.class,
                    types: ctnt.types.iter().map(|t| t.to_info(db, types, span)).collect(),
                };

                let inner = inner.to_info(db, types, span);

                TyInfo::Ctnt(ctnt, inner)
            },
            | TyKind::ForAll(vars, ret) => {
                let vars = vars.iter().map(|v| v.to_info(db, types, span)).collect();
                let ret = ret.to_info(db, types, span);

                TyInfo::ForAll(vars, ret)
            },
            | TyKind::TypeVar(tv) => TyInfo::TypeVar(tv),
        };

        types.insert(info, span)
    }
}

impl FromInfo for Ty {
    type Input = TyId;

    fn from_info(db: &dyn HirDatabase, types: &Types, id: TyId) -> Self {
        let kind = match types[id] {
            | TyInfo::Error | TyInfo::Unknown(_) | TyInfo::Skolem(_, _) => TyKind::Error,
            | TyInfo::TypeVar(tv) => TyKind::TypeVar(tv),
            | TyInfo::Figure(f) => TyKind::Figure(f),
            | TyInfo::Symbol(ref s) => TyKind::Symbol(s.clone()),
            | TyInfo::Row(ref fields, tail) => {
                let fields = fields
                    .iter()
                    .map(|f| Field {
                        name: f.name.clone(),
                        ty: Self::from_info(db, types, f.ty),
                    })
                    .collect();

                let tail = tail.map(|t| Self::from_info(db, types, t));

                TyKind::Row(fields, tail)
            },
            | TyInfo::Ctor(id) => TyKind::Ctor(id),
            | TyInfo::App(base, ref args) => {
                let base = Self::from_info(db, types, base);
                let args = args.iter().map(|&a| Self::from_info(db, types, a)).collect();

                TyKind::App(base, args)
            },
            | TyInfo::Tuple(ref tys) => {
                let tys = tys.iter().map(|&t| Self::from_info(db, types, t)).collect();

                TyKind::Tuple(tys)
            },
            | TyInfo::Func(ref args, ret) => {
                let args = args.iter().map(|&a| Self::from_info(db, types, a)).collect();
                let ret = Self::from_info(db, types, ret);

                TyKind::Func(args, ret)
            },
            | TyInfo::Ctnt(ref ctnt, inner) => {
                let ctnt = Constraint {
                    class: ctnt.class,
                    types: ctnt.types.iter().map(|&t| Self::from_info(db, types, t)).collect(),
                };

                let inner = Self::from_info(db, types, inner);

                TyKind::Ctnt(ctnt, inner)
            },
            | TyInfo::ForAll(ref vars, inner) => {
                let vars = vars.iter().map(|&v| Self::from_info(db, types, v)).collect();
                let inner = Self::from_info(db, types, inner);

                TyKind::ForAll(vars, inner)
            },
        };

        kind.intern(db)
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
