use crate::{
    db::HirDatabase,
    infer::ExprOrPatId,
    ty::{List, TypeVar, WhereClause},
};
use arena::{Arena, ArenaMap, Idx};
use hir_def::{
    expr::ExprId,
    id::{ClassId, TypeCtorId, TypeVarOwner, TypedDefId},
    name::Name,
    pat::PatId,
    type_ref::{LocalTypeRefId, LocalTypeVarId},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TyId(Idx<TyInfo>);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TyInfo {
    Error,

    Unknown(Unknown),
    Skolem(TypeVar, TyId),
    TypeVar(TypeVar),

    Figure(i128),
    Symbol(Box<str>),
    Row(List<FieldInfo>, Option<TyId>),

    Ctor(TypeCtorId),
    App(TyId, List<TyId>),

    Tuple(List<TyId>),
    Func(List<TyId>, TyId),

    Where(WhereClause<CtntInfo>, TyId),
    ForAll(List<TyId>, TyId, TypeVarScopeId),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Unknown(u32);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FieldInfo {
    pub name: Name,
    pub ty: TyId,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CtntInfo {
    pub class: ClassId,
    pub types: List<TyId>,
}

pub type TySource = (TypeVarOwner, TypeOrigin);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TypeOrigin {
    ExprId(ExprId),
    PatId(PatId),
    TypeRefId(LocalTypeRefId),
    TypeVarId(LocalTypeVarId),
    Def(TypedDefId),
    Synthetic,
}

#[derive(Default, Debug)]
pub struct TypeVars {
    scopes: Arena<TypeVarScope>,
    current: Vec<TypeVarScopeId>,
}

#[derive(Debug)]
pub struct TypeVarScope {
    var_kinds: List<TyId>,
}

pub type TypeVarScopeId = Idx<TypeVarScope>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeScopeSet(u32);

#[derive(Default)]
pub struct Types {
    types: Arena<TyInfo>,
    sources: ArenaMap<Idx<TyInfo>, TySource>,
}

pub trait ToInfo {
    type Output;

    fn to_info(self, db: &dyn HirDatabase, types: &mut Types, src: TySource) -> Self::Output;
}

pub trait FromInfo {
    type Input;

    fn from_info(db: &dyn HirDatabase, types: &Types, input: Self::Input) -> Self;
}

pub struct TyDisplay<'a> {
    db: &'a dyn crate::db::HirDatabase,
    types: &'a Types,
    ty: TyId,
    lhs_exposed: bool,
}

impl TypeVars {
    pub fn alloc_scope(&mut self, var_kinds: List<TyId>) -> TypeVarScopeId {
        let id = self.scopes.alloc(TypeVarScope { var_kinds });

        self.push_scope(id);
        id
    }

    pub fn push_scope(&mut self, id: TypeVarScopeId) {
        self.current.push(id);
    }

    pub fn pop_scope(&mut self) {
        self.current.pop().unwrap();
    }

    pub fn scope_at(&self, depth: usize) -> TypeVarScopeId {
        self.current[self.current.len() - depth - 1]
    }

    pub fn var_kinds(&self, id: TypeVarScopeId) -> &List<TyId> {
        &self.scopes[id].var_kinds
    }
}

impl Types {
    pub fn insert(&mut self, ty: TyInfo, src: TySource) -> TyId {
        let id = self.types.alloc(ty);
        self.sources.insert(id, src);
        TyId(id)
    }

    pub fn update(&mut self, id: TyId, ty: TyInfo) -> TyId {
        if self[id] == ty {
            id
        } else {
            let span = self.source(id);
            self.insert(ty, span)
        }
    }

    pub fn override_(&mut self, id: TyId, ty: TyInfo) -> TyId {
        self.types[id.0] = ty;
        id
    }

    pub fn source(&self, id: TyId) -> TySource {
        self.sources[id.0]
    }
}

impl std::ops::Index<TyId> for Types {
    type Output = TyInfo;

    fn index(&self, id: TyId) -> &Self::Output {
        &self.types[id.0]
    }
}

impl Unknown {
    pub const fn from_raw(id: u32) -> Self {
        Unknown(id)
    }

    pub const fn raw(self) -> u32 {
        self.0
    }

    pub fn to_ty(self, types: &mut Types, span: TySource) -> TyId {
        types.insert(TyInfo::Unknown(self), span)
    }
}

impl TyId {
    pub fn display<'a>(self, db: &'a dyn crate::db::HirDatabase, types: &'a Types) -> TyDisplay<'a> {
        TyDisplay {
            db,
            types,
            ty: self,
            lhs_exposed: false,
        }
    }

    pub fn match_ctor(self, types: &Types, id: TypeCtorId) -> Option<List<TyId>> {
        if let TyInfo::App(ctor, ref args) = types[self] {
            if types[ctor] == TyInfo::Ctor(id) {
                Some(args.clone())
            } else {
                None
            }
        } else {
            None
        }
    }

    pub fn everywhere<F>(self, types: &mut Types, f: &mut F) -> TyId
    where
        F: FnMut(&mut Types, TyId) -> TyId,
    {
        match types[self].clone() {
            | TyInfo::Skolem(sk, k) => {
                let k = k.everywhere(types, f);
                let ty = types.update(self, TyInfo::Skolem(sk, k));

                f(types, ty)
            },
            | TyInfo::Row(fields, tail) => {
                let fields = fields
                    .iter()
                    .map(|field| FieldInfo {
                        name: field.name.clone(),
                        ty: field.ty.everywhere(types, f),
                    })
                    .collect();

                let tail = tail.map(|t| t.everywhere(types, f));
                let ty = types.update(self, TyInfo::Row(fields, tail));

                f(types, ty)
            },
            | TyInfo::App(base, args) => {
                let base = base.everywhere(types, f);
                let args = args.iter().map(|t| t.everywhere(types, f)).collect();
                let ty = types.update(self, TyInfo::App(base, args));

                f(types, ty)
            },
            | TyInfo::Tuple(tys) => {
                let tys = tys.iter().map(|t| t.everywhere(types, f)).collect();
                let ty = types.update(self, TyInfo::Tuple(tys));

                f(types, ty)
            },
            | TyInfo::Func(args, ret) => {
                let args = args.iter().map(|t| t.everywhere(types, f)).collect();
                let ret = ret.everywhere(types, f);
                let ty = types.update(self, TyInfo::Func(args, ret));

                f(types, ty)
            },
            | TyInfo::Where(where_, inner) => {
                let where_ = WhereClause {
                    constraints: where_
                        .constraints
                        .iter()
                        .map(|c| CtntInfo {
                            class: c.class,
                            types: c.types.iter().map(|t| t.everywhere(types, f)).collect(),
                        })
                        .collect(),
                };

                let inner = inner.everywhere(types, f);
                let ty = types.update(self, TyInfo::Where(where_, inner));

                f(types, ty)
            },
            | TyInfo::ForAll(k, t, s) => {
                let k = k.iter().map(|k| k.everywhere(types, f)).collect();
                let t = t.everywhere(types, f);
                let ty = types.update(self, TyInfo::ForAll(k, t, s));

                f(types, ty)
            },
            | _ => f(types, self),
        }
    }

    pub fn everywhere_override<F>(self, types: &mut Types, f: &mut F) -> TyId
    where
        F: FnMut(&mut Types, TyId) -> TyId,
    {
        match types[self].clone() {
            | TyInfo::Skolem(sk, k) => {
                let k = k.everywhere(types, f);
                let ty = types.override_(self, TyInfo::Skolem(sk, k));

                f(types, ty)
            },
            | TyInfo::Row(fields, tail) => {
                let fields = fields
                    .iter()
                    .map(|field| FieldInfo {
                        name: field.name.clone(),
                        ty: field.ty.everywhere(types, f),
                    })
                    .collect();

                let tail = tail.map(|t| t.everywhere(types, f));
                let ty = types.override_(self, TyInfo::Row(fields, tail));

                f(types, ty)
            },
            | TyInfo::App(base, args) => {
                let base = base.everywhere(types, f);
                let args = args.iter().map(|t| t.everywhere(types, f)).collect();
                let ty = types.override_(self, TyInfo::App(base, args));

                f(types, ty)
            },
            | TyInfo::Tuple(tys) => {
                let tys = tys.iter().map(|t| t.everywhere(types, f)).collect();
                let ty = types.override_(self, TyInfo::Tuple(tys));

                f(types, ty)
            },
            | TyInfo::Func(args, ret) => {
                let args = args.iter().map(|t| t.everywhere(types, f)).collect();
                let ret = ret.everywhere(types, f);
                let ty = types.override_(self, TyInfo::Func(args, ret));

                f(types, ty)
            },
            | TyInfo::Where(where_, inner) => {
                let where_ = WhereClause {
                    constraints: where_
                        .constraints
                        .iter()
                        .map(|c| CtntInfo {
                            class: c.class,
                            types: c.types.iter().map(|t| t.everywhere(types, f)).collect(),
                        })
                        .collect(),
                };

                let inner = inner.everywhere(types, f);
                let ty = types.override_(self, TyInfo::Where(where_, inner));

                f(types, ty)
            },
            | TyInfo::ForAll(k, t, s) => {
                let k = k.iter().map(|k| k.everywhere(types, f)).collect();
                let t = t.everywhere(types, f);
                let ty = types.override_(self, TyInfo::ForAll(k, t, s));

                f(types, ty)
            },
            | _ => f(types, self),
        }
    }

    pub fn everything<F>(self, types: &Types, f: &mut F)
    where
        F: FnMut(TyId),
    {
        match types[self] {
            | TyInfo::Skolem(_, k) => k.everything(types, f),
            | TyInfo::Row(ref fields, tail) => {
                for field in fields.iter() {
                    field.ty.everything(types, f);
                }

                if let Some(tail) = tail {
                    tail.everything(types, f);
                }
            },
            | TyInfo::App(base, ref args) => {
                base.everything(types, f);

                for ty in args.iter() {
                    ty.everything(types, f);
                }
            },
            | TyInfo::Tuple(ref tys) => {
                for ty in tys.iter() {
                    ty.everything(types, f);
                }
            },
            | TyInfo::Func(ref args, ret) => {
                for ty in args.iter() {
                    ty.everything(types, f);
                }

                ret.everything(types, f);
            },
            | TyInfo::Where(ref where_, ty) => {
                for ctnt in where_.constraints.iter() {
                    for ty in ctnt.types.iter() {
                        ty.everything(types, f);
                    }
                }

                ty.everything(types, f);
            },
            | TyInfo::ForAll(ref k, t, _) => {
                for ty in k.iter() {
                    ty.everything(types, f);
                }

                t.everything(types, f);
            },
            | _ => {},
        }

        f(self)
    }

    pub fn normalize(self, types: &mut Types) -> TyId {
        self.everywhere(types, &mut |types, ty| match types[ty] {
            | TyInfo::Row(ref f1, Some(tail)) => match types[tail] {
                | TyInfo::Row(ref f2, None) => {
                    let fields = f1.iter().cloned().chain(f2.iter().cloned()).collect();

                    types.update(ty, TyInfo::Row(fields, None))
                },
                | TyInfo::TypeVar(_) | TyInfo::Unknown(_) | TyInfo::Error => ty,
                | _ => unreachable!("{:?}", tail),
            },
            | TyInfo::Where(ref w1, t1) => match types[t1] {
                | TyInfo::Where(ref w2, t2) => {
                    let where_clause = WhereClause {
                        constraints: w2.constraints.iter().chain(w1.constraints.iter()).cloned().collect(),
                    };

                    types.update(ty, TyInfo::Where(where_clause, t2))
                },
                | TyInfo::ForAll(ref kinds, inner, scope) => {
                    let w1 = w1.clone();
                    let kinds = kinds.clone();
                    let inner = types.update(inner, TyInfo::Where(w1, inner));

                    types.update(ty, TyInfo::ForAll(kinds, inner, scope))
                },
                | _ => ty,
            },
            | _ => ty,
        })
    }

    pub fn replace_vars(self, types: &mut Types, with: &[TyId], scope: TypeVarScopeId) -> TyId {
        match types[self].clone() {
            | TyInfo::TypeVar(var) if var.scope() == scope => with[var.idx() as usize],
            | TyInfo::Skolem(sk, k) => {
                let k = k.replace_vars(types, with, scope);

                types.update(self, TyInfo::Skolem(sk, k))
            },
            | TyInfo::Row(fields, tail) => {
                let fields = fields
                    .iter()
                    .map(|f| FieldInfo {
                        name: f.name.clone(),
                        ty: f.ty.replace_vars(types, with, scope),
                    })
                    .collect();

                let tail = tail.map(|t| t.replace_vars(types, with, scope));

                types.update(self, TyInfo::Row(fields, tail))
            },
            | TyInfo::App(base, args) => {
                let base = base.replace_vars(types, with, scope);
                let args = args.iter().map(|t| t.replace_vars(types, with, scope)).collect();

                types.update(self, TyInfo::App(base, args))
            },
            | TyInfo::Tuple(tys) => {
                let tys = tys.iter().map(|t| t.replace_vars(types, with, scope)).collect();

                types.update(self, TyInfo::Tuple(tys))
            },
            | TyInfo::Func(args, ret) => {
                let args = args.iter().map(|t| t.replace_vars(types, with, scope)).collect();
                let ret = ret.replace_vars(types, with, scope);

                types.update(self, TyInfo::Func(args, ret))
            },
            | TyInfo::Where(where_, inner) => {
                let where_ = WhereClause {
                    constraints: where_
                        .constraints
                        .iter()
                        .map(|c| CtntInfo {
                            class: c.class,
                            types: c.types.iter().map(|t| t.replace_vars(types, with, scope)).collect(),
                        })
                        .collect(),
                };

                let inner = inner.replace_vars(types, with, scope);

                types.update(self, TyInfo::Where(where_, inner))
            },
            | TyInfo::ForAll(k, inner, s) => {
                let k = k.iter().map(|k| k.replace_vars(types, with, scope)).collect();
                let inner = inner.replace_vars(types, with, scope);

                types.update(self, TyInfo::ForAll(k, inner, s))
            },
            | _ => self,
        }
    }

    pub fn to_row_list(self, types: &Types) -> (List<FieldInfo>, Option<TyId>) {
        match types[self] {
            | TyInfo::Row(ref fields, tail) => (fields.clone(), tail),
            | _ => (vec![].into(), None),
        }
    }

    pub fn align_rows_with<R>(
        types: &Types,
        mut f: impl FnMut(TyId, TyId) -> R,
        t1: TyId,
        t2: TyId,
    ) -> (
        Vec<R>,
        ((List<FieldInfo>, Option<TyId>), (List<FieldInfo>, Option<TyId>)),
    ) {
        let (s1, tail1) = t1.to_row_list(types);
        let (s2, tail2) = t2.to_row_list(types);

        return go((types, &mut f, tail1, tail2), s1.iter().cloned(), s2.iter().cloned());

        fn go<R>(
            (types, f, t1, t2): (&Types, &mut impl FnMut(TyId, TyId) -> R, Option<TyId>, Option<TyId>),
            mut s1: impl Iterator<Item = FieldInfo> + Clone,
            mut s2: impl Iterator<Item = FieldInfo> + Clone,
        ) -> (
            Vec<R>,
            ((List<FieldInfo>, Option<TyId>), (List<FieldInfo>, Option<TyId>)),
        ) {
            let lhs = s1.clone();
            let rhs = s2.clone();

            match (s1.next(), s2.next()) {
                | (None, _) => (Vec::new(), ((vec![].into(), t1), (rhs.collect(), t2))),
                | (_, None) => (Vec::new(), ((lhs.collect(), t1), (vec![].into(), t2))),
                | (Some(f1), Some(f2)) => {
                    if f1.name < f2.name {
                        let (vals, (mut lhs, rhs)) = go((types, f, t1, t2), s1, rhs);

                        lhs.0 = std::iter::once(f1).chain(lhs.0.iter().cloned()).collect();
                        (vals, (lhs, rhs))
                    } else if f2.name < f1.name {
                        let (vals, (lhs, mut rhs)) = go((types, f, t1, t2), lhs, s2);

                        rhs.0 = std::iter::once(f2).chain(rhs.0.iter().cloned()).collect();
                        (vals, (lhs, rhs))
                    } else {
                        let (mut vals, rest) = go((types, f, t1, t2), s1, s2);

                        vals.insert(0, f(f1.ty, f2.ty));
                        (vals, rest)
                    }
                },
            }
        }
    }
}

impl From<ExprId> for TypeOrigin {
    fn from(id: ExprId) -> Self {
        Self::ExprId(id)
    }
}

impl From<PatId> for TypeOrigin {
    fn from(id: PatId) -> Self {
        Self::PatId(id)
    }
}

impl From<LocalTypeRefId> for TypeOrigin {
    fn from(id: LocalTypeRefId) -> Self {
        Self::TypeRefId(id)
    }
}

impl From<LocalTypeVarId> for TypeOrigin {
    fn from(id: LocalTypeVarId) -> Self {
        Self::TypeVarId(id)
    }
}

impl From<ExprOrPatId> for TypeOrigin {
    fn from(id: ExprOrPatId) -> Self {
        match id {
            | ExprOrPatId::ExprId(id) => Self::ExprId(id),
            | ExprOrPatId::PatId(id) => Self::PatId(id),
        }
    }
}

impl TyDisplay<'_> {
    fn with_ty(&self, ty: TyId, lhs_exposed: bool) -> Self {
        Self {
            ty,
            lhs_exposed,
            db: self.db,
            types: self.types,
        }
    }
}

impl std::fmt::Display for TyDisplay<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.types[self.ty] {
            | TyInfo::Error => f.write_str("{error}"),
            | TyInfo::Unknown(u) => u.fmt(f),
            | TyInfo::Skolem(tv, k) => write!(f, "({} :: {})", tv, self.with_ty(k, false)),
            | TyInfo::TypeVar(tv) => tv.fmt(f),
            | TyInfo::Figure(i) => i.fmt(f),
            | TyInfo::Symbol(ref s) => s.fmt(f),
            | TyInfo::Row(ref fields, Some(tail)) => {
                write!(
                    f,
                    "#({} | {})",
                    fields
                        .iter()
                        .map(|f| format!("{} :: {}", f.name, self.with_ty(f.ty, false)))
                        .collect::<Vec<_>>()
                        .join(", "),
                    self.with_ty(tail, false)
                )
            },
            | TyInfo::Row(ref fields, None) => {
                write!(
                    f,
                    "#({})",
                    fields
                        .iter()
                        .map(|f| format!("{} :: {}", f.name, self.with_ty(f.ty, false)))
                        .collect::<Vec<_>>()
                        .join(", "),
                )
            },
            | TyInfo::Ctor(id) => self.db.type_ctor_data(id).name.fmt(f),
            | TyInfo::App(base, ref args) if self.lhs_exposed => write!(
                f,
                "({} {})",
                self.with_ty(base, false),
                args.iter()
                    .map(|&a| format!("{}", self.with_ty(a, true)))
                    .collect::<Vec<_>>()
                    .join(" ")
            ),
            | TyInfo::App(base, ref args) => write!(
                f,
                "{} {}",
                self.with_ty(base, false),
                args.iter()
                    .map(|&a| format!("{}", self.with_ty(a, true)))
                    .collect::<Vec<_>>()
                    .join(" ")
            ),
            | TyInfo::Tuple(ref tys) => write!(
                f,
                "({}{})",
                tys.iter()
                    .map(|&t| format!("{}", self.with_ty(t, false)))
                    .collect::<Vec<_>>()
                    .join(", "),
                if tys.len() == 1 { "," } else { "" }
            ),
            | TyInfo::Func(ref args, ret) if self.lhs_exposed => write!(
                f,
                "({}{}-> {})",
                args.iter()
                    .map(|&a| format!("{}", self.with_ty(a, true)))
                    .collect::<Vec<_>>()
                    .join(", "),
                if args.is_empty() { "" } else { " " },
                self.with_ty(ret, true),
            ),
            | TyInfo::Func(ref args, ret) => write!(
                f,
                "{}{}-> {}",
                args.iter()
                    .map(|&a| format!("{}", self.with_ty(a, true)))
                    .collect::<Vec<_>>()
                    .join(", "),
                if args.is_empty() { "" } else { " " },
                self.with_ty(ret, true),
            ),
            | TyInfo::Where(ref where_, ty) if self.lhs_exposed => write!(
                f,
                "({} where{})",
                self.with_ty(ty, true),
                where_
                    .constraints
                    .iter()
                    .map(|c| format!(
                        " {}{}",
                        self.db.class_data(c.class).name,
                        c.types
                            .iter()
                            .map(|&t| format!(" {}", self.with_ty(t, true)))
                            .collect::<Vec<_>>()
                            .join(""),
                    ))
                    .collect::<Vec<_>>()
                    .join(""),
            ),
            | TyInfo::Where(ref where_, ty) => write!(
                f,
                "{} where{}",
                self.with_ty(ty, true),
                where_
                    .constraints
                    .iter()
                    .map(|c| format!(
                        " {}{}",
                        self.db.class_data(c.class).name,
                        c.types
                            .iter()
                            .map(|&t| format!(" {}", self.with_ty(t, true)))
                            .collect::<Vec<_>>()
                            .join(""),
                    ))
                    .collect::<Vec<_>>()
                    .join(""),
            ),
            | TyInfo::ForAll(ref kinds, ty, scope) if self.lhs_exposed => {
                let scope: u32 = scope.into_raw().into();
                let scope = unsafe { std::char::from_u32_unchecked('a' as u32 + scope) };

                write!(
                    f,
                    "(for{}. {})",
                    kinds
                        .iter()
                        .enumerate()
                        .map(|(i, &t)| format!(" ({}{} :: {})", scope, i, self.with_ty(t, true)))
                        .collect::<Vec<_>>()
                        .join(""),
                    self.with_ty(ty, false),
                )
            },
            | TyInfo::ForAll(ref kinds, ty, scope) => {
                let scope: u32 = scope.into_raw().into();
                let scope = unsafe { std::char::from_u32_unchecked('a' as u32 + scope) };

                write!(
                    f,
                    "for{}. {}",
                    kinds
                        .iter()
                        .enumerate()
                        .map(|(i, &t)| format!(" ({}{} :: {})", scope, i, self.with_ty(t, true)))
                        .collect::<Vec<_>>()
                        .join(""),
                    self.with_ty(ty, false),
                )
            },
        }
    }
}
