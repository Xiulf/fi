use std::collections::HashMap;

use arena::ArenaMap;
use either::Either;
use hir_def::body::Body;
use hir_def::expr::ExprId;
use hir_def::id::{HasModule, TypeCtorId, TypeVarId, TypedItemId};
use hir_def::lang_item::{self, LangItem};
use hir_def::name::AsName;
use hir_def::pat::PatId;
use hir_def::type_ref::TypeRefId;
use parking_lot::RwLock;
use ra_ap_stdx::hash::{NoHashHashMap, NoHashHashSet};
use rustc_hash::FxHashMap;
use triomphe::Arc;

use crate::ty::{Constraint, ConstraintOrigin, FloatKind, GeneralizedType, IntegerKind, Ty, TyKind, Unknown};
use crate::unify::{Substitution, UnkLevel};
use crate::Db;

pub struct Ctx<'db> {
    pub(crate) db: &'db dyn Db,
    pub(crate) result: InferResult,
    pub(crate) subst: Substitution,
    pub(crate) owner: TypedItemId,
    pub(crate) level: UnkLevel,
    pub(crate) ret_ty: Ty,
    pub(crate) constraints: Vec<(Constraint, ConstraintOrigin)>,
}

pub struct BodyCtx<'db, 'ctx> {
    pub(crate) ctx: &'ctx mut Ctx<'db>,
    pub(crate) body: Arc<Body>,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct InferResult {
    pub ty: GeneralizedType,
    pub constraints: Vec<Constraint>,
    pub type_of_expr: ArenaMap<ExprId, Ty>,
    pub type_of_pat: ArenaMap<PatId, Ty>,
    pub kind_of_ty: ArenaMap<TypeRefId, Ty>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Expectation {
    None,
    HasType(Ty),
}

#[derive(Default, Debug)]
pub struct Cache(RwLock<CacheInner>);

#[derive(Default, Debug)]
pub struct CacheInner {
    pub ctor_int_kind: NoHashHashMap<TypeCtorId, Option<IntegerKind>>,
    pub int_kind_ctor: FxHashMap<IntegerKind, TypeCtorId>,
    pub ctor_float_kind: NoHashHashMap<TypeCtorId, Option<FloatKind>>,
    pub float_kind_ctor: FxHashMap<FloatKind, TypeCtorId>,
}

impl InferResult {
    pub fn default(db: &dyn Db) -> Self {
        let ty = Ty::new(db, TyKind::Error);

        InferResult {
            ty: GeneralizedType::Mono(ty),
            constraints: Default::default(),
            type_of_expr: Default::default(),
            type_of_pat: Default::default(),
            kind_of_ty: Default::default(),
        }
    }
}

impl<'db> Ctx<'db> {
    pub fn new(db: &'db dyn Db, owner: TypedItemId) -> Self {
        let ty = Ty::new(db, TyKind::Error);

        Self {
            db,
            owner,
            result: InferResult::default(db),
            subst: Substitution::default(),
            level: UnkLevel(1),
            ret_ty: ty,
            constraints: Vec::new(),
        }
    }

    pub fn with_body(&mut self, body: Arc<Body>) -> BodyCtx<'db, '_> {
        BodyCtx { ctx: self, body }
    }

    pub fn finish(mut self) -> Arc<InferResult> {
        let mut result = std::mem::replace(&mut self.result, InferResult::default(self.db));
        let mut finalize = |t: &mut Ty| {
            *t = self.resolve_type_fully(*t);
        };

        result.constraints.iter_mut().for_each(|c| {
            c.args.iter_mut().for_each(&mut finalize);
        });

        result.constraints.sort_unstable();
        result.constraints.dedup();

        result.type_of_expr.values_mut().for_each(&mut finalize);
        result.type_of_pat.values_mut().for_each(&mut finalize);
        result.ty = self.resolve_generalized_type_fully(result.ty);

        Arc::new(result)
    }

    pub fn error(&self) -> Ty {
        Ty::new(self.db, TyKind::Error)
    }

    pub fn type_kind(&self) -> Ty {
        match self.lang_ctor(lang_item::TYPE_KIND) {
            | Some(type_kind) => Ty::new(self.db, TyKind::Ctor(type_kind)),
            | None => self.error(),
        }
    }

    pub fn unit_type(&self) -> Ty {
        match self.lang_ctor(lang_item::UNIT_TYPE) {
            | Some(unit_type) => Ty::new(self.db, TyKind::Ctor(unit_type)),
            | None => self.error(),
        }
    }

    pub fn bool_type(&self) -> Ty {
        match self.lang_ctor(lang_item::BOOL_TYPE) {
            | Some(bool_type) => Ty::new(self.db, TyKind::Ctor(bool_type)),
            | None => self.error(),
        }
    }

    pub fn int_type(&self) -> Ty {
        match self.lang_ctor(lang_item::INT_TYPE) {
            | Some(int_type) => Ty::new(self.db, TyKind::Ctor(int_type)),
            | None => self.error(),
        }
    }

    pub fn float_type(&self) -> Ty {
        match self.lang_ctor(lang_item::FLOAT_TYPE) {
            | Some(float_type) => Ty::new(self.db, TyKind::Ctor(float_type)),
            | None => self.error(),
        }
    }

    fn lang_item(&self, name: &'static str) -> Option<LangItem> {
        let lib = self.owner.module(self.db).lib(self.db);
        lang_item::query(self.db, lib, name)
    }

    fn lang_ctor(&self, name: &'static str) -> Option<TypeCtorId> {
        self.lang_item(name).and_then(LangItem::as_type_ctor)
    }

    // fn lang_trait(&self, name: &'static str) -> Option<TraitId> {
    //     self.lang_item(name).and_then(LangItem::as_trait)
    // }

    pub fn constrain(&mut self, constraint: Constraint, origin: ConstraintOrigin) {
        self.constraints.push((constraint, origin));
    }

    pub fn tuple_type(&mut self, mut types: Vec<Ty>) -> Ty {
        match types.len() {
            | 0 => self.unit_type(),
            | 1 => types.remove(0),
            | _ => {
                let pair_type = match self.lang_ctor(lang_item::PAIR_TYPE) {
                    | Some(pair_type) => Ty::new(self.db, TyKind::Ctor(pair_type)),
                    | None => self.error(),
                };

                let last = types.remove(types.len() - 1);

                types
                    .into_iter()
                    .rfold(last, |r, l| Ty::new(self.db, TyKind::App(pair_type, Box::new([l, r]))))
            },
        }
    }

    pub fn instantiate(
        &mut self,
        ty: GeneralizedType,
        constraints: Vec<Constraint>,
        skolem: bool,
    ) -> (Ty, Vec<Constraint>) {
        match ty {
            | GeneralizedType::Mono(ty) => (ty, constraints),
            | GeneralizedType::Poly(vars, ty) => {
                let mut replacements = HashMap::default();

                for &var in vars.iter() {
                    replacements.insert(var, self.fresh_type(self.level, skolem));
                }

                let constraints = constraints
                    .into_iter()
                    .map(|c| Constraint {
                        trait_id: c.trait_id,
                        args: c.args.iter().map(|a| a.replace_vars(self.db, &replacements)).collect(),
                    })
                    .collect();

                (ty.replace_vars(self.db, &replacements), constraints)
            },
        }
    }

    pub fn generalize(&mut self, ty: Ty, type_vars: &[TypeVarId]) -> GeneralizedType {
        let mut vars = NoHashHashSet::default();
        self.find_all_unknowns(ty, &mut vars);
        let vars = self.new_type_vars(vars);

        if vars.is_empty() && type_vars.is_empty() {
            GeneralizedType::Mono(ty)
        } else {
            let mut type_vars = type_vars.to_vec();

            for (u, var) in vars {
                self.subst.solved.0.insert(u, Ty::new(self.db, TyKind::Var(var)));
                type_vars.push(var);
            }

            type_vars.sort();
            GeneralizedType::Poly(type_vars.into(), ty)
        }
    }

    pub fn find_all_type_vars(&self, ty: Ty, res: &mut NoHashHashSet<TypeVarId>) {
        ty.traverse(self.db, &mut |t| match t.kind(self.db) {
            | TyKind::Var(v) => {
                res.insert(*v);
            },
            | TyKind::Unknown(u, _) => match self.find_binding(*u, &self.subst.solved) {
                | Ok(t) => self.find_all_type_vars(t, res),
                | Err(_) => {},
            },
            | _ => {},
        });
    }

    pub fn find_all_unknowns(&self, ty: Ty, res: &mut NoHashHashSet<Unknown>) {
        ty.traverse(self.db, &mut |t| match t.kind(self.db) {
            | TyKind::Unknown(u, _) => match self.find_binding(*u, &self.subst.solved) {
                | Ok(t) => self.find_all_unknowns(t, res),
                | Err((level, _)) => {
                    if level >= self.level {
                        res.insert(*u);
                    }
                },
            },
            | _ => {},
        });
    }

    fn new_type_vars(&self, unknowns: NoHashHashSet<Unknown>) -> NoHashHashMap<Unknown, TypeVarId> {
        let mut res = NoHashHashMap::default();

        for (i, u) in unknowns.into_iter().enumerate() {
            let mut name = String::with_capacity(2);
            let mut n = i as u32;

            unsafe {
                name.push('\'');
                while n >= 24 {
                    name.push(char::from_u32_unchecked('a' as u32 + n % 24));
                    n -= 24;
                }
                name.push(char::from_u32_unchecked('a' as u32 + n));
            }

            let name = name.as_name(self.db);
            let var = TypeVarId::new(self.db, self.owner, Either::Right(name));

            res.insert(u, var);
        }

        res
    }

    pub(crate) fn ctor_int_kind(&self, ctor_id: TypeCtorId) -> Option<IntegerKind> {
        self.db.type_cache().ctor_int_kind(self.db, ctor_id)
    }

    pub(crate) fn ctor_float_kind(&self, ctor_id: TypeCtorId) -> Option<FloatKind> {
        self.db.type_cache().ctor_float_kind(self.db, ctor_id)
    }
}

impl<'db> std::ops::Deref for BodyCtx<'db, '_> {
    type Target = Ctx<'db>;

    fn deref(&self) -> &Self::Target {
        self.ctx
    }
}

impl<'db> std::ops::DerefMut for BodyCtx<'db, '_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.ctx
    }
}

impl Cache {
    pub fn clear(&self) {
        self.0.write().clear();
    }

    pub fn ctor_for_int_kind(&self, kind: IntegerKind) -> Option<TypeCtorId> {
        self.0.read().int_kind_ctor.get(&kind).copied()
    }

    pub fn ctor_for_float_kind(&self, kind: FloatKind) -> Option<TypeCtorId> {
        self.0.read().float_kind_ctor.get(&kind).copied()
    }

    pub fn ctor_int_kind(&self, db: &dyn Db, ctor_id: TypeCtorId) -> Option<IntegerKind> {
        if let Some(kind) = self.0.read().ctor_int_kind.get(&ctor_id) {
            return *kind;
        }

        let text = hir_def::data::type_ctor_data(db, ctor_id)
            .attrs(db)
            .by_key("int")
            .string_value()
            .next();

        let kind = text.and_then(|t| match t {
            | "i8" => Some(IntegerKind::I8),
            | "i16" => Some(IntegerKind::I16),
            | "i32" => Some(IntegerKind::I32),
            | "i64" => Some(IntegerKind::I64),
            | "i128" => Some(IntegerKind::I128),
            | "isize" => Some(IntegerKind::Isize),
            | "u8" => Some(IntegerKind::U8),
            | "u16" => Some(IntegerKind::U16),
            | "u32" => Some(IntegerKind::U32),
            | "u64" => Some(IntegerKind::U64),
            | "u128" => Some(IntegerKind::U128),
            | "usize" => Some(IntegerKind::Usize),
            | _ => None,
        });

        if let Some(kind) = kind {
            self.0.write().int_kind_ctor.insert(kind, ctor_id);
        }

        self.0.write().ctor_int_kind.insert(ctor_id, kind);
        kind
    }

    pub fn ctor_float_kind(&self, db: &dyn Db, ctor_id: TypeCtorId) -> Option<FloatKind> {
        if let Some(kind) = self.0.read().ctor_float_kind.get(&ctor_id) {
            return *kind;
        }

        let text = hir_def::data::type_ctor_data(db, ctor_id)
            .attrs(db)
            .by_key("float")
            .string_value()
            .next();

        let kind = text.and_then(|t| match t {
            | "f32" => Some(FloatKind::F32),
            | "f64" => Some(FloatKind::F64),
            | _ => None,
        });

        if let Some(kind) = kind {
            self.0.write().float_kind_ctor.insert(kind, ctor_id);
        }

        self.0.write().ctor_float_kind.insert(ctor_id, kind);
        kind
    }
}

impl CacheInner {
    fn clear(&mut self) {
        self.ctor_int_kind.clear();
        self.ctor_float_kind.clear();
    }
}

impl Expectation {
    pub fn adjust_for_branches(self, db: &dyn Db) -> Self {
        match self {
            | Self::HasType(ty) => {
                if let TyKind::Unknown(_, _) = ty.kind(db) {
                    Self::None
                } else {
                    Self::HasType(ty)
                }
            },
            | _ => Self::None,
        }
    }

    pub fn to_option(self) -> Option<Ty> {
        match self {
            | Self::HasType(ty) => Some(ty),
            | _ => None,
        }
    }
}
