#![feature(trait_upcasting, let_chains)]

use ctx::Expectation;
use hir_def::expr::ExprId;
use hir_def::id::{ContainerId, CtorId, ImplId, TraitId, TypeAliasId, TypeVarId, TypedItemId, ValueId};
use hir_def::name::Name;
use hir_def::pat::PatId;
use syntax::TextRange;
use triomphe::Arc;
use ty::{Constraint, ConstraintOrigin, FuncType, GeneralizedType, Ty, TyKind};
use vfs::InFile;

pub mod ctx;
pub mod diagnostics;
pub mod expr;
pub mod lower;
pub mod pat;
pub mod traits;
pub mod ty;
pub mod unify;

pub trait Db: hir_def::Db + salsa::DbWithJar<Jar> {
    fn type_cache(&self) -> &ctx::Cache;
}

#[salsa::jar(db = Db)]
pub struct Jar(
    ty::Ty,
    infer,
    ctor_ty,
    alias_ty,
    trait_types,
    impl_types,
    traits::trait_impls,
    traits::lib_impls,
);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TyOrigin {
    ExprId(ExprId),
    PatId(PatId),
}

#[salsa::tracked]
pub fn infer(db: &dyn Db, value: ValueId) -> Arc<ctx::InferResult> {
    let data = hir_def::data::value_data(db, value);
    let body = hir_def::body::query(db, value).0;
    let type_map = TypedItemId::from(value).type_map(db).0;
    let type_vars = all_type_vars(db, value);
    let mut ctx = ctx::Ctx::new(db, value.into());
    let mut lcx = lower::LowerCtx::new(&mut ctx, type_map);
    let ret = data
        .ty(db)
        .map(|t| lcx.lower_type_ref(t, body.params().is_empty()))
        .unwrap_or_else(|| ctx.fresh_type(ctx.level, false));

    let mut bcx = ctx.with_body(body.clone());
    let params = body
        .params()
        .iter()
        .map(|&p| bcx.infer_pat(p, Expectation::None))
        .collect::<Box<[_]>>();

    let ty = if params.is_empty() {
        ret
    } else {
        let func = FuncType {
            ret,
            params,
            env: bcx.unit_type(),
            variadic: false,
        };

        Ty::new(db, TyKind::Func(func))
    };

    bcx.ret_ty = ret;
    bcx.result.ty = GeneralizedType::new(ty, &type_vars);
    bcx.result.constraints = all_constraints(db, value);

    let _ = bcx.infer_expr(body.body_expr(), ctx::Expectation::HasType(ret));

    if let ContainerId::ImplId(id) = value.container(db) {
        let it = value.it(db);
        let item_tree = hir_def::item_tree::query(db, it.file);
        let name = item_tree[it.value].name;

        if let Some((b, mut constraints)) = impl_ty(db, id, name) {
            let a = ctx.result.ty.clone();
            ctx.result.ty = ctx.unify_generalized_types(&a, b, TyOrigin::ExprId(body.body_expr()));
            ctx.result.constraints.append(&mut constraints);
        }
    }

    let (GeneralizedType::Mono(ty) | GeneralizedType::Poly(_, ty)) = ctx.result.ty;

    ctx.solve_constraints();
    ctx.result.ty = ctx.generalize(ty, &type_vars);
    ctx.finish()
}

fn impl_ty(db: &dyn Db, id: ImplId, name: Name) -> Option<(GeneralizedType, Vec<Constraint>)> {
    let data = hir_def::data::impl_data(db, id);
    let trait_data = hir_def::data::trait_data(db, data.trait_id(db)?);
    let trait_item = *trait_data.items(db).get(&name)?;
    let infer = infer(db, trait_item);
    let ty = infer.ty.clone();
    let constraints = infer.constraints[1..].to_vec();
    let (types, _constraints) = impl_types(db, id);
    let replacements = trait_data
        .type_vars(db)
        .iter()
        .zip(types.iter())
        .map(|(&var, &ty)| (var, ty))
        .collect();

    match ty {
        | GeneralizedType::Mono(ty) => Some((GeneralizedType::Mono(ty.replace_vars(db, &replacements)), constraints)),
        | GeneralizedType::Poly(vars, ty) => {
            let ty = ty.replace_vars(db, &replacements);
            let vars = vars[replacements.len()..].into();

            Some((GeneralizedType::new(ty, &vars), constraints))
        },
    }
}

#[salsa::tracked]
pub fn ctor_ty(db: &dyn Db, ctor: CtorId) -> GeneralizedType {
    let type_ctor = ctor.type_ctor(db);
    let type_ctor_data = hir_def::data::type_ctor_data(db, type_ctor);
    let data = hir_def::data::ctor_data(db, ctor);
    let types = data.types(db);
    let type_vars = type_ctor_data.type_vars(db);
    let ty = Ty::new(db, TyKind::Ctor(type_ctor));
    let type_map = TypedItemId::from(ctor).type_map(db).0;
    let mut ctx = ctx::Ctx::new(db, ctor.into());
    let mut ctx = lower::LowerCtx::new(&mut ctx, type_map);
    let ty = if type_vars.is_empty() {
        ty
    } else {
        let args = type_vars.iter().map(|&v| Ty::new(db, TyKind::Var(v))).collect();
        Ty::new(db, TyKind::App(ty, args))
    };

    if types.is_empty() {
        return GeneralizedType::new(ty, type_vars);
    }

    let params = types.iter().map(|&t| ctx.lower_type_ref(t, false)).collect();

    GeneralizedType::new(
        Ty::new(
            db,
            TyKind::Func(FuncType {
                params,
                ret: ty,
                env: ctx.unit_type(),
                variadic: false,
            }),
        ),
        type_vars,
    )
}

#[salsa::tracked]
pub fn alias_ty(db: &dyn Db, alias: TypeAliasId) -> GeneralizedType {
    let data = hir_def::data::type_alias_data(db, alias);
    let type_map = TypedItemId::from(alias).type_map(db).0;
    let mut ctx = ctx::Ctx::new(db, alias.into());
    let mut ctx = lower::LowerCtx::new(&mut ctx, type_map);
    let ty = match data.ty(db) {
        | Some(ty) => ctx.lower_type_ref(ty, true),
        | None => ctx.error(),
    };

    GeneralizedType::new(ty, data.type_vars(db))
}

#[salsa::tracked]
pub fn trait_types(db: &dyn Db, trait_id: TraitId) -> (Box<[Ty]>, Box<[Constraint]>) {
    let data = hir_def::data::trait_data(db, trait_id);
    // let type_map = TypedItemId::TraitId(trait_id).type_map(db).0;
    // let mut ctx = ctx::Ctx::new(db, trait_id.into());
    // let mut lcx = lower::LowerCtx::new(&mut ctx, type_map);
    // let types = data.types(db).iter().map(|t| lcx.lower_type_ref(*t, false)).collect();
    let types = Box::new([]);
    let args = data
        .type_vars(db)
        .iter()
        .map(|&v| Ty::new(db, TyKind::Var(v)))
        .collect();

    let constraints = [Constraint { trait_id, args }].into();

    (types, constraints)
}

#[salsa::tracked]
pub fn impl_types(db: &dyn Db, impl_id: ImplId) -> (Box<[Ty]>, Box<[Constraint]>) {
    let data = hir_def::data::impl_data(db, impl_id);
    let type_map = TypedItemId::ImplId(impl_id).type_map(db).0;
    let mut ctx = ctx::Ctx::new(db, impl_id.into());
    let mut lcx = lower::LowerCtx::new(&mut ctx, type_map);
    let types = data.types(db).iter().map(|t| lcx.lower_type_ref(*t, false)).collect();

    (types, Box::new([]))
}

fn all_type_vars(db: &dyn Db, id: ValueId) -> Box<[TypeVarId]> {
    let data = hir_def::data::value_data(db, id);
    let parent = match id.container(db) {
        | ContainerId::ModuleId(_) => return data.type_vars(db).clone(),
        | ContainerId::TraitId(id) => TypedItemId::TraitId(id).type_map(db).2,
        | ContainerId::ImplId(id) => TypedItemId::ImplId(id).type_map(db).2,
    };

    [&*parent, data.type_vars(db)].concat().into_boxed_slice()
}

fn all_constraints(db: &dyn Db, id: ValueId) -> Vec<Constraint> {
    let parent = match id.container(db) {
        | ContainerId::ModuleId(_) => return Vec::new(),
        | ContainerId::TraitId(id) => trait_types(db, id).1,
        | ContainerId::ImplId(id) => impl_types(db, id).1,
    };

    [&*parent].concat()
}

impl TyOrigin {
    pub fn to_text_range(self, db: &dyn Db, owner: TypedItemId) -> InFile<TextRange> {
        match self {
            | Self::ExprId(id) => {
                let value = owner.as_value_id().unwrap();
                let (_, src_map) = hir_def::body::query(db, value);
                let src = src_map.expr_src(id);

                InFile::new(src_map.file(), src.text_range())
            },
            | Self::PatId(id) => {
                let value = owner.as_value_id().unwrap();
                let (_, src_map) = hir_def::body::query(db, value);
                let src = src_map.pat_src(id);

                InFile::new(src_map.file(), src.text_range())
            },
        }
    }
}

impl ConstraintOrigin {
    pub fn to_text_range(self, db: &dyn Db, owner: TypedItemId) -> InFile<TextRange> {
        match self {
            | Self::ExprId(id) => {
                let value = owner.as_value_id().unwrap();
                let (_, src_map) = hir_def::body::query(db, value);
                let src = src_map.expr_src(id);

                InFile::new(src_map.file(), src.text_range())
            },
            | Self::PatId(id) => {
                let value = owner.as_value_id().unwrap();
                let (_, src_map) = hir_def::body::query(db, value);
                let src = src_map.pat_src(id);

                InFile::new(src_map.file(), src.text_range())
            },
            | Self::Impl(_, _) => todo!(),
        }
    }
}

ra_ap_stdx::impl_from!(ExprId, PatId for TyOrigin);
