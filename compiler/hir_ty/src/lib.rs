#![feature(trait_upcasting, let_chains)]

use arena::ArenaMap;
use ctx::Expectation;
use hir_def::expr::ExprId;
use hir_def::id::{
    ContainerId, CtorId, FieldId, ImplId, LocalCtorId, TraitId, TypeAliasId, TypeCtorId, TypeVarId, TypedItemId,
    ValueId,
};
use hir_def::name::Name;
use hir_def::pat::PatId;
use syntax::TextRange;
use triomphe::Arc;
use ty::{Constraint, ConstraintOrigin, FuncType, GeneralizedType, Ty, TyKind};
use vfs::InFile;

pub mod ctx;
pub mod diagnostics;
pub mod expr;
pub mod kind;
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
    ty::is_recursive,
    infer,
    type_ctor_ty,
    ctor_ty,
    field_ty,
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
            is_varargs: false,
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

        if let Some((b, c)) = impl_ty(db, id, name) {
            let a = ctx.result.ty.clone();
            ctx.result.ty = ctx.unify_generalized_types(&a, b, c, TyOrigin::ExprId(body.body_expr()));
        }
    }

    let (GeneralizedType::Mono(ty) | GeneralizedType::Poly(_, ty)) = ctx.result.ty;
    let is_main = hir_def::attrs::query(db, value.into()).by_key("main").exists();

    if is_main {
        ctx.solve_constraints(false);
    } else {
        ctx.solve_constraints(true);
        ctx.result.ty = ctx.generalize(ty, &type_vars);
    }

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

    let constraints = constraints
        .into_iter()
        .map(|c| c.replace_vars(db, &replacements))
        .collect::<Vec<_>>();

    match ty {
        | GeneralizedType::Mono(ty) => Some((GeneralizedType::Mono(ty.replace_vars(db, &replacements)), constraints)),
        | GeneralizedType::Poly(vars, ty) => {
            let ty = ty.replace_vars(db, &replacements);
            let vars = vars[replacements.len()..].into();

            Some((GeneralizedType::new(ty, &vars), constraints))
        },
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct TypeCtorResult {
    pub kind: Ty,
    pub ctors: ArenaMap<LocalCtorId, GeneralizedType>,
}

#[salsa::tracked]
pub fn type_ctor_ty(db: &dyn Db, type_ctor: TypeCtorId) -> Arc<TypeCtorResult> {
    let it = type_ctor.it(db);
    let item_tree = hir_def::item_tree::query(db, it.file);
    let data = hir_def::data::type_ctor_data(db, type_ctor);
    let type_vars = data.type_vars(db);
    let type_map = TypedItemId::from(type_ctor).type_map(db).0;
    let mut ctx = ctx::Ctx::new(db, type_ctor.into());
    let mut lcx = lower::LowerCtx::new(&mut ctx, type_map);
    let kind = data
        .kind(db)
        .map(|k| lcx.lower_type_ref(k, true))
        .unwrap_or_else(|| lcx.ctx.fresh_type(lcx.ctx.level, false));

    let mut ctors = ArenaMap::default();
    let ty = Ty::new(db, TyKind::Ctor(type_ctor));
    let ty = if type_vars.is_empty() {
        ty
    } else {
        let args = type_vars.iter().map(|&v| Ty::new(db, TyKind::Var(v))).collect();
        Ty::new(db, TyKind::App(ty, args))
    };

    for &ctor in item_tree[it.value].ctors.iter() {
        let ctor_id = CtorId::new(db, type_ctor, ctor);
        let data = hir_def::data::ctor_data(db, ctor_id);
        let types = data.types(db);
        let ty = if types.is_empty() {
            ty
        } else {
            let params = types.iter().map(|&t| lcx.lower_type_ref(t, true)).collect();

            Ty::new(
                db,
                TyKind::Func(FuncType {
                    params,
                    ret: ty,
                    env: lcx.unit_type(),
                    is_varargs: false,
                }),
            )
        };

        ctors.insert(ctor, GeneralizedType::new(ty, type_vars));
    }

    let kind = ctx.resolve_type_fully(kind);

    Arc::new(TypeCtorResult { kind, ctors })
}

#[salsa::tracked]
pub fn ctor_ty(db: &dyn Db, ctor: CtorId) -> GeneralizedType {
    type_ctor_ty(db, ctor.type_ctor(db)).ctors[ctor.local_id(db)].clone()
}

#[salsa::tracked]
pub fn field_ty(_db: &dyn Db, _field: FieldId) -> GeneralizedType {
    todo!()
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

#[salsa::tracked(return_ref)]
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

#[salsa::tracked(return_ref)]
pub fn impl_types(db: &dyn Db, impl_id: ImplId) -> (Box<[Ty]>, Box<[Constraint]>) {
    let data = hir_def::data::impl_data(db, impl_id);
    let type_map = TypedItemId::ImplId(impl_id).type_map(db).0;
    let mut ctx = ctx::Ctx::new(db, impl_id.into());
    let mut lcx = lower::LowerCtx::new(&mut ctx, type_map);
    let types = data.types(db).iter().map(|t| lcx.lower_type_ref(*t, false)).collect();
    let constraints = data
        .where_clause(db)
        .map(|wc| {
            wc.constraints
                .iter()
                .filter_map(|c| {
                    Some(Constraint {
                        trait_id: c.trait_id?,
                        args: c.args.iter().map(|t| lcx.lower_type_ref(*t, false)).collect(),
                    })
                })
                .collect()
        })
        .unwrap_or_default();

    (types, constraints)
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
        | ContainerId::TraitId(id) => &trait_types(db, id).1,
        | ContainerId::ImplId(id) => &impl_types(db, id).1,
    };

    let data = hir_def::data::value_data(db, id);
    let where_clause = data.where_clause(db);
    let constraints = match where_clause {
        | Some(ref wc) => &*wc.constraints,
        | None => &[],
    };

    let type_map = TypedItemId::ValueId(id).type_map(db).0;
    let mut ctx = ctx::Ctx::new(db, id.into());
    let mut lcx = lower::LowerCtx::new(&mut ctx, type_map);
    let constraints = constraints
        .iter()
        .filter_map(|c| lcx.lower_constraint(c))
        .collect::<Vec<_>>();

    [&**parent, &constraints].concat()
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
            | Self::ExprId(id, _) => {
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
