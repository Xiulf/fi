#![feature(trait_upcasting)]

use ctx::Expectation;
use hir_def::expr::ExprId;
use hir_def::id::{CtorId, TypedItemId, ValueId};
use hir_def::pat::PatId;
use syntax::TextRange;
use triomphe::Arc;
use ty::{FuncType, GeneralizedType, Ty, TyKind};
use vfs::InFile;

pub mod ctx;
pub mod diagnostics;
pub mod expr;
pub mod lower;
pub mod pat;
pub mod ty;
pub mod unify;

pub trait Db: hir_def::Db + salsa::DbWithJar<Jar> {
    fn type_cache(&self) -> &ctx::Cache;
}

#[salsa::jar(db = Db)]
pub struct Jar(ty::Ty, infer, ctor_ty);

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
    let mut ctx = ctx::Ctx::new(db, value.into());
    let mut lcx = lower::LowerCtx::new(&mut ctx, type_map);
    let ret = data
        .ty(db)
        .map(|t| lcx.lower_type_ref(t))
        .unwrap_or_else(|| ctx.fresh_type(ctx.level));

    let mut bcx = ctx.with_body(body.clone());
    let params = body
        .params()
        .iter()
        .map(|&p| bcx.infer_pat(p, Expectation::None))
        .collect::<Box<[_]>>();

    bcx.ret_ty = ret;
    bcx.result.ty = if params.is_empty() {
        GeneralizedType::new(ret, data.type_vars(db))
    } else {
        GeneralizedType::new(
            Ty::new(
                db,
                TyKind::Func(FuncType {
                    ret,
                    params,
                    env: bcx.unit_type(),
                    variadic: false,
                }),
            ),
            data.type_vars(db),
        )
    };

    let _ = bcx.infer_expr(body.body_expr(), ctx::Expectation::HasType(ret));
    let (GeneralizedType::Mono(ty) | GeneralizedType::Poly(_, ty)) = ctx.result.ty;

    ctx.result.ty = ctx.generalize(ty, data.type_vars(db));
    ctx.finish()
}

#[salsa::tracked]
pub fn ctor_ty(db: &dyn Db, ctor: CtorId) -> GeneralizedType {
    let type_ctor = ctor.type_ctor(db);
    let data = hir_def::data::ctor_data(db, ctor);
    let types = data.types(db);
    let ty = Ty::new(db, TyKind::Ctor(type_ctor));
    let type_map = TypedItemId::from(ctor).type_map(db).0;
    let mut ctx = ctx::Ctx::new(db, ctor.into());
    let mut ctx = lower::LowerCtx::new(&mut ctx, type_map);

    if types.is_empty() {
        return GeneralizedType::Mono(ty);
    }

    let params = types.iter().map(|&t| ctx.lower_type_ref(t)).collect();

    GeneralizedType::Mono(Ty::new(
        db,
        TyKind::Func(FuncType {
            params,
            ret: ty,
            env: ctx.unit_type(),
            variadic: false,
        }),
    ))
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

ra_ap_stdx::impl_from!(ExprId, PatId for TyOrigin);
