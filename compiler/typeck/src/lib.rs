#![feature(crate_visibility_modifier)]
#![feature(min_const_generics)]
#![feature(try_blocks)]

pub mod check;
pub mod ctx;
pub mod error;
pub mod infer;
pub mod skolem;
pub mod subsume;
pub mod ty;
pub mod unify;

use hir::ir;
use std::collections::HashMap;
use std::sync::Arc;

#[salsa::query_group(TypeDatabaseStorage)]
pub trait TypeDatabase: hir::HirDatabase + InferDb {
    fn typecheck(&self, id: ir::DefId) -> Arc<TypecheckResult>;

    fn variants(&self, id: ir::DefId) -> ty::List<ty::Variant>;
}

pub trait InferDb {
    fn to_ty_db(&self) -> &dyn TypeDatabase;
}

#[derive(Debug, PartialEq, Eq)]
pub struct TypecheckResult {
    pub ty: ty::Ty,
    pub tys: HashMap<ir::HirId, ty::Ty>,
}

fn typecheck(db: &dyn TypeDatabase, id: ir::DefId) -> Arc<TypecheckResult> {
    let file = db.module_tree(id.lib).file(id.module);
    let hir = db.module_hir(file);
    let def = hir.def(id);
    let mut ctx = ctx::Ctx::new(db);

    let ty: error::Result<ty::Ty> = match def {
        ir::Def::Item(item) => match &item.kind {
            ir::ItemKind::Func { ty, body } => {
                try {
                    let ty = ctx.hir_ty(ty);

                    ctx.tys.insert(item.id, ty.clone());

                    if let ty::Type::Unknown(_) = *ty {
                        let infer = ctx.infer_body(item.span, &hir.bodies[body])?;

                        ctx.unify_types(ty.clone(), infer)?;
                    } else {
                        ctx.check_body(item.span, &hir.bodies[body], ty.clone())?;
                    }

                    ty
                }
            }
            ir::ItemKind::Alias {
                vars,
                value,
                kind: _,
            } => {
                try {
                    let ty = ctx.hir_ty(value);

                    if !vars.is_empty() {
                        ty::Ty::forall(
                            item.span,
                            vars.iter()
                                .map(|v| (ty::TypeVar(v.id), Some(ctx.ty_kind(v.span)))),
                            ty,
                            None,
                        )
                    } else {
                        ty
                    }
                }
            }
            _ => unimplemented!(),
        },
        _ => unimplemented!(),
    };

    let ty = match ty {
        Ok(ty) => ty,
        Err(_e) => ty::Ty::error(def.span()),
    };

    Arc::new(TypecheckResult {
        ty,
        tys: ctx.finish(),
    })
}

fn variants(db: &dyn TypeDatabase, id: ir::DefId) -> ty::List<ty::Variant> {
    let file = db.module_tree(id.lib).file(id.module);
    let hir = db.module_hir(file);
    let def = hir.def(id);
    let mut ctx = ctx::Ctx::new(db);

    if let ir::Def::Item(ir::Item {
        kind: ir::ItemKind::Data { body, .. },
        ..
    }) = def
    {
        body.iter()
            .filter_map(|ctor_id| {
                if let ir::Def::Item(item) = hir.def(ctor_id.owner) {
                    if let ir::ItemKind::DataCtor { data: _, tys } = &item.kind {
                        let tys = tys.iter().map(|t| ctx.hir_ty(t)).collect();

                        Some(ty::Variant {
                            id: ctor_id.owner,
                            tys,
                        })
                    } else {
                        None
                    }
                } else {
                    None
                }
            })
            .collect()
    } else {
        unreachable!()
    }
}
