#![feature(crate_visibility_modifier)]
#![feature(min_const_generics)]
#![feature(try_blocks)]

pub mod check;
pub mod ctx;
pub mod display;
pub mod error;
pub mod infer;
pub mod kind;
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
    let mut ctx = ctx::Ctx::new(db, file);

    let ty: error::Result<ty::Ty> = match def {
        ir::Def::Item(item) => match &item.kind {
            ir::ItemKind::Func { ty, body } => {
                try {
                    let mut ty = ctx.hir_ty(ty);

                    ctx.tys.insert(item.id, ty.clone());

                    if let ty::Type::Unknown(_) = *ty {
                        let infer = ctx.infer_body(item.span, &hir.bodies[body])?;

                        ctx.unify_types(ty.clone(), infer)?;
                        ty = ctx.subst_type(ty);
                        ty = ctx.generalize(ty, item.id.owner);
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
                            file,
                            vars.iter()
                                .map(|v| (ty::TypeVar(v.id), Some(ctx.ty_kind(v.span, file)))),
                            ty,
                            None,
                        )
                    } else {
                        ty
                    }
                }
            }
            ir::ItemKind::Data { head, .. } => {
                if let ir::TypeKind::Infer = head.kind.kind {
                    let mut kind = ctx.ty_kind(item.span, file);

                    if !head.vars.is_empty() {
                        let params = (0..head.vars.len()).map(|_| {
                            ctx.fresh_type_with_kind(item.span, file, ctx.ty_kind(item.span, file))
                        });

                        let params = ty::Ty::tuple(item.span, file, params);
                        let func_ty = ctx.func_ty(item.span, file);

                        kind =
                            ty::Ty::app(item.span, file, func_ty, ty::List::from([params, kind]));
                    }

                    Ok(kind)
                } else {
                    let kind = ctx.hir_ty(&head.kind);

                    Ok(kind)
                }
            }
            _ => unimplemented!(),
        },
        _ => unimplemented!(),
    };

    let ty = match ty {
        Ok(ty) => ty,
        Err(e) => {
            e.report(db);
            ty::Ty::error(def.span(), file)
        }
    };

    let mut tys = std::mem::replace(&mut ctx.tys, Default::default());

    tys.values_mut().for_each(|t| {
        let ty = t.clone();
        let ty = ctx.subst_type(ty);
        let ty = skolem::unskolemize(ty);

        *t = ty;
    });

    ctx.tys = tys;

    for mut e in std::mem::replace(&mut ctx.errors, Vec::new()) {
        if let error::TypeError::HoleType(name, ty) = e {
            let loc = ty.loc();
            e = error::TypeError::HoleType(name, ctx.subst_type(ty) ^ loc);
        }

        e.report(db)
    }

    if db.has_errors() {
        db.print_and_exit();
    } else {
        db.print();
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
    let mut ctx = ctx::Ctx::new(db, file);

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
