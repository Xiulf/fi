pub mod constraint;
pub mod ctx;
pub mod subst;
pub mod ty;
mod unify;

use hir::ir;
use std::collections::HashMap;
use std::sync::Arc;

#[salsa::query_group(TypeDatabaseStorage)]
pub trait TypeDatabase: hir::HirDatabase + InferDb {
    fn typecheck(&self, id: ir::DefId) -> Arc<TypeCheckResult>;
}

pub trait InferDb {
    fn new_infer_var(&self) -> ty::InferVar;
}

#[derive(Debug, PartialEq, Eq)]
pub struct TypeCheckResult {
    pub ty: ty::Ty,
    pub tys: HashMap<ir::HirId, ty::Ty>,
}

fn typecheck(db: &dyn TypeDatabase, id: ir::DefId) -> Arc<TypeCheckResult> {
    let file = db.module_tree(id.lib).file(id.module);
    let hir = db.module_hir(file);
    let def = hir.def(id);
    let mut ctx = ctx::Ctx::new(db, file);

    let ty = match def {
        ir::Def::Item(item) => match &item.kind {
            ir::ItemKind::Foreign { ty, kind: _ } => ctx.hir_ty(ty),
            ir::ItemKind::Func { ty, body } => {
                let ty_ = ctx.hir_ty(ty);

                ctx.infer_body(&hir.bodies[body], ty_.clone(), ty.span);
                ty_
            }
            ir::ItemKind::Const { ty, body } => {
                let ty_ = ctx.hir_ty(ty);

                ctx.infer_body(&hir.bodies[body], ty_.clone(), ty.span);
                ty_
            }
            ir::ItemKind::Static { ty, body } => {
                let ty_ = ctx.hir_ty(ty);

                ctx.infer_body(&hir.bodies[body], ty_.clone(), ty.span);
                ty_
            }
            ir::ItemKind::Alias {
                vars,
                value,
                kind: _,
            } => {
                let ty = ctx.hir_ty(value);

                if vars.is_empty() {
                    ty
                } else {
                    ty::Ty::for_all(
                        vars.iter()
                            .map(|v| {
                                let kind = ctx.hir_ty(&v.kind);
                                let var = ty::TypeVar(v.id);

                                ctx.insert_var_kind(var, kind);
                                var
                            })
                            .collect(),
                        ty,
                    )
                }
            }
            ir::ItemKind::Data { head, body } => {
                let ty = ty::Ty::data(
                    item.id.owner,
                    body.iter()
                        .map(|ctor| {
                            let tys = ctor.tys.iter().map(|t| ctx.hir_ty(t)).collect();

                            ty::Variant {
                                id: ctor.id.owner,
                                tys,
                            }
                        })
                        .collect(),
                );

                if head.vars.is_empty() {
                    ty
                } else {
                    ty::Ty::for_all(
                        head.vars
                            .iter()
                            .map(|v| {
                                let kind = ctx.hir_ty(&v.kind);
                                let var = ty::TypeVar(v.id);

                                ctx.insert_var_kind(var, kind);
                                var
                            })
                            .collect(),
                        ty,
                    )
                }
            }
            _ => ty::Ty::error(),
        },
        ir::Def::TraitItem(_item) => {
            unimplemented!();
        }
        ir::Def::ImplItem(_item) => {
            unimplemented!();
        }
    };

    ctx.unify();

    if db.has_errors() {
        db.print_and_exit();
    } else {
        db.print();
    }

    Arc::new(TypeCheckResult {
        ty,
        tys: ctx.finish(),
    })
}
