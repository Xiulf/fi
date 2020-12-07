pub mod constraint;
pub mod ctx;
pub mod error;
pub mod subst;
pub mod ty;
mod unify;

use hir::ir;
use std::collections::HashMap;
use std::sync::Arc;

#[salsa::query_group(TypeDatabaseStorage)]
pub trait TypeDatabase: hir::HirDatabase + InferDb {
    fn typecheck(&self, id: ir::DefId) -> Arc<TypeCheckResult>;

    fn variants(&self, id: ir::DefId) -> ty::List<ty::Variant>;
}

pub trait InferDb {
    fn to_ty_db(&self) -> &dyn TypeDatabase;
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
            ir::ItemKind::Data { head, body: _ } => {
                let ty = ty::Ty::data(item.id.owner);

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
            ir::ItemKind::DataCtor { data, tys } => {
                let data_ty = db.typecheck(data.owner).ty.clone();
                let tys = tys.iter().map(|t| ctx.hir_ty(t)).collect::<ty::List<_>>();

                if tys.is_empty() {
                    data_ty
                } else if let ty::Type::ForAll(vars, data_ty) = &*data_ty {
                    ty::Ty::for_all(vars.clone(), ty::Ty::func(tys, data_ty.clone()))
                } else {
                    ty::Ty::func(tys, data_ty)
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

    // ctx.unify();

    if db.has_errors() {
        db.print_and_exit();
    } else {
        ctx.verify();
        db.print();
    }

    Arc::new(TypeCheckResult {
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

// fn recover_cycle(
//     _db: &dyn TypeDatabase,
//     _cycle: &[String],
//     id: &ir::DefId,
// ) -> Arc<TypeCheckResult> {
//     Arc::new(TypeCheckResult {
//         ty: ty::Ty::type_of(*id),
//         tys: Default::default(),
//     })
// }
