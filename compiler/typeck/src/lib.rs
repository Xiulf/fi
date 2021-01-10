#![feature(crate_visibility_modifier)]
#![feature(min_const_generics)]
#![feature(box_patterns)]
#![feature(try_blocks)]

pub mod check;
pub mod ctx;
pub mod display;
pub mod entailment;
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
    fn typeck_module(&self, id: ir::ModuleId) -> Arc<TypecheckResult>;

    fn typecheck(&self, id: ir::DefId) -> Arc<TypecheckResult>;

    fn variants(&self, id: ir::DefId, args: ty::List<ty::Ty>) -> ty::List<ty::Variant>;

    fn impls(&self, id: ir::DefId) -> ty::List<ty::Impl>;
}

pub trait InferDb {
    fn to_ty_db(&self) -> &dyn TypeDatabase;
    fn to_hir_db(&self) -> &dyn hir::HirDatabase;
}

#[derive(Debug, PartialEq, Eq)]
pub struct TypecheckResult {
    pub ty: ty::Ty,
    pub tys: HashMap<ir::HirId, ty::Ty>,
    pub bounds: HashMap<ir::HirId, BoundInfo>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct BoundInfo {
    pub source: BoundSource,
}

#[derive(Debug, PartialEq, Eq)]
pub enum BoundSource {
    Impl(ir::DefId),
}

fn typeck_module(db: &dyn TypeDatabase, id: ir::ModuleId) -> Arc<TypecheckResult> {
    unimplemented!();
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
                    let ty = ctx.hir_ty(ty);

                    ctx.tys.insert(item.id, ty.clone());

                    let (mut ty, should_generalize) = if let ty::Type::Unknown(_) = *ty {
                        let infer = ctx.infer_body(item.span, &hir.bodies[body])?;
                        let _ = ctx.unify_types(ty.clone(), infer.clone())?;

                        (infer, true)
                    } else {
                        ctx.check_body(item.span, &hir.bodies[body], ty.clone())?;
                        (ty, false)
                    };

                    let unsolved = ctx.solve_ctnts(should_generalize)?;

                    ty = ctx.subst_type(ty);
                    ty = ctx.constrain(unsolved, ty);
                    ty = ctx.generalize(ty, item.id.owner);

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
                        // let params = (0..head.vars.len()).map(|_| {
                        //     ctx.fresh_type_with_kind(item.span, file, ctx.ty_kind(item.span, file))
                        // });

                        // for now data types can only be paramaterized by types
                        let params = head.vars.iter().map(|v| ctx.ty_kind(v.span, file));

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
            ir::ItemKind::DataCtor { data, tys } => {
                try {
                    let data = hir.items[data].data();
                    let mut ty = ty::Ty::ctor(item.span, file, data.id.owner);

                    if !data.vars.is_empty() {
                        ty = ty::Ty::app(
                            item.span,
                            file,
                            ty,
                            data.vars
                                .iter()
                                .map(|v| ty::Ty::var(v.span, file, ty::TypeVar(v.id))),
                        );
                    }

                    if !tys.is_empty() {
                        let args =
                            ty::Ty::tuple(item.span, file, tys.iter().map(|t| ctx.hir_ty(t)));

                        let func_ty = ctx.func_ty(item.span, file);

                        ty = ty::Ty::app(item.span, file, func_ty, ty::List::from([args, ty]));
                    }

                    if !data.vars.is_empty() {
                        ty = ty::Ty::forall(
                            item.span,
                            file,
                            data.vars
                                .iter()
                                .map(|v| (ty::TypeVar(v.id), Some(ctx.ty_kind(v.span, file)))),
                            ty,
                            None,
                        );
                    }

                    let ty_kind = ctx.ty_kind(item.span, file);
                    let ty = ctx.check_kind(ty, ty_kind)?;

                    ty
                }
            }
            ir::ItemKind::Foreign { ty, .. } => {
                try {
                    let ty = ctx.hir_ty(ty);
                    let ty_kind = ctx.ty_kind(ty.span(), file);
                    let elab_ty = ctx.check_kind(ty, ty_kind)?;

                    ctx.subst_type(elab_ty)
                }
            }
            _ => unimplemented!(),
        },
        ir::Def::TraitItem(item) => match &item.kind {
            ir::TraitItemKind::Func { ty } => {
                try {
                    let ty = ctx.hir_ty(ty);
                    let trait_ = hir.items[&item.owner].trait_();
                    let vars = trait_
                        .vars
                        .iter()
                        .map(|v| (ty::TypeVar(v.id), Some(ctx.hir_ty(&v.kind))))
                        .collect::<ty::List<_>>();

                    let ty = ty::Ty::ctnt(
                        ty.span(),
                        file,
                        ty::Ctnt {
                            span: ty.span(),
                            file: ty.file(),
                            trait_: item.owner.owner,
                            tys: (&vars)
                                .into_iter()
                                .map(|(v, _)| ty::Ty::var(ty.span(), ty.file(), v))
                                .collect(),
                        },
                        ty,
                    );

                    let ty = ty::Ty::forall(ty.span(), ty.file(), vars, ty, None);
                    let ty_kind = ctx.ty_kind(ty.span(), file);
                    let elab_ty = ctx.check_kind(ty, ty_kind)?;

                    ctx.subst_type(elab_ty)
                }
            }
        },
        ir::Def::ImplItem(item) => match &item.kind {
            ir::ImplItemKind::Func { ty, body } => {
                try {
                    let ty = ctx.hir_ty(ty);
                    let imp = hir.items[&item.owner].impl_();
                    let trait_file = db.module_tree(imp.trait_.lib).file(imp.trait_.module);
                    let trait_hir = db.module_hir(trait_file);
                    let trait_ = trait_hir.items[&imp.trait_.into()].trait_body();
                    let trait_item = trait_
                        .items
                        .iter()
                        .find(|it| it.name.symbol == item.name.symbol)
                        .unwrap();

                    ctx.tys.insert(item.id, ty.clone());

                    let (ty, should_generalize) = if let ty::Type::Unknown(_) = *ty {
                        let infer = ctx.infer_body(item.span, &hir.bodies[body])?;
                        let _ = ctx.unify_types(ty.clone(), infer.clone())?;

                        (infer, true)
                    } else {
                        ctx.check_body(item.span, &hir.bodies[body], ty.clone())?;
                        (ty, false)
                    };

                    let _unsolved = ctx.solve_ctnts(should_generalize)?;
                    let expected = db.typecheck(trait_item.id.0.owner).ty.clone();
                    let expected = ctx.instantiate(item.id, expected);

                    ctx.unify_types(expected, ty.clone())?;
                    ctx.subst_type(ty)
                }
            }
        },
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
        tys: ctx.tys,
        bounds: ctx.bounds,
    })
}

fn variants(db: &dyn TypeDatabase, id: ir::DefId, args: ty::List<ty::Ty>) -> ty::List<ty::Variant> {
    let file = db.module_tree(id.lib).file(id.module);
    let hir = db.module_hir(file);
    let def = hir.def(id);
    let mut ctx = ctx::Ctx::new(db, file);

    if let ir::Def::Item(ir::Item {
        kind: ir::ItemKind::Data { head, body },
        ..
    }) = def
    {
        let vars = head
            .vars
            .iter()
            .zip(args)
            .map(|(v, ty)| (ty::TypeVar(v.id), ty))
            .collect::<HashMap<_, _>>();

        body.iter()
            .filter_map(|ctor_id| {
                if let ir::Def::Item(item) = hir.def(ctor_id.owner) {
                    if let ir::ItemKind::DataCtor { data: _, tys } = &item.kind {
                        let tys = tys
                            .iter()
                            .map(|t| ctx.hir_ty(t).replace_vars(vars.clone()))
                            .collect();

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

fn impls(db: &dyn TypeDatabase, id: ir::DefId) -> ty::List<ty::Impl> {
    use salsa::debug::{DebugQueryTable, TableEntry};
    use salsa::plumbing::get_query_table;
    let file = db.module_tree(id.lib).file(id.module);
    let hir = db.module_hir(file);
    let trait_ = hir.items[&id.into()].trait_();
    let mut ctx = ctx::Ctx::new(db, file);
    let impls: Vec<TableEntry<source::FileId, Arc<ir::Module>>> =
        get_query_table::<hir::ModuleHirQuery>(db.to_hir_db()).entries();

    let impls = impls
        .into_iter()
        .map(|entry| entry.key)
        .flat_map(|file| {
            let module = db.module_hir(file);

            module
                .items
                .values()
                .filter_map(|item| match &item.kind {
                    ir::ItemKind::Impl {
                        chain,
                        index,
                        head,
                        body: _,
                    } if head.trait_ == id => Some((item.id, chain.clone(), *index, head.clone())),
                    _ => None,
                })
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();

    impls
        .into_iter()
        .map(|(id, chain, index, head)| {
            let tys = head.tys.iter().map(|t| ctx.hir_ty(t)).collect();

            // @TODO: check constraints

            ty::Impl {
                trait_: trait_.id.owner,
                id: id.owner,
                chain: chain.into(),
                chain_index: index,
                tys,
            }
        })
        .collect()
}
