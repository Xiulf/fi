#![feature(crate_visibility_modifier)]
#![feature(min_const_generics)]
#![feature(box_patterns)]
#![feature(try_blocks)]

pub mod check;
pub mod ctx;
pub mod display;
pub mod entailment;
pub mod error;
pub mod external;
pub mod infer;
pub mod items;
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
    #[salsa::invoke(external::load_external)]
    fn external_types(&self, lib: source::LibId, module: ir::ModuleId) -> Arc<external::ExternalTypeData>;

    fn typeck_module(&self, lib: source::LibId, module: ir::ModuleId) -> Arc<ModuleTypes>;

    fn typecheck(&self, id: ir::DefId) -> Arc<TypecheckResult>;

    fn variants(&self, id: ir::DefId) -> ty::Variants;
}

pub trait InferDb {
    fn to_ty_db(&self) -> &dyn TypeDatabase;
    fn to_hir_db(&self) -> &dyn hir::HirDatabase;
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypecheckResult {
    pub ty: ty::Ty,
    pub tys: HashMap<ir::HirId, ty::Ty>,
    pub bounds: HashMap<ir::HirId, BoundInfo>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct BoundInfo {
    pub source: BoundSource,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BoundSource {
    Instance(ir::DefId),
}

#[derive(Debug, PartialEq, Eq)]
pub struct ModuleTypes {
    pub items: HashMap<ir::DefId, TypecheckResult>,
    pub variants: HashMap<ir::DefId, ty::Variants>,
    pub classes: HashMap<ir::DefId, ClassTypes>,
    pub instances: HashMap<ir::DefId, Vec<ty::Instance>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ClassTypes {
    pub var_kinds: HashMap<ty::TypeVar, ty::Ty>,
}

fn typeck_module(db: &dyn TypeDatabase, lib: source::LibId, module: ir::ModuleId) -> Arc<ModuleTypes> {
    let file = db.module_tree(lib).file(module);
    let hir = db.module_hir(file);
    let mut ctx = ctx::Ctx::new(db, file, module);

    for id in hir.items.keys() {
        ctx.typeck_def(id.owner);
    }

    for id in hir.class_items.keys() {
        ctx.typeck_def(id.0.owner);
    }

    for id in hir.instance_items.keys() {
        ctx.typeck_def(id.0.owner);
    }

    if db.has_errors() {
        db.print_and_exit();
    } else {
        db.print();
    };

    Arc::new(ctx.module_types)
}

fn typecheck(db: &dyn TypeDatabase, id: ir::DefId) -> Arc<TypecheckResult> {
    if id.lib != db.lib() {
        let external = db.external_types(id.lib, id.module);
        let ty = external.types[&id].clone();

        Arc::new(TypecheckResult {
            ty,
            tys: HashMap::new(),
            bounds: HashMap::new(),
        })
    } else {
        let types = db.typeck_module(id.lib, id.module);
        let ty = types.items[&id].clone();

        Arc::new(ty)
    }
}

fn variants(db: &dyn TypeDatabase, id: ir::DefId) -> ty::Variants {
    if id.lib == db.lib() {
        let types = db.typeck_module(id.lib, id.module);

        types.variants[&id].clone()
    } else {
        let external = db.external_types(id.lib, id.module);

        external.variants[&id].clone()
    }
}

// fn variants(db: &dyn TypeDatabase, id: ir::DefId) -> ty::Variants {
//     let file = db.module_tree(id.lib).file(id.module);
//     let hir = db.module_hir(file);
//     let def = hir.def(id);
//     let mut ctx = ctx::Ctx::new(db, file, id.module);
//
//     if let ir::Def::Item(ir::Item {
//         kind: ir::ItemKind::Data { head, body },
//         ..
//     }) = def
//     {
//         let vars = head.vars.iter().map(|v| ty::TypeVar(v.id)).collect();
//
//         ty::Variants {
//             vars,
//             variants: body
//                 .iter()
//                 .filter_map(|ctor_id| {
//                     if let ir::Def::Item(item) = hir.def(ctor_id.owner) {
//                         if let ir::ItemKind::DataCtor { data: _, tys } = &item.kind {
//                             let tys = tys.iter().map(|t| ctx.hir_ty(t)).collect();
//
//                             Some(ty::Variant { id: ctor_id.owner, tys })
//                         } else {
//                             None
//                         }
//                     } else {
//                         None
//                     }
//                 })
//                 .collect(),
//         }
//     } else {
//         unreachable!()
//     }
// }
//
// fn instance(db: &dyn TypeDatabase, id: ir::DefId) -> ty::Instance {
//     assert_eq!(id.lib, db.lib());
//     let file = db.module_tree(id.lib).file(id.module);
//     let hir = db.module_hir(file);
//     let mut ctx = ctx::Ctx::new(db, file, id.module);
//
//     if let ir::ItemKind::Instance { chain, index, head, .. } = &hir.items[&id.into()].kind {
//         ty::Instance {
//             id,
//             class: head.class,
//             chain: chain.clone().into(),
//             chain_index: *index,
//             tys: head.tys.iter().map(|t| ctx.hir_ty(t)).collect(),
//         }
//     } else {
//         unreachable!();
//     }
// }
//
// fn instances(db: &dyn TypeDatabase, id: ir::DefId) -> ty::List<ty::Instance> {
//     let mut instances = db
//         .external_modules(db.lib())
//         .iter()
//         .flat_map(|module| {
//             if let Some(instances) = db.external_types(module.lib, module.id).instances.get(&id) {
//                 instances.clone()
//             } else {
//                 Vec::new()
//             }
//         })
//         .collect::<Vec<_>>();
//
//     for module in &db.module_tree(db.lib()).data {
//         for item in db.module_hir(module.file).items.values() {
//             if let ir::ItemKind::Instance { head, .. } = &item.kind {
//                 if head.class == id {
//                     instances.push(db.instance(id));
//                 }
//             }
//         }
//     }
//
//     instances.into()
// }
