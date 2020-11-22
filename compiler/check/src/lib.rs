pub mod ctx;
pub mod ty;

use hir::ir;

#[salsa::query_group(TypeDatabaseStorage)]
pub trait TypeDatabase: hir::HirDatabase + InferDb {
    fn type_of(&self, module: ir::ModuleId, id: ir::DefId) -> ty::Ty;
}

pub trait InferDb {
    fn new_infer_var(&self) -> ty::InferVar;
}

fn type_of(db: &dyn TypeDatabase, module: ir::ModuleId, id: ir::DefId) -> ty::Ty {
    let file = db.module_tree(id.lib).file(module);
    let hir = db.module_hir(file);
    let def = hir.def(id);
    let mut ctx = ctx::Ctx::new(db, module);

    match def {
        ir::Def::Item(item) => match &item.kind {
            ir::ItemKind::Foreign { ty, kind: _ } => ctx.hir_ty(ty),
            ir::ItemKind::Func { ty, body: _ } => {
                let expected = ctx.hir_ty(ty);

                expected
            }
            ir::ItemKind::Const { ty, body: _ } => {
                let expected = ctx.hir_ty(ty);

                expected
            }
            ir::ItemKind::Static { ty, body: _ } => {
                let expected = ctx.hir_ty(ty);

                expected
            }
            _ => ty::Ty::Error,
        },
        ir::Def::TraitItem(_item) => {
            unimplemented!();
        }
        ir::Def::ImplItem(_item) => {
            unimplemented!();
        }
    }
}
