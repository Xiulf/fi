pub mod ctx;
pub mod ty;

use hir::ir;

#[salsa::query_group(TypeDatabaseStorage)]
pub trait TypeDatabase: hir::HirDatabase + InferDb {
    fn type_of(&self, id: ir::DefId) -> ty::Ty;
}

pub trait InferDb {
    fn new_infer_var(&self) -> ty::InferVar;
}

fn type_of(db: &dyn TypeDatabase, id: ir::DefId) -> ty::Ty {
    let file = db.module_tree(id.lib).file(id.module);
    let hir = db.module_hir(file);
    let def = hir.def(id);
    let mut ctx = ctx::Ctx::new(db);

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
            ir::ItemKind::Alias { vars, value, kind } => {
                let ty = ctx.hir_ty(value);
                let ty = if vars.is_empty() {
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
                };

                let ty_kind = ctx.infer_kind(&ty);
                let kind = ctx.hir_ty(kind);

                // TODO: unify(kind, ty_kind)

                ty
            }
            ir::ItemKind::Data { head, body } => ty::Ty::data(item.id.owner, ty::List::new()),
            _ => ty::Ty::error(),
        },
        ir::Def::TraitItem(_item) => {
            unimplemented!();
        }
        ir::Def::ImplItem(_item) => {
            unimplemented!();
        }
    }
}
