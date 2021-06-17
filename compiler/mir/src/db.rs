use crate::eval::EvalResult;
use crate::instance_record::InstanceRecord;
use crate::ir::Bodies;
use crate::layout::Layout;
use crate::ty::Type;
use base_db::Upcast;
use hir::db::HirDatabase;
use hir::ty::Ty;
use std::sync::Arc;

#[salsa::query_group(MirDatabaseStorage)]
pub trait MirDatabase: HirDatabase + Upcast<dyn HirDatabase> {
    #[salsa::input]
    fn target_triple(&self) -> Arc<target_lexicon::Triple>;

    #[salsa::invoke(crate::layout::layout_of_query)]
    fn layout_of(&self, ty: Arc<Type>) -> Arc<Layout>;

    #[salsa::invoke(Type::mir_type_query)]
    #[salsa::cycle(mir_type_recover)]
    fn mir_type(&self, ty: Ty) -> Arc<Type>;

    #[salsa::invoke(InstanceRecord::instance_record_query)]
    fn instance_record(&self, class: hir::Class) -> Arc<InstanceRecord>;

    #[salsa::invoke(Bodies::body_mir_query)]
    fn body_mir(&self, def: hir::id::DefWithBodyId) -> Arc<Bodies>;

    #[salsa::invoke(crate::eval::eval_query)]
    fn eval(&self, def: hir::id::DefWithBodyId) -> EvalResult;
}

fn mir_type_recover(db: &dyn MirDatabase, _cycle: &[String], ty: &Ty) -> Arc<Type> {
    Arc::new(Type {
        kind: crate::ty::TypeKind::Recurse(*ty),
        repr: crate::ty::ReprOptions::default(),
    })
}
