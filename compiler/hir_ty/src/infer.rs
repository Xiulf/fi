mod kind;
mod unify;

use crate::db::HirDatabase;
use crate::ty::{Ty, TyKind, TypeVar};
use hir_def::arena::ArenaMap;
use hir_def::expr::ExprId;
use hir_def::id::{DefWithBodyId, FuncId, HasModule, TypedDefId};
use hir_def::pat::PatId;
use rustc_hash::FxHashMap;
use std::sync::Arc;

pub(crate) fn infer_query(db: &dyn HirDatabase, def: DefWithBodyId) -> Arc<InferenceResult> {
    unimplemented!();
}

#[derive(Default, Debug, PartialEq, Eq)]
pub struct InferenceResult {
    pub type_of_expr: ArenaMap<ExprId, Ty>,
    pub type_of_pat: ArenaMap<PatId, Ty>,
}

pub(crate) struct InferenceContext<'a> {
    db: &'a dyn HirDatabase,
    owner: TypedDefId,
    result: InferenceResult,
    subst: unify::Substitution,
    var_kinds: FxHashMap<TypeVar, Ty>,
}

impl<'a> InferenceContext<'a> {
    pub(crate) fn new(db: &'a dyn HirDatabase, owner: TypedDefId) -> Self {
        InferenceContext {
            db,
            owner,
            result: InferenceResult::default(),
            subst: unify::Substitution::default(),
            var_kinds: FxHashMap::default(),
        }
    }

    pub(crate) fn lang_type(&self, name: &'static str) -> Ty {
        let module = self.owner.module(self.db.upcast());
        let ty = self.db.lang_item(module.lib, name.into()).unwrap();
        let ty = ty.as_type_ctor().unwrap();

        self.db.type_for_ctor(ty)
    }

    pub(crate) fn fn_type(&self, arg: Ty, ret: Ty) -> Ty {
        let fn_type = self.lang_type("fn-type");
        let base = TyKind::App(fn_type, arg).intern(self.db);

        TyKind::App(base, ret).intern(self.db)
    }

    pub(crate) fn set_var_kind(&mut self, var: TypeVar, kind: Ty) {
        self.var_kinds.insert(var, kind);
    }
}
