mod kind;
mod skolem;
mod unify;

use crate::db::HirDatabase;
use crate::ty::{DebruijnIndex, Ty, TyKind, TypeVar, UniverseIndex};
use diagnostics::InferenceDiagnostic;
use hir_def::arena::ArenaMap;
use hir_def::expr::ExprId;
use hir_def::id::{DefWithBodyId, FuncId, HasModule, TypedDefId};
use hir_def::pat::PatId;
use hir_def::resolver::Resolver;
use rustc_hash::FxHashMap;
use std::sync::Arc;

pub(crate) fn infer_query(db: &dyn HirDatabase, def: DefWithBodyId) -> Arc<InferenceResult> {
    unimplemented!();
}

#[derive(Default, Debug, PartialEq, Eq)]
pub struct InferenceResult {
    pub(crate) type_of_expr: ArenaMap<ExprId, Ty>,
    pub(crate) type_of_pat: ArenaMap<PatId, Ty>,
    pub(crate) diagnostics: Vec<InferenceDiagnostic>,
}

pub(crate) struct InferenceContext<'a> {
    pub(crate) db: &'a dyn HirDatabase,
    pub(crate) resolver: Resolver,
    pub(crate) owner: TypedDefId,
    pub(crate) result: InferenceResult,
    subst: unify::Substitution,
    pub(crate) var_kinds: Vec<Ty>,
    universes: UniverseIndex,
}

impl<'a> InferenceContext<'a> {
    pub(crate) fn new(db: &'a dyn HirDatabase, resolver: Resolver, owner: TypedDefId) -> Self {
        InferenceContext {
            db,
            owner,
            resolver,
            result: InferenceResult::default(),
            subst: unify::Substitution::default(),
            var_kinds: Vec::default(),
            universes: UniverseIndex::ROOT,
        }
    }

    pub(crate) fn finish(self) -> InferenceResult {
        self.result
    }

    pub(crate) fn lang_type(&self, name: &'static str) -> Ty {
        let module = self.owner.module(self.db.upcast());
        let ty = self.db.lang_item(module.lib, name.into()).unwrap();
        let ty = ty.as_type_ctor().unwrap();

        self.db.type_for_ctor(ty).ty
    }

    pub(crate) fn fn_type(&self, arg: Ty, ret: Ty) -> Ty {
        let fn_type = self.lang_type("fn-type");
        let base = TyKind::App(fn_type, arg).intern(self.db);

        TyKind::App(base, ret).intern(self.db)
    }

    pub(crate) fn push_var_kind(&mut self, kind: Ty) {
        self.var_kinds.push(kind);
    }

    pub(crate) fn pop_var_kind(&mut self) {
        self.var_kinds.pop().unwrap();
    }

    pub(crate) fn report(&mut self, diag: InferenceDiagnostic) {
        self.result.diagnostics.push(diag);
    }
}

pub(crate) mod diagnostics {
    use crate::db::HirDatabase;
    use crate::diagnostics::*;
    use crate::ty::Ty;
    use hir_def::diagnostic::DiagnosticSink;
    use hir_def::id::{HasSource, TypeVarOwner};
    use hir_def::type_ref::LocalTypeRefId;

    #[derive(Debug, PartialEq, Eq)]
    pub enum InferenceDiagnostic {
        MismatchedKind {
            id: LocalTypeRefId,
            expected: Ty,
            found: Ty,
        },
    }

    impl InferenceDiagnostic {
        pub fn add_to(&self, db: &dyn HirDatabase, owner: TypeVarOwner, sink: &mut DiagnosticSink) {
            let file = owner.source(db.upcast()).file_id;

            match self {
                | InferenceDiagnostic::MismatchedKind { id, expected, found } => {
                    let src = owner.with_type_source_map(db.upcast(), |source_map| source_map.type_ref_syntax(*id));
                    let src = src.unwrap().syntax_node_ptr();

                    sink.push(MismatchedKind {
                        file,
                        src,
                        found: *found,
                        expected: *expected,
                    });
                },
            }
        }
    }
}
