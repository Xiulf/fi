mod expr;
mod kind;
mod pat;
mod skolem;
mod unify;

use crate::db::HirDatabase;
use crate::ty::{Constraint, DebruijnIndex, Ty, TyKind, TypeVar, UniverseIndex};
use diagnostics::InferenceDiagnostic;
use hir_def::arena::ArenaMap;
use hir_def::body::Body;
use hir_def::diagnostic::DiagnosticSink;
use hir_def::expr::ExprId;
use hir_def::id::{ClassId, DefWithBodyId, FuncId, HasModule, TypeVarOwner};
use hir_def::pat::PatId;
use hir_def::resolver::{HasResolver, Resolver};
use rustc_hash::FxHashMap;
use std::sync::Arc;

pub(crate) fn infer_query(db: &dyn HirDatabase, def: DefWithBodyId) -> Arc<InferenceResult> {
    let resolver = def.resolver(db.upcast());
    let mut icx = BodyInferenceContext::new(db, resolver, def);

    icx.infer_body();

    Arc::new(icx.finish())
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
    pub(crate) owner: TypeVarOwner,
    pub(crate) result: InferenceResult,
    subst: unify::Substitution,
    pub(crate) var_kinds: Vec<Ty>,
    universes: UniverseIndex,
    constraints: Vec<Constraint>,
}

struct BodyInferenceContext<'a> {
    icx: InferenceContext<'a>,
    body: Arc<Body>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ExprOrPatId {
    ExprId(ExprId),
    PatId(PatId),
}

impl From<ExprId> for ExprOrPatId {
    fn from(id: ExprId) -> Self {
        ExprOrPatId::ExprId(id)
    }
}

impl From<PatId> for ExprOrPatId {
    fn from(id: PatId) -> Self {
        ExprOrPatId::PatId(id)
    }
}

impl<'a> InferenceContext<'a> {
    pub(crate) fn new(db: &'a dyn HirDatabase, resolver: Resolver, owner: TypeVarOwner) -> Self {
        InferenceContext {
            db,
            owner,
            resolver,
            result: InferenceResult::default(),
            subst: unify::Substitution::default(),
            var_kinds: Vec::default(),
            universes: UniverseIndex::ROOT,
            constraints: Vec::default(),
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

    pub(crate) fn lang_class(&self, name: &'static str) -> ClassId {
        let module = self.owner.module(self.db.upcast());
        let id = self.db.lang_item(module.lib, name.into()).unwrap();

        id.as_class().unwrap()
    }

    pub(crate) fn fn_type(&self, arg: Ty, ret: Ty) -> Ty {
        let fn_type = self.lang_type("fn-type");
        let base = TyKind::App(fn_type, arg).intern(self.db);

        TyKind::App(base, ret).intern(self.db)
    }

    pub(crate) fn ptr_type(&self, to: Ty) -> Ty {
        let ptr_type = self.lang_type("ptr-type");

        TyKind::App(ptr_type, to).intern(self.db)
    }

    pub(crate) fn array_type(&self, of: Ty, len: i128) -> Ty {
        let array_type = self.lang_type("array-type");
        let base = TyKind::App(array_type, of).intern(self.db);
        let len = TyKind::Figure(len).intern(self.db);

        TyKind::App(base, len).intern(self.db)
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

    pub(crate) fn error(&self) -> Ty {
        TyKind::Error.intern(self.db)
    }

    pub(crate) fn unit(&self) -> Ty {
        TyKind::Tuple(Arc::new([])).intern(self.db)
    }
}

impl<'a> BodyInferenceContext<'a> {
    fn new(db: &'a dyn HirDatabase, resolver: Resolver, owner: DefWithBodyId) -> Self {
        BodyInferenceContext {
            icx: InferenceContext::new(db, resolver, owner.into()),
            body: db.body(owner),
        }
    }

    fn finish(self) -> InferenceResult {
        self.icx.finish()
    }

    fn infer_body(&mut self) {
        self.infer_expr(self.body.body_expr());
    }
}

impl<'a> std::ops::Deref for BodyInferenceContext<'a> {
    type Target = InferenceContext<'a>;

    fn deref(&self) -> &Self::Target {
        &self.icx
    }
}

impl<'a> std::ops::DerefMut for BodyInferenceContext<'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.icx
    }
}

impl InferenceResult {
    pub fn add_diagnostics(&self, db: &dyn HirDatabase, owner: DefWithBodyId, sink: &mut DiagnosticSink) {
        self.diagnostics.iter().for_each(|d| d.add_to(db, owner.into(), sink));
    }
}

pub(crate) mod diagnostics {
    use super::ExprOrPatId;
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
        MismatchedType {
            id: ExprOrPatId,
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
                | InferenceDiagnostic::MismatchedType { id, expected, found } => {
                    let soure_map = match owner {
                        | TypeVarOwner::DefWithBodyId(id) => db.body_source_map(id).1,
                        | _ => return,
                    };

                    let src = match *id {
                        | ExprOrPatId::ExprId(e) => soure_map.expr_syntax(e).unwrap().value.syntax_node_ptr(),
                        | ExprOrPatId::PatId(e) => soure_map.pat_syntax(e).unwrap().value.syntax_node_ptr(),
                    };

                    sink.push(MismatchedType {
                        file,
                        src,
                        expected: *expected,
                        found: *found,
                    });
                },
            }
        }
    }
}
