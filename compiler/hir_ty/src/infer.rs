mod ctnt;
mod expr;
mod kind;
mod pat;
mod skolem;
mod subsume;
mod unify;

use crate::class::{ClassEnv, ClassEnvScope};
use crate::db::HirDatabase;
use crate::lower::LowerCtx;
use crate::ty::{Constraint, DebruijnIndex, Ty, TyKind, TypeVar, UniverseIndex};
use diagnostics::InferenceDiagnostic;
use hir_def::arena::ArenaMap;
use hir_def::body::Body;
use hir_def::diagnostic::DiagnosticSink;
use hir_def::expr::ExprId;
use hir_def::id::{AssocItemId, ClassId, ContainerId, DefWithBodyId, FuncId, HasModule, Lookup, TypeVarOwner};
use hir_def::pat::PatId;
use hir_def::resolver::{HasResolver, Resolver};
use rustc_hash::FxHashMap;
use std::sync::Arc;

pub(crate) fn infer_query(db: &dyn HirDatabase, def: DefWithBodyId) -> Arc<InferenceResult> {
    let body = db.body(def);
    let resolver = Resolver::for_expr(db.upcast(), def, body.body_expr());
    let mut icx = BodyInferenceContext::new(db, resolver, def);

    if let DefWithBodyId::FuncId(id) = def {
        let data = db.func_data(id);
        let mut lcx = LowerCtx::new(data.type_map(), &mut icx);
        let var_kinds = data
            .vars
            .iter()
            .rev()
            .map(|&v| {
                let var = &data.type_map()[v];
                let kind = var.kind.map(|k| lcx.lower_ty(k)).unwrap_or(lcx.fresh_kind());

                lcx.push_var_kind(kind);
                kind
            })
            .collect::<Vec<_>>();

        let ctnts = data
            .constraints
            .iter()
            .filter_map(|c| lcx.lower_constraint(c))
            .collect::<Vec<_>>();

        for ctnt in &ctnts {
            icx.class_env.push(ctnt.clone());
        }

        icx.infer_body();

        for ctnt in ctnts {
            icx.result.self_type = TyKind::Ctnt(ctnt, icx.result.self_type).intern(db);
        }

        for kind in var_kinds {
            icx.result.self_type = TyKind::ForAll(kind, icx.result.self_type).intern(db);
        }
    } else {
        icx.infer_body();
    }

    Arc::new(icx.finish())
}

#[derive(Debug, PartialEq, Eq)]
pub struct InferenceResult {
    pub self_type: Ty,
    pub type_of_expr: ArenaMap<ExprId, Ty>,
    pub type_of_pat: ArenaMap<PatId, Ty>,
    pub instances: ArenaMap<ExprId, Vec<Ty>>,
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
    class_env: ClassEnv,
    constraints: Vec<(Constraint, ExprOrPatId, Option<ClassEnvScope>)>,
}

struct BodyInferenceContext<'a> {
    icx: InferenceContext<'a>,
    body: Arc<Body>,
    ret_type: Ty,
    yield_type: Option<Ty>,
    clos_ret_type: Option<Ty>,
    block_ret_type: Option<Ty>,
    block_break_type: Option<Ty>,
    breakable: Vec<Breakable>,
}

#[derive(Clone, Copy)]
enum Breakable {
    Loop(Ty),
    While,
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
            result: InferenceResult {
                self_type: TyKind::Error.intern(db),
                type_of_expr: ArenaMap::default(),
                type_of_pat: ArenaMap::default(),
                instances: ArenaMap::default(),
                diagnostics: Vec::new(),
            },
            subst: unify::Substitution::default(),
            var_kinds: Vec::default(),
            universes: UniverseIndex::ROOT,
            class_env: ClassEnv::default(),
            constraints: Vec::default(),
        }
    }

    pub(crate) fn finish(mut self) -> InferenceResult {
        self.finish_mut()
    }

    pub(crate) fn finish_mut(&mut self) -> InferenceResult {
        self.solve_constraints();

        let mut res = std::mem::replace(&mut self.result, InferenceResult {
            self_type: TyKind::Error.intern(self.db),
            type_of_expr: ArenaMap::default(),
            type_of_pat: ArenaMap::default(),
            instances: ArenaMap::default(),
            diagnostics: Vec::new(),
        });

        res.self_type = self.generalize(res.self_type);
        res.diagnostics = res.diagnostics.into_iter().map(|i| i.subst_types(&self)).collect();
        res.type_of_expr.values_mut().for_each(|v| *v = self.subst_type(*v));
        res.type_of_pat.values_mut().for_each(|v| *v = self.subst_type(*v));
        res.instances.values_mut().for_each(|v| {
            // v.reverse();
            v.iter_mut().for_each(|t| *t = self.subst_type(*t));
        });

        res
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

    pub(crate) fn report_mismatch(&mut self, expected: Ty, found: Ty, id: ExprOrPatId) {
        self.report(InferenceDiagnostic::MismatchedType { id, expected, found });
    }

    pub(crate) fn constrain(&mut self, id: ExprOrPatId, ctnt: Constraint) {
        self.constraints.push((ctnt, id, self.class_env.current()));
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
        let error = TyKind::Error.intern(db);

        BodyInferenceContext {
            icx: InferenceContext::new(db, resolver, owner.into()),
            body: db.body(owner),
            ret_type: error,
            yield_type: None,
            clos_ret_type: None,
            block_ret_type: None,
            block_break_type: None,
            breakable: Vec::new(),
        }
    }

    fn finish(self) -> InferenceResult {
        self.icx.finish()
    }

    fn infer_body(&mut self) {
        let ret = self.fresh_type();
        let mut ty = ret;

        for pat in self.body.params().to_vec().into_iter().rev() {
            let arg = self.infer_pat(pat);

            ty = self.fn_type(arg, ty);
        }

        self.result.self_type = ty;
        self.ret_type = ret;
        self.check_expr(self.body.body_expr(), ret);

        if let Some(yield_type) = self.yield_type {
            let block_type = self.lang_type("block-type");
            let yield_type = TyKind::App(block_type, yield_type).intern(self.db);
            let yield_type = TyKind::App(yield_type, ret).intern(self.db);

            ty = self.fn_type(yield_type, ret);

            for &pat in self.body.params().iter().rev() {
                let arg = self.result.type_of_pat[pat];

                ty = self.fn_type(arg, ty);
            }

            self.result.self_type = ty;
        }

        if let TypeVarOwner::DefWithBodyId(DefWithBodyId::FuncId(id)) = self.owner {
            let loc = id.lookup(self.db.upcast());

            if let ContainerId::Instance(inst) = loc.container {
                let data = self.db.func_data(id);
                let lower = self.db.lower_instance(inst);
                let class = self.db.class_data(lower.instance.class);
                let item = class.item(&data.name).unwrap();
                let item_ty = match item {
                    | AssocItemId::FuncId(id) => self.db.value_ty(id.into()),
                    | AssocItemId::StaticId(id) => self.db.value_ty(id.into()),
                };

                let expr = self.body.body_expr();
                let item_ty = self.instantiate(item_ty, expr.into());
                let self_type = self.result.self_type;
                let body_expr = self.body.body_expr();

                if !self.subsume_types(item_ty, self_type, body_expr.into()) {
                    self.report(InferenceDiagnostic::MismatchedType {
                        id: body_expr.into(),
                        expected: item_ty,
                        found: self_type,
                    });
                }
            }
        }
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
    use super::{ExprOrPatId, InferenceContext};
    use crate::db::HirDatabase;
    use crate::diagnostics::*;
    use crate::ty::{Constraint, Ty};
    use hir_def::diagnostic::DiagnosticSink;
    use hir_def::expr::ExprId;
    use hir_def::id::{HasSource, TypeVarOwner};
    use hir_def::type_ref::LocalTypeRefId;

    #[derive(Debug, PartialEq, Eq)]
    pub enum InferenceDiagnostic {
        UnresolvedType {
            id: LocalTypeRefId,
        },
        UnresolvedValue {
            id: ExprOrPatId,
        },
        UnresolvedOperator {
            id: ExprId,
        },
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
        UnsolvedConstraint {
            id: ExprOrPatId,
            ctnt: Constraint,
        },
        BreakOutsideLoop {
            id: ExprId,
        },
        CannotBreakWithValue {
            id: ExprId,
        },
        NextOutsideLoop {
            id: ExprId,
        },
        CannotNextWithValue {
            id: ExprId,
        },
    }

    impl InferenceDiagnostic {
        pub(super) fn subst_types(self, icx: &InferenceContext) -> Self {
            match self {
                | InferenceDiagnostic::MismatchedKind { id, expected, found } => InferenceDiagnostic::MismatchedKind {
                    id,
                    expected: icx.subst_type(expected),
                    found: icx.subst_type(found),
                },
                | InferenceDiagnostic::MismatchedType { id, expected, found } => InferenceDiagnostic::MismatchedType {
                    id,
                    expected: icx.subst_type(expected),
                    found: icx.subst_type(found),
                },
                | InferenceDiagnostic::UnsolvedConstraint { id, ctnt } => InferenceDiagnostic::UnsolvedConstraint {
                    id,
                    ctnt: icx.subst_ctnt(&ctnt),
                },
                | _ => self,
            }
        }

        pub fn add_to(&self, db: &dyn HirDatabase, owner: TypeVarOwner, sink: &mut DiagnosticSink) {
            let file = owner.source(db.upcast()).file_id;

            match self {
                | InferenceDiagnostic::UnresolvedType { id } => {
                    owner.with_type_source_map(db.upcast(), |source_map| {
                        let src = source_map.type_ref_syntax(*id).unwrap();

                        sink.push(UnresolvedType { file, ty: src });
                    });
                },
                | InferenceDiagnostic::UnresolvedValue { id } => {
                    let soure_map = match owner {
                        | TypeVarOwner::DefWithBodyId(id) => db.body_source_map(id).1,
                        | _ => return,
                    };

                    let src = match *id {
                        | ExprOrPatId::ExprId(e) => soure_map.expr_syntax(e).unwrap().value.syntax_node_ptr(),
                        | ExprOrPatId::PatId(e) => soure_map.pat_syntax(e).unwrap().value.syntax_node_ptr(),
                    };

                    sink.push(UnresolvedValue { file, src });
                },
                | InferenceDiagnostic::UnresolvedOperator { id } => {
                    let soure_map = match owner {
                        | TypeVarOwner::DefWithBodyId(id) => db.body_source_map(id).1,
                        | _ => return,
                    };

                    let src = soure_map.expr_syntax(*id).unwrap().value.syntax_node_ptr();

                    sink.push(UnresolvedOperator { file, src });
                },
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
                | InferenceDiagnostic::UnsolvedConstraint { id, ctnt } => {
                    let soure_map = match owner {
                        | TypeVarOwner::DefWithBodyId(id) => db.body_source_map(id).1,
                        | _ => return,
                    };

                    let src = match *id {
                        | ExprOrPatId::ExprId(e) => soure_map.expr_syntax(e).unwrap().value.syntax_node_ptr(),
                        | ExprOrPatId::PatId(e) => soure_map.pat_syntax(e).unwrap().value.syntax_node_ptr(),
                    };

                    sink.push(UnsolvedConstraint {
                        file,
                        src,
                        ctnt: ctnt.clone(),
                    });
                },
                | InferenceDiagnostic::BreakOutsideLoop { id } => {
                    let source_map = match owner {
                        | TypeVarOwner::DefWithBodyId(id) => db.body_source_map(id).1,
                        | _ => return,
                    };

                    let src = source_map.expr_syntax(*id).unwrap().value.syntax_node_ptr();

                    sink.push(BreakOutsideLoop { file, src });
                },
                | InferenceDiagnostic::CannotBreakWithValue { id } => {
                    let source_map = match owner {
                        | TypeVarOwner::DefWithBodyId(id) => db.body_source_map(id).1,
                        | _ => return,
                    };

                    let src = source_map.expr_syntax(*id).unwrap().value.syntax_node_ptr();

                    sink.push(CannotBreakWithValue { file, src });
                },
                | InferenceDiagnostic::NextOutsideLoop { id } => {
                    let source_map = match owner {
                        | TypeVarOwner::DefWithBodyId(id) => db.body_source_map(id).1,
                        | _ => return,
                    };

                    let src = source_map.expr_syntax(*id).unwrap().value.syntax_node_ptr();

                    sink.push(NextOutsideLoop { file, src });
                },
                | InferenceDiagnostic::CannotNextWithValue { id } => {
                    let source_map = match owner {
                        | TypeVarOwner::DefWithBodyId(id) => db.body_source_map(id).1,
                        | _ => return,
                    };

                    let src = source_map.expr_syntax(*id).unwrap().value.syntax_node_ptr();

                    sink.push(CannotNextWithValue { file, src });
                },
            }
        }
    }
}
