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
use crate::ty::{Constraint, List, Ty, TyKind};
use diagnostics::InferenceDiagnostic;
use hir_def::arena::ArenaMap;
use hir_def::body::Body;
use hir_def::diagnostic::DiagnosticSink;
use hir_def::expr::ExprId;
use hir_def::id::{ClassId, ContainerId, DefWithBodyId, HasModule, Lookup, MemberId, TypeVarOwner};
use hir_def::pat::PatId;
use hir_def::resolver::Resolver;
use hir_def::type_ref::LocalTypeRefId;
use rustc_hash::FxHashMap;
use std::sync::Arc;

pub(crate) fn infer_query(db: &dyn HirDatabase, def: DefWithBodyId) -> Arc<InferenceResult> {
    let body = db.body(def);
    let resolver = Resolver::for_expr(db.upcast(), def, body.body_expr());
    let mut icx = BodyInferenceContext::new(db, resolver, def);

    match def.container(db.upcast()) {
        | ContainerId::Class(id) => icx.class_owner(id),
        | ContainerId::Member(id) => icx.member_owner(id),
        | ContainerId::Module(_) => {},
    }

    let ty = match def {
        | DefWithBodyId::FuncId(id) => icx.with_owner(TypeVarOwner::TypedDefId(id.into()), |icx| {
            let data = db.func_data(id);
            let mut lcx = LowerCtx::new(data.type_map(), icx);

            data.ty.map(|t| lcx.lower_ty(t)).unwrap_or(lcx.fresh_type())
        }),
        | DefWithBodyId::StaticId(id) => icx.with_owner(TypeVarOwner::TypedDefId(id.into()), |icx| {
            let data = db.static_data(id);
            let mut lcx = LowerCtx::new(data.type_map(), icx);

            data.ty.map(|t| lcx.lower_ty(t)).unwrap_or(lcx.fresh_type())
        }),
        | DefWithBodyId::ConstId(id) => icx.with_owner(TypeVarOwner::TypedDefId(id.into()), |icx| {
            let data = db.const_data(id);
            let mut lcx = LowerCtx::new(data.type_map(), icx);

            data.ty.map(|t| lcx.lower_ty(t)).unwrap_or(lcx.fresh_type())
        }),
    };

    icx.check_body(ty);

    Arc::new(icx.finish())
}

#[derive(Debug, PartialEq, Eq)]
pub struct InferenceResult {
    pub self_type: Ty,
    pub type_of_expr: ArenaMap<ExprId, Ty>,
    pub type_of_pat: ArenaMap<PatId, Ty>,
    pub instances: FxHashMap<ExprId, Vec<Ty>>,
    pub methods: FxHashMap<ExprId, MethodSource>,
    pub(crate) diagnostics: Vec<InferenceDiagnostic>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum MethodSource {
    Member(MemberId),
    Record(usize),
}

pub struct InferenceContext<'a> {
    pub(crate) db: &'a dyn HirDatabase,
    pub(crate) resolver: Resolver,
    pub(crate) owner: TypeVarOwner,
    pub(crate) result: InferenceResult,
    subst: unify::Substitution,
    origin: FxHashMap<Ty, TypeOrigin>,
    pub(crate) var_kinds: Vec<List<Ty>>,
    class_env: ClassEnv,
    member_records: usize,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TypeOrigin {
    ExprId(ExprId),
    PatId(PatId),
    TypeRefId(LocalTypeRefId),
}

impl From<ExprId> for ExprOrPatId {
    fn from(id: ExprId) -> Self {
        Self::ExprId(id)
    }
}

impl From<PatId> for ExprOrPatId {
    fn from(id: PatId) -> Self {
        Self::PatId(id)
    }
}

impl From<ExprId> for TypeOrigin {
    fn from(id: ExprId) -> Self {
        Self::ExprId(id)
    }
}

impl From<PatId> for TypeOrigin {
    fn from(id: PatId) -> Self {
        Self::PatId(id)
    }
}

impl From<LocalTypeRefId> for TypeOrigin {
    fn from(id: LocalTypeRefId) -> Self {
        Self::TypeRefId(id)
    }
}

impl<'a> InferenceContext<'a> {
    pub fn new(db: &'a dyn HirDatabase, resolver: Resolver, owner: TypeVarOwner) -> Self {
        InferenceContext {
            db,
            owner,
            resolver,
            result: InferenceResult {
                self_type: TyKind::Error.intern(db),
                type_of_expr: ArenaMap::default(),
                type_of_pat: ArenaMap::default(),
                instances: FxHashMap::default(),
                methods: FxHashMap::default(),
                diagnostics: Vec::new(),
            },
            subst: unify::Substitution::default(),
            origin: FxHashMap::default(),
            var_kinds: Vec::default(),
            class_env: ClassEnv::default(),
            member_records: 0,
            constraints: Vec::default(),
        }
    }

    pub fn finish(mut self) -> InferenceResult {
        self.finish_mut()
    }

    pub fn finish_mut(&mut self) -> InferenceResult {
        self.solve_constraints();

        let mut res = std::mem::replace(&mut self.result, InferenceResult {
            self_type: TyKind::Error.intern(self.db),
            type_of_expr: ArenaMap::default(),
            type_of_pat: ArenaMap::default(),
            instances: FxHashMap::default(),
            methods: FxHashMap::default(),
            diagnostics: Vec::new(),
        });

        res.self_type = self.generalize(res.self_type);
        res.diagnostics = res.diagnostics.into_iter().map(|i| i.subst_types(&self)).collect();

        let mut finalize = |v: &mut Ty| {
            *v = self.subst_type(*v);
            *v = self.unskolemize(*v);
            *v = v.normalize(self.db);
        };

        res.type_of_expr.values_mut().for_each(&mut finalize);
        res.type_of_pat.values_mut().for_each(&mut finalize);
        res.instances
            .values_mut()
            .for_each(|v| v.iter_mut().for_each(&mut finalize));

        res
    }

    pub(crate) fn lang_type(&self, name: &'static str) -> Ty {
        let module = self.owner.module(self.db.upcast());
        let ty = self.db.lang_item(module.lib, name.into()).unwrap();
        let ty = ty.as_type_ctor().unwrap();

        TyKind::Ctor(ty).intern(self.db)
    }

    pub(crate) fn type_kind(&self) -> Ty {
        let module = self.owner.module(self.db.upcast());
        let ty = self.db.lang_item(module.lib, "type-kind".into()).unwrap();
        let ty = ty.as_type_ctor().unwrap();

        TyKind::Ctor(ty).intern(self.db)
    }

    pub(crate) fn lang_class(&self, name: &'static str) -> ClassId {
        let module = self.owner.module(self.db.upcast());
        let id = self.db.lang_item(module.lib, name.into()).unwrap();

        id.as_class().unwrap()
    }

    pub(crate) fn fn_type(&self, args: List<Ty>, ret: Ty) -> Ty {
        TyKind::Func(args, ret).intern(self.db)
    }

    pub(crate) fn push_var_kind(&mut self, kind: List<Ty>) {
        self.var_kinds.push(kind);
    }

    pub(crate) fn pop_var_kind(&mut self) {
        self.var_kinds.pop().unwrap();
    }

    pub(crate) fn set_origin(&mut self, ty: Ty, origin: impl Into<TypeOrigin>) {
        self.origin.insert(ty, origin.into());
    }

    pub(crate) fn get_origin(&self, ty: Ty) -> Option<TypeOrigin> {
        self.origin.get(&ty).copied()
    }

    pub(crate) fn report(&mut self, diag: InferenceDiagnostic) {
        self.result.diagnostics.push(diag);
    }

    pub(crate) fn report_mismatch(&mut self, expected: Ty, found: Ty, id: impl Into<ExprOrPatId>) {
        let expected_origin = self.get_origin(expected);
        let found_origin = self.get_origin(found);

        self.report(InferenceDiagnostic::MismatchedType {
            id: id.into(),
            expected,
            found,
            expected_origin,
            found_origin,
        });
    }

    pub(crate) fn constrain(&mut self, id: ExprOrPatId, ctnt: Constraint) {
        self.constraints.push((ctnt, id, self.class_env.current()));
    }

    pub(crate) fn error(&self) -> Ty {
        TyKind::Error.intern(self.db)
    }

    pub(crate) fn unit(&self) -> Ty {
        TyKind::Tuple([].into()).intern(self.db)
    }

    fn with_owner<T>(&mut self, id: TypeVarOwner, f: impl FnOnce(&mut Self) -> T) -> T {
        let owner = std::mem::replace(&mut self.owner, id);
        let res = f(self);

        self.owner = owner;
        res
    }

    fn class_owner(&mut self, id: ClassId) {
        let lower = self.db.lower_class(id);

        self.push_var_kind(lower.class.vars.clone());
    }

    fn member_owner(&mut self, id: MemberId) {
        let lower = self.db.lower_member(id);

        self.push_var_kind(lower.member.vars.clone());
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

    fn check_body(&mut self, ann: Ty) {
        if let TypeVarOwner::DefWithBodyId(DefWithBodyId::FuncId(id)) = self.owner {
            let loc = id.lookup(self.db.upcast());

            if let ContainerId::Member(_inst) = loc.container {
                // let data = self.db.func_data(id);
                // let lower = self.db.lower_instance(inst);
                // let class = self.db.class_data(lower.instance.class);
                // let item = class.item(&data.name).unwrap();
                // let mut item_ty = match item {
                //     | AssocItemId::FuncId(id) => self.db.value_ty(id.into()),
                //     | AssocItemId::StaticId(id) => self.db.value_ty(id.into()),
                // };

                // for &ty in lower.instance.types.iter() {
                //     if let TyKind::ForAll(_, t) = item_ty.lookup(self.db) {
                //         item_ty = t.replace_var(self.db, ty);
                //     }
                // }

                // if let TyKind::Ctnt(_, ty) = item_ty.lookup(self.db) {
                //     item_ty = ty;
                // }

                // self.result.self_type = item_ty;

                // while let TyKind::ForAll(kind, ty) = item_ty.lookup(self.db) {
                //     item_ty = self.skolemize(kind, ty);
                // }

                // while let TyKind::Ctnt(ctnt, ty) = item_ty.lookup(self.db) {
                //     self.class_env.push(ctnt, true);
                //     item_ty = ty;
                // }

                // let fn_type_id = self.fn_type_id();

                // for pat in self.body.params().to_vec() {
                //     if let Some([arg, ret]) = item_ty.match_ctor(self.db, fn_type_id) {
                //         self.check_pat(pat, arg);
                //         item_ty = ret;
                //     }
                // }

                // self.ret_type = item_ty;
                // self.check_expr(self.body.body_expr(), item_ty);

                // return;
            }
        }

        match ann.lookup(self.db) {
            | TyKind::ForAll(vars, ty) => {
                self.push_var_kind(vars);
                self.check_body(ty);
                return;
            },
            | TyKind::Ctnt(ctnt, ty) => {
                self.class_env.push(ctnt, false);
                self.check_body(ty);
                return;
            },
            | _ => {},
        }

        let (ret, args) = match ann.lookup(self.db) {
            | TyKind::Func(args, ret) => (ret, Some(args)),
            | _ => (ann, None),
        };

        let body = self.body.clone();
        let ty = if let Some(args) = args {
            for (&pat, &arg) in body.params().iter().zip(args.iter()) {
                self.check_pat(pat, arg);
            }

            ann
        } else if !body.params().is_empty() {
            let args = body.params().iter().map(|&p| self.infer_pat(p)).collect();

            self.fn_type(args, ret)
        } else {
            ann
        };

        self.result.self_type = ty;
        self.ret_type = ret;
        self.check_expr(self.body.body_expr(), ret);

        if let Some(_yield_type) = self.yield_type {
            todo!();
            // let block_type = self.lang_type("block-type");
            // let yield_type = TyKind::App(block_type, yield_type).intern(self.db);
            // let yield_type = TyKind::App(yield_type, ret).intern(self.db);

            // ty = self.fn_type(yield_type, ret);

            // for &pat in self.body.params().iter().rev() {
            //     let arg = self.result.type_of_pat[pat];

            //     ty = self.fn_type(arg, ty);
            // }

            // self.result.self_type = ty;
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
    use super::{ExprOrPatId, InferenceContext, TypeOrigin};
    use crate::db::HirDatabase;
    use crate::diagnostics::*;
    use crate::ty::{Constraint, Ty};
    use hir_def::body::BodySourceMap;
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
        PrivateValue {
            id: ExprOrPatId,
        },
        PrivateType {
            id: LocalTypeRefId,
        },
        PrivateOperator {
            id: ExprOrPatId,
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
            expected_origin: Option<TypeOrigin>,
            found_origin: Option<TypeOrigin>,
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
                | InferenceDiagnostic::MismatchedType {
                    id,
                    expected,
                    found,
                    expected_origin,
                    found_origin,
                } => InferenceDiagnostic::MismatchedType {
                    id,
                    expected: icx.subst_type(expected),
                    found: icx.subst_type(found),
                    expected_origin,
                    found_origin,
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
            let ty_src = |source_map: &BodySourceMap, origin| match origin {
                | TypeOrigin::ExprId(o) => source_map.expr_syntax(o).unwrap().value.syntax_node_ptr(),
                | TypeOrigin::PatId(o) => source_map.pat_syntax(o).unwrap().value.syntax_node_ptr(),
                | TypeOrigin::TypeRefId(o) => {
                    owner.with_type_source_map(db.upcast(), |m| m.type_ref_syntax(o).unwrap().syntax_node_ptr())
                },
            };

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
                | InferenceDiagnostic::PrivateValue { id } => {
                    let soure_map = match owner {
                        | TypeVarOwner::DefWithBodyId(id) => db.body_source_map(id).1,
                        | _ => return,
                    };

                    let src = match *id {
                        | ExprOrPatId::ExprId(e) => soure_map.expr_syntax(e).unwrap().value.syntax_node_ptr(),
                        | ExprOrPatId::PatId(e) => soure_map.pat_syntax(e).unwrap().value.syntax_node_ptr(),
                    };

                    sink.push(PrivateValue { file, src });
                },
                | InferenceDiagnostic::PrivateType { id } => {
                    owner.with_type_source_map(db.upcast(), |source_map| {
                        let src = source_map.type_ref_syntax(*id).unwrap();

                        sink.push(PrivateType { file, src });
                    });
                },
                | InferenceDiagnostic::PrivateOperator { id } => {
                    let soure_map = match owner {
                        | TypeVarOwner::DefWithBodyId(id) => db.body_source_map(id).1,
                        | _ => return,
                    };

                    let src = match *id {
                        | ExprOrPatId::ExprId(e) => soure_map.expr_syntax(e).unwrap().value.syntax_node_ptr(),
                        | ExprOrPatId::PatId(e) => soure_map.pat_syntax(e).unwrap().value.syntax_node_ptr(),
                    };

                    sink.push(PrivateOperator { file, src });
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
                | InferenceDiagnostic::MismatchedType {
                    id,
                    expected,
                    found,
                    expected_origin,
                    found_origin,
                } => {
                    let source_map = match owner {
                        | TypeVarOwner::DefWithBodyId(id) => db.body_source_map(id).1,
                        | _ => return,
                    };

                    let src = match *id {
                        | ExprOrPatId::ExprId(e) => source_map.expr_syntax(e).unwrap().value.syntax_node_ptr(),
                        | ExprOrPatId::PatId(e) => source_map.pat_syntax(e).unwrap().value.syntax_node_ptr(),
                    };

                    let expected_src = expected_origin.map(|o| ty_src(&source_map, o));
                    let found_src = found_origin.map(|o| ty_src(&source_map, o));

                    sink.push(MismatchedType {
                        file,
                        src,
                        expected: *expected,
                        found: *found,
                        expected_src,
                        found_src,
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
