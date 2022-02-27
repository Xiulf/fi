mod ctnt;
mod expr;
mod kind;
mod pat;
mod skolem;
mod subsume;
mod unify;

use crate::class::{ClassEnv, ClassEnvScope};
use crate::db::HirDatabase;
use crate::info::{CtntInfo, FromInfo, ToInfo, TyId, TyInfo, TySource, TypeOrigin, TypeVarScopeId, TypeVars, Types};
use crate::lower::LowerCtx;
use crate::ty::{Constraint, List, Ty, TypeVar, WhereClause};
use arena::ArenaMap;
use diagnostics::InferenceDiagnostic;
use hir_def::body::Body;
use hir_def::diagnostic::DiagnosticSink;
use hir_def::expr::ExprId;
use hir_def::id::{AssocItemId, ClassId, ContainerId, DefWithBodyId, HasModule, MemberId, TypeVarOwner};
use hir_def::name::Name;
use hir_def::pat::PatId;
use hir_def::resolver::Resolver;
use hir_def::type_ref::{LocalTypeRefId, LocalTypeVarId};
use rustc_hash::FxHashMap;
use std::sync::Arc;

pub(crate) fn infer_query(db: &dyn HirDatabase, def: DefWithBodyId) -> Arc<InferenceResult<Ty, Constraint>> {
    let body = db.body(def);
    let resolver = Resolver::for_expr(db.upcast(), def, body.body_expr());
    let mut icx = BodyInferenceContext::new(db, resolver, def);

    match def.container(db.upcast()) {
        | ContainerId::Class(id) => icx.class_owner(id),
        | ContainerId::Member(id) => icx.member_owner(id),
        | ContainerId::Module(_) => {},
    }

    let (ty, item) = match def {
        | DefWithBodyId::FuncId(id) => icx.with_owner(TypeVarOwner::TypedDefId(id.into()), |icx| {
            let data = db.func_data(id);
            let mut lcx = LowerCtx::new(data.type_map(), icx);
            let src = lcx.source(TypeOrigin::Def(id.into()));
            let scope = lcx.push_type_vars(&data.type_vars);
            let ty = data
                .ty
                .map(|t| lcx.lower_ty(t))
                .unwrap_or(lcx.fresh_type_without_kind(src));

            (lcx.wrap_type_vars(ty, scope, src), data.name.clone())
        }),
        | DefWithBodyId::StaticId(id) => icx.with_owner(TypeVarOwner::TypedDefId(id.into()), |icx| {
            let data = db.static_data(id);
            let mut lcx = LowerCtx::new(data.type_map(), icx);
            let src = lcx.source(TypeOrigin::Def(id.into()));

            (
                data.ty
                    .map(|t| lcx.lower_ty(t))
                    .unwrap_or(lcx.fresh_type_without_kind(src)),
                data.name.clone(),
            )
        }),
        | DefWithBodyId::ConstId(id) => icx.with_owner(TypeVarOwner::TypedDefId(id.into()), |icx| {
            let data = db.const_data(id);
            let mut lcx = LowerCtx::new(data.type_map(), icx);
            let src = lcx.source(TypeOrigin::Def(id.into()));

            (
                data.ty
                    .map(|t| lcx.lower_ty(t))
                    .unwrap_or(lcx.fresh_type_without_kind(src)),
                data.name.clone(),
            )
        }),
    };

    let ty = match def.container(db.upcast()) {
        | ContainerId::Class(id) => icx.class_item(id, ty),
        | ContainerId::Member(id) => icx.member_item(id, ty, &item, &body),
        | _ => ty,
    };

    icx.result.self_type = ty;

    if icx.result.diagnostics.is_empty() {
        icx.check_body(ty, matches!(def, DefWithBodyId::FuncId(_)));
    }

    Arc::new(icx.finish())
}

#[derive(Debug, PartialEq, Eq)]
pub struct InferenceResult<T, C> {
    pub self_type: T,
    pub type_of_expr: ArenaMap<ExprId, T>,
    pub type_of_pat: ArenaMap<PatId, T>,
    pub kind_of_ty: ArenaMap<LocalTypeRefId, T>,
    pub instances: FxHashMap<ExprId, Vec<T>>,
    pub methods: FxHashMap<ExprId, MethodSource>,
    pub(crate) diagnostics: Vec<InferenceDiagnostic<T, C>>,
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
    pub(crate) result: InferenceResult<TyId, CtntInfo>,
    pub(crate) types: Types,
    pub(crate) type_vars: TypeVars,
    subst: unify::Substitution,
    class_env: ClassEnv,
    member_records: usize,
    constraints: Vec<(CtntInfo, ExprOrPatId, Option<ClassEnvScope>)>,
}

struct BodyInferenceContext<'a> {
    icx: InferenceContext<'a>,
    body: Arc<Body>,
    ret_type: TyId,
    yield_type: Option<TyId>,
    clos_ret_type: Option<TyId>,
    block_ret_type: Option<TyId>,
    block_break_type: Option<TyId>,
    breakable: Vec<Breakable>,
}

#[derive(Clone, Copy)]
enum Breakable {
    Loop(TyId),
    While,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ExprOrPatId {
    ExprId(ExprId),
    PatId(PatId),
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

impl<'a> InferenceContext<'a> {
    pub fn new(db: &'a dyn HirDatabase, resolver: Resolver, owner: TypeVarOwner) -> Self {
        let mut types = Types::default();
        let src = (owner, TypeOrigin::Synthetic);
        let self_type = types.insert(TyInfo::Error, src);

        InferenceContext {
            db,
            owner,
            resolver,
            types,
            result: InferenceResult {
                self_type,
                type_of_expr: ArenaMap::default(),
                type_of_pat: ArenaMap::default(),
                kind_of_ty: ArenaMap::default(),
                instances: FxHashMap::default(),
                methods: FxHashMap::default(),
                diagnostics: Vec::new(),
            },
            subst: unify::Substitution::default(),
            type_vars: TypeVars::default(),
            class_env: ClassEnv::default(),
            member_records: 0,
            constraints: Vec::default(),
        }
    }

    pub fn finish(mut self) -> InferenceResult<Ty, Constraint> {
        self.finish_mut()
    }

    pub fn finish_mut(&mut self) -> InferenceResult<Ty, Constraint> {
        self.solve_constraints();

        let self_type = self.types.insert(TyInfo::Error, (self.owner, TypeOrigin::Synthetic));
        let mut res = std::mem::replace(&mut self.result, InferenceResult {
            self_type,
            type_of_expr: ArenaMap::default(),
            type_of_pat: ArenaMap::default(),
            kind_of_ty: ArenaMap::default(),
            instances: FxHashMap::default(),
            methods: FxHashMap::default(),
            diagnostics: Vec::new(),
        });

        res.self_type = self.generalize(res.self_type);
        res.diagnostics = res.diagnostics.into_iter().map(|i| i.subst_types(self)).collect();

        let mut finalize = |v: &mut TyId| {
            *v = self.subst_type(*v);
            *v = self.unskolemize(*v);
            *v = v.normalize(&mut self.types);
        };

        res.type_of_expr.values_mut().for_each(&mut finalize);
        res.type_of_pat.values_mut().for_each(&mut finalize);
        res.kind_of_ty.values_mut().for_each(&mut finalize);
        res.instances
            .values_mut()
            .for_each(|v| v.iter_mut().for_each(&mut finalize));

        InferenceResult::from_info(self.db, &self.types, res)
    }

    pub fn convert_ty(&self, ty: TyId) -> Ty {
        Ty::from_info(self.db, &self.types, ty)
    }

    pub(crate) fn lang_type(&mut self, name: &'static str, src: TySource) -> TyId {
        let module = self.owner.module(self.db.upcast());
        let ty = self.db.lang_item(module.lib, name.into()).unwrap();
        let ty = ty.as_type_ctor().unwrap();

        self.types.insert(TyInfo::Ctor(ty), src)
    }

    pub(crate) fn type_kind(&mut self, src: TySource) -> TyId {
        let module = self.owner.module(self.db.upcast());
        let ty = self.db.lang_item(module.lib, "type-kind".into()).unwrap();
        let ty = ty.as_type_ctor().unwrap();

        self.types.insert(TyInfo::Ctor(ty), src)
    }

    pub(crate) fn lang_class(&self, name: &'static str) -> ClassId {
        let module = self.owner.module(self.db.upcast());
        let id = self.db.lang_item(module.lib, name.into()).unwrap();

        id.as_class().unwrap()
    }

    pub(crate) fn fn_type(&mut self, args: List<TyId>, ret: TyId, src: TySource) -> TyId {
        self.types.insert(TyInfo::Func(args, ret), src)
    }

    pub(crate) fn report(&mut self, diag: InferenceDiagnostic<TyId, CtntInfo>) {
        self.result.diagnostics.push(diag);
    }

    pub(crate) fn report_mismatch(&mut self, expected: TyId, found: TyId, id: impl Into<ExprOrPatId>) {
        let expected_src = self.types.source(expected);
        let found_src = self.types.source(found);

        self.report(InferenceDiagnostic::MismatchedType {
            id: id.into(),
            expected,
            found,
            expected_src,
            found_src,
        });
    }

    pub(crate) fn constrain(&mut self, id: ExprOrPatId, ctnt: CtntInfo) {
        self.constraints.push((ctnt, id, self.class_env.current()));
    }

    pub(crate) fn error(&mut self, src: TySource) -> TyId {
        self.types.insert(TyInfo::Error, src)
    }

    pub(crate) fn unit(&mut self, src: TySource) -> TyId {
        self.types.insert(TyInfo::Tuple([].into()), src)
    }

    pub(crate) fn source(&self, origin: impl Into<TypeOrigin>) -> TySource {
        (self.owner, origin.into())
    }

    pub(crate) fn push_type_vars(&mut self, vars: &[LocalTypeVarId]) -> TypeVarScopeId {
        let kinds = vars
            .iter()
            .map(|&var| {
                let src = self.source(var);

                self.fresh_type(src)
            })
            .collect();

        self.type_vars.alloc_scope(kinds)
    }

    pub(crate) fn wrap_type_vars(&mut self, inner: TyId, scope: TypeVarScopeId, src: TySource) -> TyId {
        if !self.type_vars.var_kinds(scope).is_empty() {
            let kinds = self.type_vars.var_kinds(scope).clone();

            self.types.insert(TyInfo::ForAll(kinds, inner, scope), src)
        } else {
            inner
        }
    }

    fn with_owner<T>(&mut self, id: TypeVarOwner, f: impl FnOnce(&mut Self) -> T) -> T {
        let owner = std::mem::replace(&mut self.owner, id);
        let res = f(self);

        self.owner = owner;
        res
    }

    fn class_owner(&mut self, id: ClassId) {
        let lower = self.db.lower_class(id);
        let src = self.source(TypeOrigin::Synthetic);
        let vars = lower
            .class
            .vars
            .iter()
            .map(|&v| v.to_info(self.db, &mut self.types, src))
            .collect();

        self.type_vars.alloc_scope(vars);
    }

    fn member_owner(&mut self, id: MemberId) {
        let lower = self.db.lower_member(id);
        let src = self.source(TypeOrigin::Synthetic);
        let vars = lower
            .member
            .vars
            .iter()
            .map(|&v| v.to_info(self.db, &mut self.types, src))
            .collect();

        self.type_vars.alloc_scope(vars);
    }

    fn class_item(&mut self, class: ClassId, ann: TyId) -> TyId {
        let src = self.source(TypeOrigin::Def(class.into()));
        let lower = self.db.lower_class(class);
        let scope = self.type_vars.scope_at(1);
        let types = (0..lower.class.vars.len() as u32)
            .map(|i| self.types.insert(TyInfo::TypeVar(TypeVar::new(i, scope)), src))
            .collect::<List<_>>();

        let kinds = lower
            .class
            .vars
            .iter()
            .map(|t| t.to_info(self.db, &mut self.types, src))
            .collect::<List<_>>();

        let where_clause = WhereClause {
            constraints: [CtntInfo { class, types }].into(),
        };

        let item_ty = self.types.insert(TyInfo::Where(where_clause, ann), src);

        self.types.insert(TyInfo::ForAll(kinds, item_ty, scope), src)
    }

    fn member_item(&mut self, member: MemberId, ann: TyId, item: &Name, body: &Body) -> TyId {
        let src = self.source(TypeOrigin::Synthetic);
        let lower = self.db.lower_member(member);
        let class = self.db.class_data(lower.member.class);
        let item = class.item(item).unwrap();
        let item_ty = match item {
            | AssocItemId::FuncId(id) => self.db.value_ty(id.into()),
            | AssocItemId::StaticId(id) => self.db.value_ty(id.into()),
        };

        let types = lower
            .member
            .types
            .iter()
            .map(|t| t.to_info(self.db, &mut self.types, src))
            .collect::<Vec<_>>();

        let kinds = lower
            .member
            .vars
            .iter()
            .map(|t| t.to_info(self.db, &mut self.types, src))
            .collect::<List<_>>();

        let item_ty = item_ty.to_info(self.db, &mut self.types, src);
        let mut item_ty = match self.types[item_ty].clone() {
            | TyInfo::ForAll(_, inner, scope) => inner.replace_vars(&mut self.types, &types, scope),
            | _ => item_ty,
        };

        if !lower.member.where_clause.constraints.is_empty() {
            let where_clause = lower.member.where_clause.clone().to_info(self.db, &mut self.types, src);

            item_ty = self.types.insert(TyInfo::Where(where_clause, item_ty), src);
        }

        if !kinds.is_empty() {
            let scope = self.type_vars.scope_at(1);

            item_ty = self.types.insert(TyInfo::ForAll(kinds, item_ty, scope), src)
        }

        if !self.unify_types(item_ty, ann) {
            self.report_mismatch(item_ty, ann, body.body_expr());
        }

        item_ty
    }
}

impl<'a> BodyInferenceContext<'a> {
    fn new(db: &'a dyn HirDatabase, resolver: Resolver, owner: DefWithBodyId) -> Self {
        let mut icx = InferenceContext::new(db, resolver, owner.into());
        let src = icx.source(TypeOrigin::Synthetic);
        let error = icx.types.insert(TyInfo::Error, src);

        BodyInferenceContext {
            icx,
            body: db.body(owner),
            ret_type: error,
            yield_type: None,
            clos_ret_type: None,
            block_ret_type: None,
            block_break_type: None,
            breakable: Vec::new(),
        }
    }

    fn finish(self) -> InferenceResult<Ty, Constraint> {
        self.icx.finish()
    }

    fn check_body(&mut self, ann: TyId, is_func: bool) {
        match self.types[ann].clone() {
            | TyInfo::ForAll(_, ty, scope) => {
                self.type_vars.push_scope(scope);
                self.check_body(ty, is_func);
                return;
            },
            | TyInfo::Where(where_, ty) => {
                for ctnt in where_.constraints.iter() {
                    self.class_env.push(ctnt.clone(), false);
                }

                self.check_body(ty, is_func);
                return;
            },
            | _ => {},
        }

        let body = self.body.clone();
        let ret = match self.types[ann] {
            | TyInfo::Func(_, ret) => ret,
            | _ => {
                let src = self.source(body.body_expr());

                self.fresh_type(src)
            },
        };

        let args = body
            .params()
            .iter()
            .map(|&pat| self.infer_pat(pat))
            .collect::<List<_>>();

        let ty = if !args.is_empty() || is_func {
            let src = self.types.source(ann);

            self.fn_type(args, ret, src)
        } else {
            ann
        };

        if body.has_body() && !self.unify_types(ann, ty) {
            self.report_mismatch(ann, ty, body.body_expr());
        }

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

impl InferenceResult<Ty, Constraint> {
    pub fn add_diagnostics(&self, db: &dyn HirDatabase, owner: DefWithBodyId, sink: &mut DiagnosticSink) {
        self.diagnostics.iter().for_each(|d| d.add_to(db, owner.into(), sink));
    }
}

pub(crate) mod diagnostics {
    use super::{ExprOrPatId, InferenceContext};
    use crate::db::HirDatabase;
    use crate::diagnostics::*;
    use crate::info::{CtntInfo, TyId, TySource, TypeOrigin};
    use crate::ty::{Constraint, Ty};
    use hir_def::diagnostic::DiagnosticSink;
    use hir_def::expr::ExprId;
    use hir_def::id::{ClassId, HasSource, MemberId, TypeVarOwner};
    use hir_def::type_ref::{LocalTypeRefId, TypeVarSource};
    use syntax::{AstNode, SyntaxNodePtr};

    #[derive(Debug, Clone, PartialEq, Eq)]
    pub enum ClassSource {
        TyCtnt(LocalTypeRefId),
        MemberCtnt(MemberId, usize),
        ClassCtnt(ClassId, usize),
        Member(MemberId),
    }

    #[derive(Debug, Clone, PartialEq, Eq)]
    pub enum InferenceDiagnostic<T, C> {
        UnresolvedValue {
            id: ExprOrPatId,
        },
        UnresolvedType {
            owner: TypeVarOwner,
            id: LocalTypeRefId,
        },
        UnresolvedClass {
            src: ClassSource,
        },
        UnresolvedOperator {
            id: ExprId,
        },
        PrivateValue {
            id: ExprOrPatId,
        },
        PrivateType {
            owner: TypeVarOwner,
            id: LocalTypeRefId,
        },
        PrivateClass {
            src: ClassSource,
        },
        PrivateOperator {
            id: ExprOrPatId,
        },
        MismatchedKind {
            expected: T,
            found: T,
            expected_src: TySource,
            found_src: TySource,
        },
        MismatchedType {
            id: ExprOrPatId,
            expected: T,
            found: T,
            expected_src: TySource,
            found_src: TySource,
        },
        UnsolvedConstraint {
            id: ExprOrPatId,
            ctnt: C,
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

    impl InferenceDiagnostic<TyId, CtntInfo> {
        pub(super) fn subst_types(self, icx: &mut InferenceContext) -> Self {
            match self {
                | InferenceDiagnostic::MismatchedKind {
                    expected,
                    found,
                    expected_src,
                    found_src,
                } => InferenceDiagnostic::MismatchedKind {
                    expected: icx.subst_type(expected),
                    found: icx.subst_type(found),
                    expected_src,
                    found_src,
                },
                | InferenceDiagnostic::MismatchedType {
                    id,
                    expected,
                    found,
                    expected_src,
                    found_src,
                } => InferenceDiagnostic::MismatchedType {
                    id,
                    expected: icx.subst_type(expected),
                    found: icx.subst_type(found),
                    expected_src,
                    found_src,
                },
                | InferenceDiagnostic::UnsolvedConstraint { id, ctnt } => InferenceDiagnostic::UnsolvedConstraint {
                    id,
                    ctnt: icx.subst_ctnt(&ctnt),
                },
                | _ => self,
            }
        }
    }

    impl InferenceDiagnostic<Ty, Constraint> {
        pub fn add_to(&self, db: &dyn HirDatabase, owner: TypeVarOwner, sink: &mut DiagnosticSink) {
            let file = owner.source(db.upcast()).file_id;
            let ty_src = |src: TySource| match src.1 {
                | TypeOrigin::ExprId(id) => match src.0 {
                    | TypeVarOwner::DefWithBodyId(owner) => {
                        let source_map = db.body_source_map(owner).1;

                        source_map.expr_syntax(id).ok().map(|s| s.map(|v| v.syntax_node_ptr()))
                    },
                    | _ => None,
                },
                | TypeOrigin::PatId(id) => match src.0 {
                    | TypeVarOwner::DefWithBodyId(owner) => {
                        let source_map = db.body_source_map(owner).1;

                        source_map.pat_syntax(id).ok().map(|s| s.map(|v| v.syntax_node_ptr()))
                    },
                    | _ => None,
                },
                | TypeOrigin::TypeRefId(id) => src.0.with_type_source_map(db.upcast(), |source_map| {
                    let file = src.0.source(db.upcast());

                    source_map
                        .type_ref_syntax(id)
                        .map(|s| file.with_value(s.syntax_node_ptr()))
                }),
                | TypeOrigin::TypeVarId(id) => src.0.with_type_source_map(db.upcast(), |source_map| {
                    let file = src.0.source(db.upcast());

                    source_map.type_var_syntax(id).map(|s| match s {
                        | TypeVarSource::Type(s) => file.with_value(s.syntax_node_ptr()),
                        | TypeVarSource::NameRef(s) => file.with_value(s.syntax_node_ptr()),
                    })
                }),
                | TypeOrigin::Def(id) => Some(id.source(db.upcast()).map(|v| SyntaxNodePtr::new(v.syntax()))),
                | TypeOrigin::Synthetic => None,
            };

            match self {
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
                | InferenceDiagnostic::UnresolvedType { owner, id } => {
                    owner.with_type_source_map(db.upcast(), |source_map| {
                        let src = source_map.type_ref_syntax(*id).unwrap();

                        sink.push(UnresolvedType { file, ty: src });
                    });
                },
                | InferenceDiagnostic::UnresolvedClass { src } => {
                    let root = db.parse(file).syntax_node();
                    let node = match src {
                        | ClassSource::TyCtnt(id) => {
                            owner.with_type_source_map(db.upcast(), |source_map| {
                                let src = source_map.type_ref_syntax(*id).unwrap();
                                src.syntax_node_ptr().to_node(&root)
                            });
                        },
                        | ClassSource::ClassCtnt(id, idx) => {},
                        | ClassSource::MemberCtnt(id, idx) => {},
                        | ClassSource::Member(id) => {},
                    };

                    let src = todo!();

                    sink.push(UnresolvedClass { file, src });
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
                | InferenceDiagnostic::PrivateType { owner, id } => {
                    owner.with_type_source_map(db.upcast(), |source_map| {
                        let src = source_map.type_ref_syntax(*id).unwrap();

                        sink.push(PrivateType { file, src });
                    });
                },
                | InferenceDiagnostic::PrivateClass { src } => {
                    let root = db.parse(file).syntax_node();
                    let node = match src {
                        | ClassSource::TyCtnt(id) => {
                            owner.with_type_source_map(db.upcast(), |source_map| {
                                let src = source_map.type_ref_syntax(*id).unwrap();
                                src.syntax_node_ptr().to_node(&root)
                            });
                        },
                        | ClassSource::ClassCtnt(id, idx) => {},
                        | ClassSource::MemberCtnt(id, idx) => {},
                        | ClassSource::Member(id) => {},
                    };

                    let src = todo!();

                    sink.push(PrivateClass { file, src });
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
                | InferenceDiagnostic::MismatchedKind {
                    expected,
                    found,
                    expected_src,
                    found_src,
                } => {
                    let expected_src = ty_src(*expected_src);
                    let found_src = ty_src(*found_src).or(expected_src).unwrap();

                    sink.push(MismatchedKind {
                        found: *found,
                        expected: *expected,
                        expected_src,
                        found_src,
                    });
                },
                | InferenceDiagnostic::MismatchedType {
                    id,
                    expected,
                    found,
                    expected_src,
                    found_src,
                } => {
                    let source_map = match owner {
                        | TypeVarOwner::DefWithBodyId(id) => db.body_source_map(id).1,
                        | _ => return,
                    };

                    let src = match *id {
                        | ExprOrPatId::ExprId(e) => source_map.expr_syntax(e).unwrap().value.syntax_node_ptr(),
                        | ExprOrPatId::PatId(e) => source_map.pat_syntax(e).unwrap().value.syntax_node_ptr(),
                    };

                    let expected_src = ty_src(*expected_src);
                    let found_src = ty_src(*found_src);

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
