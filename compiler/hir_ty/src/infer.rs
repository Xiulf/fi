mod ctnt;
mod expr;
mod kind;
mod pat;
mod skolem;
mod subsume;
mod unify;

use std::iter::FromIterator;
use std::sync::Arc;

use arena::ArenaMap;
use diagnostics::InferenceDiagnostic;
use hir_def::body::Body;
use hir_def::diagnostic::DiagnosticSink;
use hir_def::expr::ExprId;
use hir_def::id::{
    AssocItemId, ClassId, ContainerId, DefWithBodyId, FixityId, HasModule, MemberId, TypeCtorId, TypeVarOwner,
};
use hir_def::infix::ProcessInfix;
use hir_def::name::Name;
use hir_def::pat::PatId;
use hir_def::path::Path;
use hir_def::resolver::Resolver;
use hir_def::type_ref::{LocalTypeRefId, LocalTypeVarId};
use rustc_hash::FxHashMap;
use smallvec::SmallVec;

use self::diagnostics::{CtntExpected, CtntFound};
use crate::class::{ClassEnv, ClassEnvScope};
use crate::db::HirDatabase;
use crate::infer::diagnostics::OperatorSource;
use crate::info::{CtntInfo, FromInfo, ToInfo, TyId, TyInfo, TySource, TypeOrigin, TypeVarScopeId, TypeVars, Types};
use crate::lower::LowerCtx;
use crate::ty::{Constraint, List, Ty, TyAndSrc, TypeVar, WhereClause};

pub(crate) fn infer_query(db: &dyn HirDatabase, def: DefWithBodyId) -> Arc<InferenceResult<Ty, Constraint>> {
    let body = db.body(def);
    let resolver = Resolver::for_expr(db.upcast(), def, body.body_expr());
    let has_annotation = match def {
        | DefWithBodyId::FuncId(id) => db.func_data(id).ty.is_some(),
        | DefWithBodyId::ConstId(id) => db.const_data(id).ty.is_some(),
        | DefWithBodyId::StaticId(id) => db.static_data(id).ty.is_some(),
    };

    let mut icx = BodyInferenceContext::new(db, resolver, def, !has_annotation);

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
            let ty = data.ty.map(|t| lcx.lower_ty(t)).unwrap_or(lcx.fresh_type(src));

            lcx.check_kind_type(ty);
            (ty, data.name.clone())
        }),
        | DefWithBodyId::StaticId(id) => icx.with_owner(TypeVarOwner::TypedDefId(id.into()), |icx| {
            let data = db.static_data(id);
            let mut lcx = LowerCtx::new(data.type_map(), icx);
            let src = lcx.source(TypeOrigin::Def(id.into()));
            let ty = data.ty.map(|t| lcx.lower_ty(t)).unwrap_or(lcx.fresh_type(src));

            lcx.check_kind_type(ty);
            (ty, data.name.clone())
        }),
        | DefWithBodyId::ConstId(id) => icx.with_owner(TypeVarOwner::TypedDefId(id.into()), |icx| {
            let data = db.const_data(id);
            let mut lcx = LowerCtx::new(data.type_map(), icx);
            let src = lcx.source(TypeOrigin::Def(id.into()));
            let ty = data.ty.map(|t| lcx.lower_ty(t)).unwrap_or(lcx.fresh_type(src));

            lcx.check_kind_type(ty);
            (ty, data.name.clone())
        }),
    };

    let ty = match def.container(db.upcast()) {
        | ContainerId::Class(id) => icx.class_item(id, ty),
        | ContainerId::Member(id) => icx.member_item(id, ty, &item, &body),
        | _ => ty,
    };

    icx.result.self_type.ty = ty;

    if icx.result.diagnostics.is_empty() {
        icx.check_body(ty, matches!(def, DefWithBodyId::FuncId(_)));
    }

    Arc::new(icx.finish())
}

pub(crate) fn infer_recover(
    db: &dyn HirDatabase,
    _cycle: &Vec<String>,
    def: &DefWithBodyId,
) -> Arc<InferenceResult<Ty, Constraint>> {
    let owner = TypeVarOwner::from(*def);
    let src = (owner, TypeOrigin::Def(owner.into()));
    let self_type = crate::ty::TyKind::Error(crate::ty::Reason::Error).intern(db);
    let diagnostics = vec![InferenceDiagnostic::InferenceCycle { owner }];

    Arc::new(InferenceResult {
        self_type: TyAndSrc { ty: self_type, src },
        type_of_expr: ArenaMap::default(),
        type_of_pat: ArenaMap::default(),
        kind_of_ty: ArenaMap::default(),
        instances: FxHashMap::default(),
        methods: FxHashMap::default(),
        diagnostics,
    })
}

#[derive(Debug, PartialEq, Eq)]
pub struct InferenceResult<T, C> {
    pub self_type: TyAndSrc<T>,
    pub type_of_expr: ArenaMap<ExprId, T>,
    pub type_of_pat: ArenaMap<PatId, T>,
    pub kind_of_ty: ArenaMap<LocalTypeRefId, T>,
    pub instances: FxHashMap<ExprId, Vec<T>>,
    pub methods: FxHashMap<(ExprId, usize), SmallVec<[MethodSource; 1]>>,
    pub(crate) diagnostics: Vec<InferenceDiagnostic<T, C>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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
    pub(crate) subst: unify::Substitution,
    pub(crate) constraints: Vec<(CtntInfo, CtntExpected, CtntFound, Option<ClassEnvScope>)>,
    can_generalize: bool,
    class_env: ClassEnv,
    member_records: usize,
}

struct BodyInferenceContext<'a> {
    icx: InferenceContext<'a>,
    body: Arc<Body>,
    ret_type: TyId,
    lambda_type: Vec<TyId>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ExprOrPatId {
    ExprIdInfix(ExprId, usize),
    ExprId(ExprId),
    PatId(PatId),
}

impl From<(ExprId, usize)> for ExprOrPatId {
    fn from((id, i): (ExprId, usize)) -> Self {
        Self::ExprIdInfix(id, i)
    }
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
    pub fn new(db: &'a dyn HirDatabase, resolver: Resolver, owner: TypeVarOwner, can_generalize: bool) -> Self {
        let mut types = Types::default();
        let src = (owner, TypeOrigin::Def(owner.into()));
        let self_type = types.insert(TyInfo::Error, src);

        InferenceContext {
            db,
            owner,
            resolver,
            types,
            result: InferenceResult {
                self_type: TyAndSrc { ty: self_type, src },
                type_of_expr: ArenaMap::default(),
                type_of_pat: ArenaMap::default(),
                kind_of_ty: ArenaMap::default(),
                instances: FxHashMap::default(),
                methods: FxHashMap::default(),
                diagnostics: Vec::new(),
            },
            can_generalize,
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

        if !self.can_generalize {
            self.report_unknowns(self.result.self_type.ty);
        }

        let self_type = self.types.insert(TyInfo::Error, (self.owner, TypeOrigin::Synthetic));
        let mut res = std::mem::replace(&mut self.result, InferenceResult {
            self_type: TyAndSrc {
                ty: self_type,
                src: (self.owner, TypeOrigin::Synthetic),
            },
            type_of_expr: ArenaMap::default(),
            type_of_pat: ArenaMap::default(),
            kind_of_ty: ArenaMap::default(),
            instances: FxHashMap::default(),
            methods: FxHashMap::default(),
            diagnostics: Vec::new(),
        });

        if self.can_generalize {
            res.self_type.ty = self.generalize(res.self_type.ty);
        }

        res.self_type.ty = res.self_type.ty.normalize(&mut self.types);
        self.result.self_type = res.self_type;
        self.result.type_of_pat = res.type_of_pat.clone();
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
        let ty = self.db.lang_item(module.lib, name.into()).expect(name);
        let ty = ty.as_type_ctor().unwrap();

        self.types.insert(TyInfo::Ctor(ty), src)
    }

    pub(crate) fn lang_ctor(&self, name: &'static str) -> TypeCtorId {
        let module = self.owner.module(self.db.upcast());
        let ty = self.db.lang_item(module.lib, name.into()).expect(name);

        ty.as_type_ctor().unwrap()
    }

    pub(crate) fn type_kind(&mut self, src: TySource) -> TyId {
        let type_ctor = self.lang_ctor("type-kind");

        self.types.insert(TyInfo::Ctor(type_ctor), src)
    }

    pub(crate) fn lang_class(&self, name: &'static str) -> ClassId {
        let module = self.owner.module(self.db.upcast());
        let id = self.db.lang_item(module.lib, name.into()).expect(name);

        id.as_class().unwrap()
    }

    pub(crate) fn fn_args(&self, mut ty: TyId, mut max_args: usize) -> (List<TyId>, TyId) {
        let fn_ctor = self.lang_ctor("fn-type");
        let mut args = Vec::new();

        while let Some(a) = ty.match_ctor(&self.types, fn_ctor) {
            if max_args == 0 {
                break;
            }

            args.push(a[0]);
            ty = a[1];
            max_args -= 1;
        }

        (args.into(), ty)
    }

    pub(crate) fn fn_type<I>(&mut self, args: I, ret: TyId, src: TySource) -> TyId
    where
        I: IntoIterator<Item = TyId>,
        I::IntoIter: DoubleEndedIterator,
    {
        let fn_type = self.lang_type("fn-type", src);

        args.into_iter()
            .rev()
            .fold(ret, |ret, arg| self.app_type(fn_type, [arg, ret], src))
    }

    pub(crate) fn app_type(&mut self, base: TyId, args: impl IntoIterator<Item = TyId>, src: TySource) -> TyId {
        self.types.insert(TyInfo::App(base, List::from_iter(args)), src)
    }

    pub(crate) fn report(&mut self, diag: InferenceDiagnostic<TyId, CtntInfo>) {
        self.result.diagnostics.push(diag);
    }

    // #[track_caller]
    pub(crate) fn report_mismatch(&mut self, expected: TyId, found: TyId, id: impl Into<ExprOrPatId>) {
        // log::debug!("mismatch: {}", std::panic::Location::caller());
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

    pub(crate) fn constrain(&mut self, expected: CtntExpected, found: CtntFound, ctnt: CtntInfo) {
        self.constraints.push((ctnt, expected, found, self.class_env.current()));
    }

    pub(crate) fn error(&mut self, src: TySource) -> TyId {
        self.types.insert(TyInfo::Error, src)
    }

    pub(crate) fn unit(&mut self, src: TySource) -> TyId {
        self.lang_type("unit-type", src)
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

        self.type_vars.add_scope(kinds)
    }

    fn with_owner<T>(&mut self, id: TypeVarOwner, f: impl FnOnce(&mut Self) -> T) -> T {
        let owner = std::mem::replace(&mut self.owner, id);
        let res = f(self);

        self.owner = owner;
        res
    }

    pub(crate) fn class_owner(&mut self, id: ClassId) {
        let lower = self.db.lower_class(id);
        let src = self.source(TypeOrigin::Def(id.into()));
        let vars = lower
            .class
            .vars
            .iter()
            .map(|&v| v.to_info(self.db, &mut self.types, &mut self.type_vars, src))
            .collect();

        self.type_vars.add_scope(vars);
    }

    pub(crate) fn member_owner(&mut self, id: MemberId) {
        let lower = self.db.lower_member(id);
        let src = self.source(TypeOrigin::Def(id.into()));
        let vars = lower
            .member
            .vars
            .iter()
            .map(|&v| v.to_info(self.db, &mut self.types, &mut self.type_vars, src))
            .collect();

        self.type_vars.add_scope(vars);
    }

    pub(crate) fn class_item(&mut self, class: ClassId, ann: TyId) -> TyId {
        let src = self.source(TypeOrigin::Def(class.into()));
        let lower = self.db.lower_class(class);
        let scope = self.type_vars.top_scope();
        let types = (0..lower.class.vars.len() as u32)
            .map(|i| self.types.insert(TyInfo::TypeVar(TypeVar::new(i, scope)), src))
            .collect::<List<_>>();

        let kinds = lower
            .class
            .vars
            .iter()
            .map(|t| t.to_info(self.db, &mut self.types, &mut self.type_vars, src))
            .collect::<List<_>>();

        let where_clause = WhereClause {
            constraints: [CtntInfo { class, types }].into(),
        };

        let item_ty = self.types.insert(TyInfo::Where(where_clause, ann), src);

        self.types.insert(TyInfo::ForAll(kinds, item_ty, scope), src)
    }

    pub(crate) fn member_item(&mut self, member: MemberId, ann: TyId, item: &Name, body: &Body) -> TyId {
        self.can_generalize = false;

        let lower = self.db.lower_member(member);

        if lower.member.class == ClassId::dummy() {
            return ann;
        }

        let src = self.source(TypeOrigin::Def(member.into()));
        let data = self.db.member_data(member);
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
            .zip(data.types.iter())
            .map(|(t, &ty)| {
                let src = self.source(ty);

                t.to_info(self.db, &mut self.types, &mut self.type_vars, src)
            })
            .collect::<Vec<_>>();

        let kinds = lower
            .member
            .vars
            .iter()
            .map(|t| t.to_info(self.db, &mut self.types, &mut self.type_vars, src))
            .collect::<List<_>>();

        let item_ty = item_ty
            .ty
            .to_info(self.db, &mut self.types, &mut self.type_vars, item_ty.src);

        let mut item_ty = match self.types[item_ty].clone() {
            | TyInfo::ForAll(_, inner, scope) => inner.replace_vars(&mut self.types, &types, scope),
            | _ => item_ty,
        };

        if let TyInfo::Where(_, inner) = self.types[item_ty] {
            item_ty = inner;
        }

        if !lower.member.where_clause.constraints.is_empty() {
            let where_clause =
                lower
                    .member
                    .where_clause
                    .clone()
                    .to_info(self.db, &mut self.types, &mut self.type_vars, src);

            item_ty = self.types.insert(TyInfo::Where(where_clause, item_ty), src);
        }

        if !kinds.is_empty() {
            let scope = self.type_vars.top_scope();

            item_ty = self.types.insert(TyInfo::ForAll(kinds, item_ty, scope), src)
        }

        let item_ty = item_ty.normalize(&mut self.types);

        if !self.unify_types(item_ty, ann) {
            self.report_mismatch(item_ty, ann, body.body_expr());
        }

        item_ty
    }

    fn report_unknowns(&mut self, ty: TyId) {
        let ty = self.subst_type(ty);
        let mut unknowns = FxHashMap::default();
        let mut find_unknowns = |ty: TyId| match self.types[ty] {
            | TyInfo::Unknown(u) if !unknowns.contains_key(&u) => {
                unknowns.insert(u, self.types.source(ty));
            },
            | _ => {},
        };

        ty.everything(&self.types, &mut find_unknowns);

        for (_, src) in unknowns {
            self.report(InferenceDiagnostic::UninferredType { src });
        }

        let constraints = std::mem::replace(&mut self.constraints, Vec::new());

        for (ctnt, expected, found, _) in constraints {
            self.report(InferenceDiagnostic::UnsolvedConstraint { expected, found, ctnt });
        }
    }
}

impl<'a> BodyInferenceContext<'a> {
    fn new(db: &'a dyn HirDatabase, resolver: Resolver, owner: DefWithBodyId, can_generalize: bool) -> Self {
        let mut icx = InferenceContext::new(db, resolver, owner.into(), can_generalize);
        let src = icx.source(TypeOrigin::Synthetic);
        let error = icx.types.insert(TyInfo::Error, src);

        BodyInferenceContext {
            icx,
            body: db.body(owner),
            ret_type: error,
            lambda_type: Vec::new(),
        }
    }

    fn finish(self) -> InferenceResult<Ty, Constraint> {
        self.icx.finish()
    }

    fn check_body(&mut self, ann: TyId, is_func: bool) {
        match self.types[ann].clone() {
            | TyInfo::ForAll(_kinds, inner, scope) => {
                self.type_vars.push_scope(scope);
                // let sk = self.skolemize(&kinds, inner, scope);
                self.check_body(inner, is_func);
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
        let (a, mut ret) = self.fn_args(ann, body.params().len());

        if a.is_empty() {
            let src = self.source(body.body_expr());
            ret = self.fresh_type(src);
        }

        let args = body.params().iter().map(|&pat| self.infer_pat(pat)).collect::<Vec<_>>();

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
        self.check_expr(body.body_expr(), ret);
    }

    pub(crate) fn with_expr_scope<T>(&mut self, expr: ExprId, f: impl FnOnce(&mut Self) -> T) -> T {
        if let TypeVarOwner::DefWithBodyId(def) = self.owner {
            let new_resolver = Resolver::for_expr(self.db.upcast(), def, expr);
            let old_resolver = std::mem::replace(&mut self.resolver, new_resolver);
            let res = f(self);

            self.resolver = old_resolver;
            res
        } else {
            unreachable!()
        }
    }
}

impl<'a> ProcessInfix for InferenceContext<'a> {
    type It = TyId;
    type Src = TySource;

    fn db(&self) -> &dyn hir_def::db::DefDatabase {
        self.db.upcast()
    }

    fn resolver(&self) -> &Resolver {
        &self.resolver
    }

    fn error(&mut self, src: Self::Src) -> Self::It {
        Self::error(self, src)
    }

    fn on_unresolved(&mut self, src: Self::Src, idx: usize, _: &Path) {
        self.report(InferenceDiagnostic::UnresolvedOperator {
            src: match src.1 {
                | TypeOrigin::ExprIdInfix(id, idx) => OperatorSource::ExprOrPat((id, idx).into()),
                | TypeOrigin::ExprId(id) => OperatorSource::ExprOrPat(id.into()),
                | TypeOrigin::PatId(id) => OperatorSource::ExprOrPat(id.into()),
                | TypeOrigin::TypeRefId(id) => OperatorSource::TypeRef(self.owner, id.into()),
                | _ => unreachable!(),
            },
            idx,
        });
    }

    fn on_private(&mut self, src: Self::Src, idx: usize, _: &Path, _: FixityId) {
        self.report(InferenceDiagnostic::PrivateOperator {
            src: match src.1 {
                | TypeOrigin::ExprIdInfix(id, idx) => OperatorSource::ExprOrPat((id, idx).into()),
                | TypeOrigin::ExprId(id) => OperatorSource::ExprOrPat(id.into()),
                | TypeOrigin::PatId(id) => OperatorSource::ExprOrPat(id.into()),
                | TypeOrigin::TypeRefId(id) => OperatorSource::TypeRef(self.owner, id.into()),
                | _ => unreachable!(),
            },
            idx,
        });
    }
}

impl<'a> ProcessInfix for BodyInferenceContext<'a> {
    type It = TyId;
    type Src = TySource;

    fn db(&self) -> &dyn hir_def::db::DefDatabase {
        self.db.upcast()
    }

    fn resolver(&self) -> &Resolver {
        &self.resolver
    }

    fn error(&mut self, src: Self::Src) -> Self::It {
        InferenceContext::error(self, src)
    }

    fn on_unresolved(&mut self, src: Self::Src, idx: usize, path: &Path) {
        InferenceContext::on_unresolved(self, src, idx, path);
    }

    fn on_private(&mut self, src: Self::Src, idx: usize, path: &Path, id: FixityId) {
        InferenceContext::on_private(self, src, idx, path, id);
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
    use std::sync::Arc;

    use hir_def::diagnostic::DiagnosticSink;
    use hir_def::expr::ExprId;
    use hir_def::id::{ClassId, HasSource, Lookup, MemberId, TypeVarOwner};
    use hir_def::in_file::InFile;
    use hir_def::resolver::ValueNs;
    use hir_def::type_ref::{LocalTypeRefId, TypeVarSource};
    use syntax::{ast, AstNode, AstPtr, SyntaxNodePtr};

    use super::{ExprOrPatId, InferenceContext};
    use crate::db::HirDatabase;
    use crate::diagnostics::*;
    use crate::info::{CtntInfo, TyId, TySource, TypeOrigin};
    use crate::search::TypeSearchResult;
    use crate::ty::{Constraint, Ty};

    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub enum OperatorSource {
        ExprOrPat(ExprOrPatId),
        TypeRef(TypeVarOwner, LocalTypeRefId),
    }

    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub enum CtntExpected {
        ExprOrPat(ExprOrPatId),
        Where(WhereSource, usize),
    }

    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub enum CtntFound {
        ExprOrPat(ExprOrPatId),
        Member(MemberId),
    }

    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub enum WhereSource {
        TypeRef(TypeVarOwner, LocalTypeRefId),
        Class(ClassId),
        Member(MemberId),
    }

    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub enum ClassSource {
        WhereClause(WhereSource, usize),
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
            src: OperatorSource,
            idx: usize,
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
            src: OperatorSource,
            idx: usize,
        },
        UninferredType {
            src: TySource,
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
            expected: CtntExpected,
            found: CtntFound,
            ctnt: C,
        },
        RecursiveTypeAlias {
            src: TySource,
        },
        InferenceCycle {
            owner: TypeVarOwner,
        },
        ValueHole {
            id: ExprId,
            ty: T,
            search: Arc<TypeSearchResult<ValueNs>>,
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
                | InferenceDiagnostic::UnsolvedConstraint { expected, found, ctnt } => {
                    InferenceDiagnostic::UnsolvedConstraint {
                        expected,
                        found,
                        ctnt: icx.subst_ctnt(&ctnt),
                    }
                },
                | InferenceDiagnostic::ValueHole { id, ty, .. } => {
                    let ty = icx.subst_type(ty);
                    let search = icx.search_value(ty, id);

                    InferenceDiagnostic::ValueHole { id, ty, search }
                },
                | _ => self,
            }
        }
    }

    impl InferenceDiagnostic<Ty, Constraint> {
        pub fn add_to(&self, db: &dyn HirDatabase, owner: TypeVarOwner, sink: &mut DiagnosticSink) {
            let file = owner.source(db.upcast()).file_id;
            let expr_or_pat = |owner, id| match owner {
                | TypeVarOwner::DefWithBodyId(owner) => {
                    let source_map = db.body_source_map(owner).1;

                    match id {
                        | ExprOrPatId::ExprIdInfix(id, _) | ExprOrPatId::ExprId(id) => match source_map.expr_syntax(id)
                        {
                            | Ok(e) => e.map(|v| v.syntax_node_ptr()),
                            | Err(e) => e.0.source(db.upcast()).map(|v| SyntaxNodePtr::new(v.syntax())),
                        },
                        | ExprOrPatId::PatId(id) => match source_map.pat_syntax(id) {
                            | Ok(e) => e.map(|v| v.syntax_node_ptr()),
                            | Err(e) => e.0.source(db.upcast()).map(|v| SyntaxNodePtr::new(v.syntax())),
                        },
                    }
                },
                | _ => unreachable!(),
            };

            let where_src = |src| match src {
                | WhereSource::TypeRef(owner, tr) => owner.with_type_source_map(db.upcast(), |source_map| {
                    let root = owner.source(db.upcast()).file_syntax(db.upcast());

                    source_map
                        .type_ref_syntax(tr)
                        .and_then(AstPtr::cast::<ast::TypeWhere>)
                        .and_then(|a| a.to_node(&root).where_clause())
                        .map(|a| InFile::new(file, a))
                }),
                | WhereSource::Class(id) => id
                    .lookup(db.upcast())
                    .source(db.upcast())
                    .map(|c| c.where_clause())
                    .transpose(),
                | WhereSource::Member(id) => id
                    .lookup(db.upcast())
                    .source(db.upcast())
                    .map(|c| c.where_clause())
                    .transpose(),
            };

            let ty_src = |src: TySource| match src.1 {
                | TypeOrigin::ExprIdInfix(id, _) | TypeOrigin::ExprId(id) => match src.0 {
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
                        | TypeVarSource::Name(s) => file.with_value(s.syntax_node_ptr()),
                        | TypeVarSource::NameRef(s) => file.with_value(s.syntax_node_ptr()),
                    })
                }),
                | TypeOrigin::Def(id) => Some(id.source(db.upcast()).map(|v| SyntaxNodePtr::new(v.syntax()))),
                | TypeOrigin::Synthetic => None,
            };

            match self {
                | InferenceDiagnostic::UnresolvedValue { id } => {
                    let src = expr_or_pat(owner, *id).value;

                    sink.push(UnresolvedValue { file, src });
                },
                | InferenceDiagnostic::UnresolvedType { owner, id } => {
                    owner.with_type_source_map(db.upcast(), |source_map| {
                        let src = source_map.type_ref_syntax(*id).unwrap();

                        sink.push(UnresolvedType { file, ty: src });
                    });
                },
                | InferenceDiagnostic::UnresolvedClass { src } => {
                    let src = match *src {
                        | ClassSource::WhereClause(w, i) => where_src(w)
                            .and_then(|w| w.map(|w| w.constraints().nth(i).and_then(|c| c.class())).transpose()),
                        | ClassSource::Member(id) => id
                            .lookup(db.upcast())
                            .source(db.upcast())
                            .map(|m| m.class())
                            .transpose(),
                    };

                    let src = src.map(|s| s.map(|s| AstPtr::new(&s))).unwrap();

                    sink.push(UnresolvedClass {
                        file: src.file_id,
                        src: src.value,
                    });
                },
                | InferenceDiagnostic::UnresolvedOperator { src, idx } => {
                    let src = match *src {
                        | OperatorSource::ExprOrPat(id) => {
                            let source_map = match owner {
                                | TypeVarOwner::DefWithBodyId(id) => db.body_source_map(id).1,
                                | _ => return,
                            };

                            match id {
                                | ExprOrPatId::ExprIdInfix(id, _) | ExprOrPatId::ExprId(id) => {
                                    source_map.expr_syntax(id).unwrap().value.syntax_node_ptr()
                                },
                                | ExprOrPatId::PatId(id) => source_map.pat_syntax(id).unwrap().value.syntax_node_ptr(),
                            }
                        },
                        | OperatorSource::TypeRef(owner, id) => owner.with_type_source_map(db.upcast(), |source_map| {
                            source_map.type_ref_syntax(id).unwrap().syntax_node_ptr()
                        }),
                    };

                    sink.push(UnresolvedOperator { file, src, idx: *idx });
                },
                | InferenceDiagnostic::PrivateValue { id } => {
                    let src = expr_or_pat(owner, *id).value;

                    sink.push(PrivateValue { file, src });
                },
                | InferenceDiagnostic::PrivateType { owner, id } => {
                    owner.with_type_source_map(db.upcast(), |source_map| {
                        let src = source_map.type_ref_syntax(*id).unwrap();

                        sink.push(PrivateType { file, src });
                    });
                },
                | InferenceDiagnostic::PrivateClass { src } => {
                    let src = match *src {
                        | ClassSource::WhereClause(w, i) => where_src(w)
                            .and_then(|w| w.map(|w| w.constraints().nth(i).and_then(|c| c.class())).transpose()),
                        | ClassSource::Member(id) => id
                            .lookup(db.upcast())
                            .source(db.upcast())
                            .map(|m| m.class())
                            .transpose(),
                    };

                    let src = src.map(|s| s.map(|s| AstPtr::new(&s))).unwrap();

                    sink.push(PrivateClass {
                        file: src.file_id,
                        src: src.value,
                    });
                },
                | InferenceDiagnostic::PrivateOperator { src, idx } => {
                    let src = match *src {
                        | OperatorSource::ExprOrPat(id) => {
                            let source_map = match owner {
                                | TypeVarOwner::DefWithBodyId(id) => db.body_source_map(id).1,
                                | _ => return,
                            };

                            match id {
                                | ExprOrPatId::ExprIdInfix(id, _) | ExprOrPatId::ExprId(id) => {
                                    source_map.expr_syntax(id).unwrap().value.syntax_node_ptr()
                                },
                                | ExprOrPatId::PatId(id) => source_map.pat_syntax(id).unwrap().value.syntax_node_ptr(),
                            }
                        },
                        | OperatorSource::TypeRef(owner, id) => owner.with_type_source_map(db.upcast(), |source_map| {
                            source_map.type_ref_syntax(id).unwrap().syntax_node_ptr()
                        }),
                    };

                    sink.push(PrivateOperator { file, src, idx: *idx });
                },
                | InferenceDiagnostic::UninferredType { src } => {
                    let src = ty_src(*src).unwrap();

                    sink.push(UninferredType { src });
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
                    let src = expr_or_pat(owner, *id).value;
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
                | InferenceDiagnostic::UnsolvedConstraint { expected, found, ctnt } => {
                    let expected = match *expected {
                        | CtntExpected::ExprOrPat(id) => Some(expr_or_pat(owner, id)),
                        | CtntExpected::Where(w, i) => where_src(w)
                            .and_then(|w| w.map(|w| w.constraints().nth(i)).transpose())
                            .map(|c| c.map(|c| SyntaxNodePtr::new(c.syntax()))),
                    };

                    let src = match *found {
                        | CtntFound::ExprOrPat(id) => expr_or_pat(owner, id),
                        | CtntFound::Member(id) => id
                            .lookup(db.upcast())
                            .source(db.upcast())
                            .map(|i| SyntaxNodePtr::new(i.class().unwrap().syntax())),
                    };

                    sink.push(UnsolvedConstraint {
                        file: src.file_id,
                        src: src.value,
                        expected,
                        ctnt: ctnt.clone(),
                    });
                },
                | InferenceDiagnostic::RecursiveTypeAlias { src } => {
                    let src = ty_src(*src).unwrap();

                    sink.push(RecursiveTypeAlias {
                        file: src.file_id,
                        src: src.value,
                    });
                },
                | InferenceDiagnostic::InferenceCycle { owner } => {
                    let src = owner.source(db.upcast());

                    sink.push(InferenceCycle {
                        file: src.file_id,
                        src: SyntaxNodePtr::new(src.value.syntax()),
                    });
                },
                | InferenceDiagnostic::ValueHole { id, ty, search } => {
                    let owner = match owner {
                        | TypeVarOwner::DefWithBodyId(id) => id,
                        | _ => return,
                    };

                    let source_map = db.body_source_map(owner).1;
                    let src = source_map.expr_syntax(*id).unwrap().map(|ptr| ptr.syntax_node_ptr());

                    sink.push(ValueHole {
                        file: src.file_id,
                        src: src.value,
                        ty: *ty,
                        owner,
                        search: search.clone(),
                    });
                },
            }
        }
    }
}
