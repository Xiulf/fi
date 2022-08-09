use std::sync::Arc;

use arena::ArenaMap;
use hir_def::diagnostic::DiagnosticSink;
use hir_def::id::*;
use hir_def::path::Path;
use hir_def::resolver::{HasResolver, Resolver, TypeNs};
use hir_def::type_ref::{LocalTypeRefId, TypeMap, TypeRef};
use rustc_hash::FxHashMap;

use crate::class::{Class, FunDep, Member};
use crate::db::HirDatabase;
use crate::infer::diagnostics::{ClassSource, CtntExpected, CtntFound, InferenceDiagnostic, WhereSource};
use crate::infer::InferenceContext;
use crate::info::{CtntInfo, FieldInfo, FromInfo, ToInfo, TyId, TyInfo, TySource, TypeOrigin};
use crate::ty::{Constraint, List, Ty, TyAndSrc, TypeVar, WhereClause};

pub struct LowerCtx<'a, 'b> {
    type_map: &'a TypeMap,
    icx: &'a mut InferenceContext<'b>,
    types: ArenaMap<LocalTypeRefId, TyId>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct LowerResult<T> {
    pub ty: TyAndSrc<T>,
    pub types: ArenaMap<LocalTypeRefId, T>,
    pub diagnostics: Vec<InferenceDiagnostic<Ty, Constraint>>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct ClassLowerResult<T, C> {
    pub class: Class<T, C>,
    pub diagnostics: Vec<InferenceDiagnostic<Ty, Constraint>>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct MemberLowerResult<T, C> {
    pub member: Member<T, C>,
    pub diagnostics: Vec<InferenceDiagnostic<Ty, Constraint>>,
}

impl<'a, 'b> LowerCtx<'a, 'b> {
    pub fn new(type_map: &'a TypeMap, icx: &'a mut InferenceContext<'b>) -> Self {
        Self {
            type_map,
            icx,
            types: ArenaMap::default(),
        }
    }

    pub fn finish(self, ty: TyId) -> Arc<LowerResult<Ty>> {
        let icx_res = self.icx.finish_mut();
        let res = LowerResult {
            ty: TyAndSrc {
                ty,
                src: self.icx.types.source(ty),
            },
            types: self.types,
            diagnostics: icx_res.diagnostics,
        };

        Arc::new(LowerResult::from_info(self.icx.db, &self.icx.types, res))
    }

    fn finish_class(self, class: Class<TyId, CtntInfo>) -> Arc<ClassLowerResult<Ty, Constraint>> {
        let icx_res = self.icx.finish_mut();
        let res = ClassLowerResult {
            class,
            diagnostics: icx_res.diagnostics,
        };

        Arc::new(ClassLowerResult::from_info(self.db, &self.icx.types, res))
    }

    fn finish_member(self, instance: Member<TyId, CtntInfo>) -> Arc<MemberLowerResult<Ty, Constraint>> {
        let icx_res = self.icx.finish_mut();
        let res = MemberLowerResult {
            member: instance,
            diagnostics: icx_res.diagnostics,
        };

        Arc::new(MemberLowerResult::from_info(self.db, &self.icx.types, res))
    }

    pub fn for_assoc_item<T>(
        &mut self,
        owner: TypeVarOwner,
        type_map: &TypeMap,
        f: impl FnOnce(&mut LowerCtx) -> T,
    ) -> T {
        let resolver = owner.resolver(self.db.upcast());
        let resolver = std::mem::replace(&mut self.resolver, resolver);
        let type_map = unsafe { &*(type_map as *const TypeMap) };
        let type_map = std::mem::replace(&mut self.type_map, type_map);
        let owner = std::mem::replace(&mut self.owner, owner);
        let res = f(self);

        self.resolver = resolver;
        self.type_map = type_map;
        self.owner = owner;
        res
    }

    pub fn lower_ty(&mut self, ty: LocalTypeRefId) -> TyId {
        let src = self.source(ty);
        let lowered = match &self.type_map[ty] {
            | TypeRef::Error => self.error(src),
            | TypeRef::Placeholder => self.fresh_type_without_kind(src),
            | TypeRef::Figure(i) => self.icx.types.insert(TyInfo::Figure(*i), src),
            | TypeRef::Symbol(s) => self.icx.types.insert(TyInfo::Symbol(s.clone().into()), src),
            | TypeRef::Path(path) => self.lower_path(&path, ty),
            | TypeRef::Unit => self.unit(src),
            | &TypeRef::App(mut base, arg) => {
                let mut args = vec![self.lower_ty(arg)];

                while let TypeRef::App(b, arg) = self.type_map[base] {
                    base = b;
                    args.push(self.lower_ty(arg));
                }

                args.reverse();

                let base = self.lower_ty(base);
                let src = self.source(ty);

                self.check_kind_for_app(base, &args, src);
                self.app_type(base, args, src).normalize(&mut self.icx.types)
            },
            | TypeRef::Infix(types, ops) => {
                let types = types.iter().map(|&t| self.lower_ty(t)).collect::<Vec<_>>();

                self.process_infix(
                    types.into_iter(),
                    ops,
                    src,
                    |ctx, _, op, lhs, rhs| {
                        ctx.check_kind_for_app(op, &[lhs, rhs], src);
                        ctx.app_type(op, [lhs, rhs], src)
                    },
                    |ctx, _, path, resolver| ctx.lower_path_resolver(path, resolver, ty),
                )
            },
            | TypeRef::Record(fields, tail) => {
                let row = self.lower_row(fields, *tail, ty);
                let record_ty = self.lang_type("record-type", src);
                let type_kind = self.type_kind(src);
                let row_kind = self.lang_type("row-kind", src);
                let row_kind = self.icx.types.insert(TyInfo::App(row_kind, [type_kind].into()), src);

                self.check_kind(row, row_kind);
                self.icx.types.insert(TyInfo::App(record_ty, [row].into()), src)
            },
            | TypeRef::Row(fields, tail) => self.lower_row(fields, *tail, ty),
            | TypeRef::Forall(vars, inner) => {
                let scope = self.push_type_vars(vars);
                let kinds = self.type_vars.var_kinds(scope).clone();
                let resolver = Resolver::for_type_ref(self.db.upcast(), self.owner, *inner);
                let resolver = std::mem::replace(&mut self.icx.resolver, resolver);
                let inner = self.lower_ty(*inner);

                self.icx.resolver = resolver;
                self.type_vars.pop_scope();
                self.icx.types.insert(TyInfo::ForAll(kinds, inner, scope), src)
            },
            | TypeRef::Where(where_clause, inner) => {
                let where_ = self.lower_where_clause(where_clause, WhereSource::TypeRef(ty));
                let inner = self.lower_ty(*inner);

                self.icx.types.insert(TyInfo::Where(where_, inner), src)
            },
        };

        self.types.insert(ty, lowered);
        lowered
    }

    pub(crate) fn lower_where_clause(
        &mut self,
        where_clause: &hir_def::type_ref::WhereClause,
        src: WhereSource,
    ) -> WhereClause<CtntInfo> {
        for tv_kind in where_clause.type_var_kinds.iter() {
            let var = self.lower_path(&tv_kind.type_var, tv_kind.kind);
            let kind = self.lower_ty(tv_kind.kind);

            self.check_kind(var, kind);
        }

        let constraints = where_clause
            .constraints
            .iter()
            .enumerate()
            .filter_map(|(i, c)| self.lower_constraint(c, ClassSource::WhereClause(src, i)))
            .collect();

        WhereClause { constraints }
    }

    pub(crate) fn lower_row(
        &mut self,
        fields: &[hir_def::type_ref::Field],
        tail: Option<LocalTypeRefId>,
        type_ref: LocalTypeRefId,
    ) -> TyId {
        let src = self.source(type_ref);
        let elem_kind = self.fresh_type(src);
        let fields = fields
            .iter()
            .map(|f| {
                let ty = self.lower_ty(f.ty);

                self.check_kind(ty, elem_kind);

                FieldInfo {
                    name: f.name.clone(),
                    ty,
                }
            })
            .collect();

        let row_kind = self.lang_type("row-kind", src);
        let row_kind = self.icx.types.insert(TyInfo::App(row_kind, [elem_kind].into()), src);
        let tail = tail.map(|t| {
            let ty = self.lower_ty(t);

            self.check_kind(ty, row_kind);
            ty
        });

        self.icx
            .types
            .insert(TyInfo::Row(fields, tail), src)
            .normalize(&mut self.icx.types)
    }
}

impl InferenceContext<'_> {
    fn lower_path(&mut self, path: &Path, type_ref: LocalTypeRefId) -> TyId {
        self.lower_path_resolver(path, self.resolver.clone(), type_ref)
    }

    fn lower_path_resolver(&mut self, path: &Path, resolver: Resolver, type_ref: LocalTypeRefId) -> TyId {
        let src = self.source(type_ref);
        let owner = self.owner;
        let (resolution, vis, remaining) = match resolver.resolve_type(self.db.upcast(), path) {
            | Some(it) => it,
            | None => {
                self.report(InferenceDiagnostic::UnresolvedType { owner, id: type_ref });
                return self.types.insert(TyInfo::Error, src);
            },
        };

        if path.segments().len() > 1 && !vis.is_visible_from(self.db.upcast(), self.resolver.module().unwrap()) {
            self.report(InferenceDiagnostic::PrivateType { owner, id: type_ref });
        }

        assert_eq!(remaining, None);

        self.lower_resolved_path(resolution, type_ref)
    }

    fn lower_resolved_path(&mut self, resolution: TypeNs, type_ref: LocalTypeRefId) -> TyId {
        let src = self.source(type_ref);

        match resolution {
            | TypeNs::Fixity(_) => {
                // @TODO: report error
                self.types.insert(TyInfo::Error, src)
            },
            | TypeNs::Class(_) => {
                // @TODO: report error
                self.types.insert(TyInfo::Error, src)
            },
            | TypeNs::TypeVar(id) => {
                let (idx, depth) = self.resolver.type_var_index(id).unwrap();
                let scope = self.type_vars.scope_at(depth);
                let type_var = TypeVar::new(idx as u32, scope);

                self.types.insert(TyInfo::TypeVar(type_var), src)
            },
            | TypeNs::TypeAlias(id) if TypeVarOwner::TypedDefId(id.into()) == self.owner => {
                self.report(InferenceDiagnostic::RecursiveTypeAlias { src });
                self.error(src)
            },
            | TypeNs::TypeAlias(id) => self.types.insert(TyInfo::Alias(id), src),
            | TypeNs::TypeCtor(id) => self.types.insert(TyInfo::Ctor(id), src),
        }
    }
}

impl LowerCtx<'_, '_> {
    pub(crate) fn lower_constraint(
        &mut self,
        ctnt: &hir_def::type_ref::Constraint,
        src: ClassSource,
    ) -> Option<CtntInfo> {
        let class = self.lower_class_path(&ctnt.class, src)?;
        let class_info = self.db.lower_class(class);
        let types = ctnt
            .types
            .iter()
            .zip(class_info.class.vars.iter())
            .map(|(&t, &kind)| {
                let ty = self.lower_ty(t);
                let src = self.icx.types.source(ty);
                let kind = kind.to_info(self.db, &mut self.icx.types, &mut self.icx.type_vars, src);

                self.check_kind(ty, kind);
                ty
            })
            .collect();

        Some(CtntInfo { class, types })
    }

    fn lower_class_path(&mut self, path: &Path, src: ClassSource) -> Option<ClassId> {
        let (resolution, vis, _) = match self.resolver.resolve_type(self.db.upcast(), path) {
            | Some(it) => it,
            | None => {
                self.report(InferenceDiagnostic::UnresolvedClass { src });
                return None;
            },
        };

        if path.segments().len() > 1 && !vis.is_visible_from(self.db.upcast(), self.resolver.module().unwrap()) {
            self.report(InferenceDiagnostic::PrivateClass { src });
            return None;
        }

        match resolution {
            | TypeNs::Class(id) => Some(id),
            | _ => {
                todo!("report error: not a class");
            },
        }
    }
}

impl<'a, 'b> std::ops::Deref for LowerCtx<'a, 'b> {
    type Target = InferenceContext<'b>;

    fn deref(&self) -> &Self::Target {
        &self.icx
    }
}

impl<'a, 'b> std::ops::DerefMut for LowerCtx<'a, 'b> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.icx
    }
}

pub(crate) fn value_ty(db: &dyn HirDatabase, id: ValueTyDefId) -> TyAndSrc<Ty> {
    match id {
        | ValueTyDefId::FuncId(id) => db.infer(id.into()).self_type,
        | ValueTyDefId::StaticId(id) => db.infer(id.into()).self_type,
        | ValueTyDefId::ConstId(id) => db.infer(id.into()).self_type,
        | ValueTyDefId::CtorId(id) => db.ctor_ty(id).ty,
    }
}

pub(crate) fn ctor_ty(db: &dyn HirDatabase, id: CtorId) -> Arc<LowerResult<Ty>> {
    let ty_data = db.type_ctor_data(id.parent);
    let ctor_data = &ty_data.ctors[id.local_id];
    let resolver = id.resolver(db.upcast());
    let mut icx = InferenceContext::new(db, resolver, TypeVarOwner::TypedDefId(id.into()), false);
    let mut ctx = LowerCtx::new(ty_data.type_map(), &mut icx);
    let src = ctx.source(TypeOrigin::Def(id.into()));
    let mut ret = ctx.icx.types.insert(TyInfo::Ctor(id.parent), src);
    let kind = db
        .kind_for_ctor(id.parent)
        .ty
        .to_info(db, &mut ctx.icx.types, &mut ctx.icx.type_vars, src)
        .ty;

    let vars = {
        let (args, _) = ctx.fn_args(kind, usize::MAX);

        if args.len() > 0 {
            let scope = ctx.type_vars.add_scope(args.clone());
            Some((args, scope))
        } else {
            None
        }
    };

    if let Some((vars, scope)) = &vars {
        let args = (0..vars.len())
            .map(|i| {
                ctx.icx
                    .types
                    .insert(TyInfo::TypeVar(TypeVar::new(i as u32, *scope)), src)
            })
            .collect::<Vec<_>>();

        ret = ctx.icx.app_type(ret, args, src);
    }

    let args = ctor_data
        .types
        .iter()
        .map(|&ty| {
            let ty_ = ctx.lower_ty(ty);

            ctx.check_kind_type(ty_);
            ty_
        })
        .collect::<Vec<_>>();

    let ty = if !args.is_empty() {
        ctx.fn_type(args, ret, src)
    } else {
        ret
    };

    let ty = if let Some((vars, scope)) = vars {
        ctx.icx.types.insert(TyInfo::ForAll(vars, ty, scope), src)
    } else {
        ty
    };

    ctx.finish(ty)
}

pub(crate) fn type_for_alias(db: &dyn HirDatabase, id: TypeAliasId) -> Arc<LowerResult<Ty>> {
    let data = db.type_alias_data(id);
    let resolver = id.resolver(db.upcast());
    let mut icx = InferenceContext::new(db, resolver, TypeVarOwner::TypedDefId(id.into()), false);
    let mut ctx = LowerCtx::new(data.type_map(), &mut icx);
    let vars = data
        .type_vars
        .iter()
        .map(|&var| {
            let src = ctx.source(var);
            ctx.fresh_type(src)
        })
        .collect::<Vec<_>>();

    let scope = ctx.type_vars.add_scope(vars.clone().into());
    let ty = ctx.lower_ty(data.alias);
    let src = ctx.source(TypeOrigin::Def(id.into()));
    let kinds = var_kinds(&mut ctx, vars, src);
    let ty = if !kinds.is_empty() {
        ctx.icx.types.insert(TyInfo::ForAll(kinds, ty, scope), src)
    } else {
        ty
    };

    let ty = ctx.subst_type(ty);

    ctx.finish(ty)
}

pub(crate) fn type_for_alias_recover(
    _db: &dyn HirDatabase,
    _cycle: &[String],
    _id: &TypeAliasId,
) -> Arc<LowerResult<Ty>> {
    unimplemented!();
}

pub(crate) fn kind_for_ctor(db: &dyn HirDatabase, id: TypeCtorId) -> Arc<LowerResult<Ty>> {
    let data = db.type_ctor_data(id);
    let resolver = id.resolver(db.upcast());
    let mut icx = InferenceContext::new(db, resolver, TypeVarOwner::TypedDefId(id.into()), false);
    let mut ctx = LowerCtx::new(data.type_map(), &mut icx);

    if let Some(kind) = data.kind {
        let ty = ctx.lower_ty(kind);

        ctx.finish(ty)
    } else {
        let var_kinds = data
            .type_vars
            .iter()
            .map(|&var| {
                let src = ctx.source(var);
                ctx.fresh_type(src)
            })
            .collect::<Vec<_>>();

        let _scope = ctx.type_vars.add_scope(var_kinds.clone().into());
        let src = ctx.source(TypeOrigin::Def(id.into()));
        let ty_kind = if var_kinds.is_empty() {
            ctx.type_kind(src)
        } else {
            let ty_kind = ctx.type_kind(src);

            ctx.icx.fn_type(var_kinds.clone(), ty_kind, src)
        };

        ctx.icx.result.self_type = TyAndSrc { ty: ty_kind, src };

        for (_, ctor) in data.ctors.iter() {
            for &ty in ctor.types.iter() {
                let ty_ = ctx.lower_ty(ty);

                ctx.check_kind_type(ty_);
            }
        }

        let type_kind = ctx.type_kind(src);

        for kind in var_kinds {
            let kind = ctx.subst_type(kind);

            if let TyInfo::Unknown(u) = ctx.icx.types[kind] {
                ctx.solve_type(u, type_kind);
            }
        }

        ctx.type_vars.pop_scope();

        let ty = ctx.subst_type(ty_kind);

        ctx.finish(ty)
    }
}

pub(crate) fn kind_for_ctor_recover(db: &dyn HirDatabase, _cycle: &[String], id: &TypeCtorId) -> Arc<LowerResult<Ty>> {
    let data = db.type_ctor_data(*id);
    unimplemented!("{}", data.name);
}

pub(crate) fn lower_class_query(db: &dyn HirDatabase, id: ClassId) -> Arc<ClassLowerResult<Ty, Constraint>> {
    let data = db.class_data(id);
    let type_map = data.type_map();
    let resolver = id.resolver(db.upcast());
    let mut icx = InferenceContext::new(db, resolver, TypeVarOwner::TypedDefId(id.into()), false);
    let mut ctx = LowerCtx::new(data.type_map(), &mut icx);
    let var_kinds_ = data
        .type_vars
        .iter()
        .map(|&var| {
            let src = ctx.source(var);
            ctx.fresh_type(src)
        })
        .collect::<List<_>>();

    let scope = ctx.type_vars.add_scope(var_kinds_.clone());
    let vars = data
        .type_vars
        .iter()
        .enumerate()
        .map(|(i, &var)| {
            let type_var = TypeVar::new(i as u32, scope);

            (&type_map[var].name, type_var)
        })
        .collect::<FxHashMap<_, _>>();

    let fundeps = data
        .fundeps
        .iter()
        .map(|dep| FunDep {
            determiners: dep.determiners.iter().map(|n| vars[n]).collect(),
            determined: dep.determined.iter().map(|n| vars[n]).collect(),
        })
        .collect();

    let where_clause = ctx.lower_where_clause(&data.where_clause, WhereSource::Class(id));
    let diag_count = ctx.result.diagnostics.len();

    for &(_, id) in data.items.iter() {
        let ty = match id {
            | AssocItemId::FuncId(id) => {
                let data = db.func_data(id);

                if let Some(ty) = data.ty {
                    ctx.for_assoc_item(TypeVarOwner::TypedDefId(id.into()), data.type_map(), |ctx| {
                        // let _ = ctx.push_type_vars(&data.type_vars);
                        let ty = ctx.lower_ty(ty);

                        // ctx.type_vars.pop_scope();
                        ty
                    })
                } else {
                    continue;
                }
            },
            | AssocItemId::StaticId(id) => {
                let data = db.static_data(id);

                if let Some(ty) = data.ty {
                    ctx.for_assoc_item(TypeVarOwner::TypedDefId(id.into()), data.type_map(), |ctx| {
                        ctx.lower_ty(ty)
                    })
                } else {
                    continue;
                }
            },
        };

        ctx.check_kind_type(ty);
    }

    let src = ctx.source(TypeOrigin::Def(id.into()));
    let vars = var_kinds(&mut ctx, var_kinds_.to_vec(), src);

    ctx.result.diagnostics.truncate(diag_count);
    ctx.finish_class(Class {
        id,
        vars,
        fundeps,
        where_clause,
    })
}

pub(crate) fn lower_member_query(db: &dyn HirDatabase, id: MemberId) -> Arc<MemberLowerResult<Ty, Constraint>> {
    let data = db.member_data(id);
    let _type_map = data.type_map();
    let resolver = id.resolver(db.upcast());
    let mut icx = InferenceContext::new(db, resolver, TypeVarOwner::TypedDefId(id.into()), false);
    let mut ctx = LowerCtx::new(data.type_map(), &mut icx);
    let vars = data
        .type_vars
        .iter()
        .map(|&var| {
            let src = ctx.source(var);
            ctx.fresh_type(src)
        })
        .collect::<Vec<_>>();

    let _scope = ctx.type_vars.add_scope(vars.clone().into());
    let (class, types) = if let Some(class) = ctx.lower_class_path(&data.class, ClassSource::Member(id)) {
        let lower = db.lower_class(class);
        let types = data
            .types
            .iter()
            .zip(lower.class.vars.iter())
            .map(|(&ty, kind)| {
                let src = ctx.source(ty);
                let kind = kind.to_info(db, &mut ctx.icx.types, &mut ctx.icx.type_vars, src);
                let ty_ = ctx.lower_ty(ty);

                ctx.check_kind(ty_, kind);
                ty_
            })
            .collect::<List<_>>();

        (lower.class.id, types)
    } else {
        (
            ClassId::dummy(),
            data.types.iter().map(|&ty| ctx.lower_ty(ty)).collect(),
        )
    };

    let where_clause = ctx.lower_where_clause(&data.where_clause, WhereSource::Member(id));
    let src = ctx.source(TypeOrigin::Def(id.into()));
    let vars = var_kinds(&mut ctx, vars, src);

    ctx.finish_member(Member {
        id,
        class,
        vars,
        types,
        where_clause,
    })
}

pub fn verify_member(db: &dyn HirDatabase, id: MemberId) -> Vec<InferenceDiagnostic<Ty, Constraint>> {
    let lower = db.lower_member(id);

    if lower.member.class == ClassId::dummy() {
        return Vec::new();
    }

    let class = db.lower_class(lower.member.class);
    let data = db.member_data(id);
    let resolver = id.resolver(db.upcast());
    let mut ctx = InferenceContext::new(db, resolver, TypeVarOwner::TypedDefId(id.into()), false);
    let vars = lower
        .member
        .vars
        .iter()
        .zip(data.type_vars.iter())
        .map(|(t, tv)| {
            let src = ctx.source(*tv);

            t.to_info(db, &mut ctx.types, &mut ctx.type_vars, src)
        })
        .collect();

    let scope = ctx.type_vars.add_scope(vars);
    let src = ctx.source(TypeOrigin::Def(lower.member.class.into()));
    let types = lower
        .member
        .types
        .iter()
        .zip(data.types.iter())
        .map(|(t, tr)| {
            let src = ctx.source(*tr);

            t.to_info(db, &mut ctx.types, &mut ctx.type_vars, src)
        })
        .collect::<List<_>>();

    for (i, ctnt) in class.class.where_clause.constraints.iter().enumerate() {
        let ctnt = CtntInfo {
            class: ctnt.class,
            types: ctnt
                .types
                .iter()
                .map(|t| {
                    t.to_info(db, &mut ctx.types, &mut ctx.type_vars, src)
                        .replace_vars(&mut ctx.types, &types, scope)
                        .normalize(&mut ctx.types)
                })
                .collect(),
        };

        ctx.constrain(
            CtntExpected::Where(WhereSource::Class(lower.member.class), i),
            CtntFound::Member(id),
            ctnt,
        );
    }

    ctx.solve_constraints();
    ctx.finish().diagnostics
}

fn var_kinds(ctx: &mut LowerCtx, vars: Vec<TyId>, src: TySource) -> Box<[TyId]> {
    let type_kind = std::cell::OnceCell::new();
    let vars = vars
        .into_iter()
        .map(|kind| {
            let kind = ctx.subst_type(kind);
            let type_kind = *type_kind.get_or_init(|| ctx.type_kind(src));

            if let TyInfo::Unknown(u) = ctx.icx.types[kind] {
                ctx.solve_type(u, type_kind);
                type_kind
            } else {
                ctx.replace_unknowns(kind, type_kind)
            }
        })
        .collect();

    ctx.type_vars.pop_scope();
    vars
}

impl LowerResult<Ty> {
    pub fn add_diagnostics(&self, db: &dyn HirDatabase, owner: TypeVarOwner, sink: &mut DiagnosticSink) {
        self.diagnostics.iter().for_each(|it| it.add_to(db, owner, sink));
    }
}

impl ClassLowerResult<Ty, Constraint> {
    pub fn add_diagnostics(&self, db: &dyn HirDatabase, owner: TypeVarOwner, sink: &mut DiagnosticSink) {
        self.diagnostics.iter().for_each(|it| it.add_to(db, owner, sink));
    }
}

impl MemberLowerResult<Ty, Constraint> {
    pub fn add_diagnostics(&self, db: &dyn HirDatabase, owner: TypeVarOwner, sink: &mut DiagnosticSink) {
        self.diagnostics.iter().for_each(|it| it.add_to(db, owner, sink));
    }
}
