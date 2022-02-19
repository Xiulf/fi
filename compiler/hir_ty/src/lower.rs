use crate::class::{Class, FunDep, Member};
use crate::db::HirDatabase;
use crate::infer::diagnostics::{ClassSource, InferenceDiagnostic};
use crate::infer::InferenceContext;
use crate::info::{CtntInfo, FieldInfo, FromInfo, ToInfo, TyId, TyInfo, TySource, TypeOrigin};
use crate::ty::{Constraint, DebruijnIndex, List, Ty, TypeVar};
use hir_def::arena::ArenaMap;
use hir_def::diagnostic::DiagnosticSink;
use hir_def::id::*;
use hir_def::path::Path;
use hir_def::resolver::HasResolver;
use hir_def::resolver::{Resolver, TypeNs};
use hir_def::type_ref::{LocalTypeRefId, PtrLen, TypeMap, TypeRef};
use rustc_hash::FxHashMap;
use std::sync::Arc;

pub struct LowerCtx<'a, 'b> {
    type_map: &'a TypeMap,
    icx: &'a mut InferenceContext<'b>,
    types: ArenaMap<LocalTypeRefId, TyId>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct LowerResult<T> {
    pub ty: T,
    pub types: ArenaMap<LocalTypeRefId, T>,
    pub diagnostics: Vec<InferenceDiagnostic<Ty, Constraint>>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct ClassLowerResult<T> {
    pub class: Class<T>,
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
            ty,
            types: self.types,
            diagnostics: icx_res.diagnostics,
        };

        Arc::new(LowerResult::from_info(self.icx.db, &self.icx.types, res))
    }

    fn finish_class(self, class: Class<TyId>) -> Arc<ClassLowerResult<Ty>> {
        let icx_res = self.icx.finish_mut();
        let res = ClassLowerResult {
            class,
            diagnostics: icx_res.diagnostics,
        };

        Arc::new(ClassLowerResult::from_info(self.db, &self.icx.types, res))
    }

    fn finish_instance(self, instance: Member<TyId, CtntInfo>) -> Arc<MemberLowerResult<Ty, Constraint>> {
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
        self.lower_ty_ext(ty).0
    }

    pub fn lower_ty_ext(&mut self, ty: LocalTypeRefId) -> (TyId, Option<TypeNs>) {
        let mut res = None;
        let src = self.source(ty);
        let lowered = match &self.type_map[ty] {
            | TypeRef::Error => self.error(src),
            | TypeRef::Placeholder => self.fresh_type_without_kind(src),
            | TypeRef::Figure(i) => self.icx.types.insert(TyInfo::Figure(*i), src),
            | TypeRef::Symbol(s) => self.icx.types.insert(TyInfo::Symbol(s.clone().into()), src),
            | TypeRef::Path(path) => {
                let r = self.lower_path(&path, ty);

                res = r.1;
                r.0
            },
            | TypeRef::Tuple(tys) => {
                let tys = tys.iter().map(|&t| self.lower_ty(t)).collect();

                self.icx.types.insert(TyInfo::Tuple(tys), src)
            },
            | TypeRef::Ptr(to, len) => {
                let to_ = self.lower_ty(*to);

                self.check_kind_type(to_, *to);

                let info = match len {
                    | PtrLen::Single => {
                        let ptr_ty = self.lang_type("ptr-type", src);

                        TyInfo::App(ptr_ty, vec![to_].into())
                    },
                    | PtrLen::Multiple(None) => {
                        let ptr_ty = self.lang_type("ptrb-typ", src);

                        TyInfo::App(ptr_ty, vec![to_].into())
                    },
                    | PtrLen::Multiple(Some(sentinel)) => {
                        let ptr_ty = self.lang_type("ptrbs-typ", src);
                        let sentinel = self.icx.types.insert(TyInfo::Figure(sentinel.0), src);

                        TyInfo::App(ptr_ty, vec![to_, sentinel].into())
                    },
                };

                self.icx.types.insert(info, src)
            },
            | TypeRef::App(base, args) => {
                let base = self.lower_ty(*base);
                let args = args.iter().map(|a| self.lower_ty(*a)).collect::<List<_>>();

                self.check_kind_for_app(base, &args, ty);

                self.icx.types.insert(TyInfo::App(base, args), src)
            },
            | TypeRef::Func(args, ret) => {
                let args_ = args.iter().map(|arg| self.lower_ty(*arg)).collect::<List<_>>();
                let ret_ = self.lower_ty(*ret);

                self.fn_type(args_, ret_, src)
            },
            | TypeRef::Record(fields, tail) => {
                let row = self.lower_row(fields, *tail, ty);
                let record_ty = self.lang_type("record-type", src);
                let type_kind = self.type_kind(src);
                let row_kind = self.lang_type("row-kind", src);
                let row_kind = self.icx.types.insert(TyInfo::App(row_kind, [type_kind].into()), src);

                self.check_kind(row, row_kind, ty);

                self.icx.types.insert(TyInfo::App(record_ty, [row].into()), src)
            },
            | TypeRef::Slice(of) => {
                let slice_ty = self.lang_type("slice-type", src);
                let of_ = self.lower_ty(*of);

                self.check_kind_type(of_, *of);

                self.icx.types.insert(TyInfo::App(slice_ty, [of_].into()), src)
            },
            | TypeRef::Row(fields, tail) => self.lower_row(fields, *tail, ty),
            | TypeRef::Forall(vars, inner) => {
                let new_resolver = Resolver::for_type(self.db.upcast(), self.owner, *inner);
                let old_resolver = std::mem::replace(&mut self.resolver, new_resolver);
                let vars = vars
                    .iter()
                    .map(|&var| {
                        let data = &self.type_map[var];

                        data.kind
                            .map(|k| self.lower_ty(k))
                            .unwrap_or_else(|| self.fresh_kind(src))
                    })
                    .collect::<Vec<_>>();

                self.push_var_kind(vars.clone().into());

                let inner = self.lower_ty(*inner);
                let type_kind = self.type_kind(src);

                self.resolver = old_resolver;

                let kinds = vars
                    .into_iter()
                    .map(|kind| {
                        let kind = self.subst_type(kind);

                        if let TyInfo::Unknown(u) = self.icx.types[kind] {
                            self.solve_type(u, type_kind);
                            type_kind
                        } else {
                            kind
                        }
                    })
                    .collect();

                self.pop_var_kind();

                self.icx.types.insert(TyInfo::ForAll(kinds, inner), src)
            },
            | TypeRef::Constraint(ctnt, inner) => {
                if let Some(ctnt) = self.lower_constraint(ctnt, ClassSource::TyCtnt(ty)) {
                    let inner = self.lower_ty(*inner);

                    self.icx.types.insert(TyInfo::Ctnt(ctnt, inner), src)
                } else {
                    self.error(src)
                }
            },
            | ty => unimplemented!("{:?}", ty),
        };

        self.types.insert(ty, lowered);

        (lowered, res)
    }

    pub(crate) fn lower_row(
        &mut self,
        fields: &[hir_def::type_ref::Field],
        tail: Option<LocalTypeRefId>,
        type_ref: LocalTypeRefId,
    ) -> TyId {
        let src = self.source(type_ref);
        let elem_kind = self.fresh_kind(src);
        let fields = fields
            .iter()
            .map(|f| {
                let ty = self.lower_ty(f.ty);

                self.check_kind(ty, elem_kind, f.ty);

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

            self.check_kind(ty, row_kind, t);
            ty
        });

        self.icx.types.insert(TyInfo::Row(fields, tail), src)
    }

    pub(crate) fn lower_path(&mut self, path: &Path, type_ref: LocalTypeRefId) -> (TyId, Option<TypeNs>) {
        let src = self.source(type_ref);
        let (resolution, vis, remaining) = match self.resolver.resolve_type(self.db.upcast(), path) {
            | Some(it) => it,
            | None => {
                self.report(InferenceDiagnostic::UnresolvedType { id: type_ref });
                return (self.icx.types.insert(TyInfo::Error, src), None);
            },
        };

        if path.segments().len() > 1 && !vis.is_visible_from(self.db.upcast(), self.resolver.module().unwrap()) {
            self.report(InferenceDiagnostic::PrivateType { id: type_ref });
        }

        self.lower_partly_resolved_path(resolution, remaining.unwrap_or(0), type_ref)
    }

    pub(crate) fn lower_partly_resolved_path(
        &mut self,
        resolution: TypeNs,
        remaining: usize,
        type_ref: LocalTypeRefId,
    ) -> (TyId, Option<TypeNs>) {
        let src = self.source(type_ref);
        let ty = match resolution {
            | TypeNs::Class(_) => {
                // @TODO: report error
                self.icx.types.insert(TyInfo::Error, src)
            },
            | TypeNs::TypeVar(id) => {
                let (idx, depth) = self.resolver.type_var_index(id).unwrap();
                let debruijn = DebruijnIndex::new(depth as u32);
                let type_var = TypeVar::new(idx as u32, debruijn);

                self.icx.types.insert(TyInfo::TypeVar(type_var), src)
            },
            | TypeNs::TypeAlias(id) => self.db.type_for_alias(id).ty.to_info(self.db, &mut self.icx.types, src),
            | TypeNs::TypeCtor(id) => self.icx.types.insert(TyInfo::Ctor(id), src),
        };

        if remaining > 0 {
            (self.icx.types.insert(TyInfo::Error, src), None)
        } else {
            (ty, Some(resolution))
        }
    }

    pub(crate) fn lower_constraint(
        &mut self,
        ctnt: &hir_def::type_ref::Constraint,
        src: ClassSource,
    ) -> Option<CtntInfo> {
        let class = self.lower_class_path(&ctnt.class, src)?;
        let types = ctnt.types.iter().map(|&t| self.lower_ty(t)).collect();

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

pub(crate) fn value_ty(db: &dyn HirDatabase, id: ValueTyDefId) -> Ty {
    match id {
        | ValueTyDefId::FuncId(id) => func_ty(db, id).ty,
        | ValueTyDefId::StaticId(id) => static_ty(db, id),
        | ValueTyDefId::ConstId(id) => const_ty(db, id),
        | ValueTyDefId::CtorId(id) => db.ctor_ty(id).ty,
    }
}

pub fn func_ty(db: &dyn HirDatabase, id: FuncId) -> Arc<LowerResult<Ty>> {
    let data = db.func_data(id);
    let loc = id.lookup(db.upcast());

    if let Some(ty) = data.ty {
        let resolver = id.resolver(db.upcast());
        let mut icx = InferenceContext::new(db, resolver, TypeVarOwner::TypedDefId(id.into()));
        let mut ctx = LowerCtx::new(data.type_map(), &mut icx);
        let src = ctx.source(ty);
        let ty = if let ContainerId::Class(class) = loc.container {
            let lower = db.lower_class(class);
            let ctnt = CtntInfo {
                class,
                types: (0..lower.class.vars.len() as u32)
                    .map(|i| {
                        ctx.icx
                            .types
                            .insert(TyInfo::TypeVar(TypeVar::new(i, DebruijnIndex::INNER)), src)
                    })
                    .collect(),
            };

            let vars = lower
                .class
                .vars
                .iter()
                .map(|&v| v.to_info(db, &mut ctx.icx.types, src))
                .collect::<List<_>>();

            ctx.push_var_kind(vars.clone());

            let mut ty = ctx.lower_ty(ty);

            ty = ctx.icx.types.insert(TyInfo::Ctnt(ctnt, ty), src);

            if !lower.class.vars.is_empty() {
                ty = ctx.icx.types.insert(TyInfo::ForAll(vars, ty), src);
            }

            ty
        } else {
            ctx.lower_ty(ty)
        };

        let ty = ctx.subst_type(ty);

        ctx.finish(ty)
    } else {
        let def = id.into();
        let infer = db.infer(def);
        let ty = infer.self_type;

        Arc::new(LowerResult {
            ty,
            types: ArenaMap::default(),
            diagnostics: Vec::new(),
        })
    }
}

pub(crate) fn static_ty(db: &dyn HirDatabase, id: StaticId) -> Ty {
    let data = db.static_data(id);

    if let Some(ty) = data.ty {
        let resolver = id.resolver(db.upcast());
        let mut icx = InferenceContext::new(db, resolver, TypeVarOwner::TypedDefId(id.into()));
        let mut lcx = LowerCtx::new(data.type_map(), &mut icx);
        let ty = lcx.lower_ty(ty);

        lcx.finish(ty).ty
    } else {
        let def = id.into();
        let infer = db.infer(def);
        let body = db.body(def);

        infer.type_of_expr[body.body_expr()]
    }
}

pub(crate) fn const_ty(db: &dyn HirDatabase, id: ConstId) -> Ty {
    let data = db.const_data(id);

    if let Some(ty) = data.ty {
        let resolver = id.resolver(db.upcast());
        let mut icx = InferenceContext::new(db, resolver, TypeVarOwner::TypedDefId(id.into()));
        let mut lcx = LowerCtx::new(data.type_map(), &mut icx);
        let ty = lcx.lower_ty(ty);

        lcx.finish(ty).ty
    } else {
        let def = id.into();
        let infer = db.infer(def);
        let body = db.body(def);

        infer.type_of_expr[body.body_expr()]
    }
}

pub(crate) fn ctor_ty(db: &dyn HirDatabase, id: CtorId) -> Arc<LowerResult<Ty>> {
    let ty_data = db.type_ctor_data(id.parent);
    let ctor_data = &ty_data.ctors[id.local_id];
    let resolver = id.resolver(db.upcast());
    let mut icx = InferenceContext::new(db, resolver, TypeVarOwner::TypedDefId(id.into()));
    let mut ctx = LowerCtx::new(ty_data.type_map(), &mut icx);
    let src = ctx.source(TypeOrigin::Synthetic);
    let mut ret = ctx.icx.types.insert(TyInfo::Ctor(id.parent), src);
    let kind = db.kind_for_ctor(id.parent).ty.to_info(db, &mut ctx.icx.types, src);
    let vars = match ctx.icx.types[kind].clone() {
        | TyInfo::Func(vars, _) => {
            ctx.push_var_kind(vars.clone());
            Some(vars)
        },
        | _ => None,
    };

    if let Some(vars) = &vars {
        let args = (0..vars.len())
            .map(|i| {
                ctx.icx
                    .types
                    .insert(TyInfo::TypeVar(TypeVar::new(i as u32, DebruijnIndex::INNER)), src)
            })
            .collect();

        ret = ctx.icx.types.insert(TyInfo::App(ret, args), src);
    }

    let args = ctor_data
        .types
        .iter()
        .map(|&ty| {
            let ty_ = ctx.lower_ty(ty);

            ctx.check_kind_type(ty_, ty);
            ty_
        })
        .collect::<List<_>>();

    let ty = if !args.is_empty() {
        ctx.fn_type(args, ret, src)
    } else {
        ret
    };

    let ty = if let Some(vars) = vars {
        ctx.icx.types.insert(TyInfo::ForAll(vars, ty), src)
    } else {
        ty
    };

    ctx.finish(ty)
}

pub(crate) fn type_for_alias(db: &dyn HirDatabase, id: TypeAliasId) -> Arc<LowerResult<Ty>> {
    let data = db.type_alias_data(id);
    let resolver = id.resolver(db.upcast());
    let mut icx = InferenceContext::new(db, resolver, TypeVarOwner::TypedDefId(id.into()));
    let mut ctx = LowerCtx::new(data.type_map(), &mut icx);
    let src = ctx.source(TypeOrigin::Synthetic);
    let var_kinds = data
        .vars
        .iter()
        .enumerate()
        .map(|(_i, &var)| {
            let data = &data.type_map()[var];

            data.kind
                .map(|k| ctx.lower_ty(k))
                .unwrap_or_else(|| ctx.fresh_kind(src))
        })
        .collect::<Vec<_>>();

    ctx.push_var_kind(var_kinds.clone().into());

    let ty = ctx.lower_ty(data.alias);
    let type_kind = ctx.type_kind(src);
    let kinds = var_kinds
        .into_iter()
        .map(|kind| {
            let kind = ctx.subst_type(kind);

            if let TyInfo::Unknown(u) = ctx.icx.types[kind] {
                ctx.solve_type(u, type_kind);
                type_kind
            } else {
                kind
            }
        })
        .collect::<List<_>>();

    ctx.pop_var_kind();

    let ty = if !kinds.is_empty() {
        ctx.icx.types.insert(TyInfo::ForAll(kinds, ty), src)
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
    let mut icx = InferenceContext::new(db, resolver, TypeVarOwner::TypedDefId(id.into()));
    let mut ctx = LowerCtx::new(data.type_map(), &mut icx);
    let src = ctx.source(TypeOrigin::Synthetic);

    if let Some(kind) = data.kind {
        let ty = ctx.lower_ty(kind);

        ctx.finish(ty)
    } else {
        let var_kinds = data
            .vars
            .iter()
            .map(|&var| {
                let data = &data.type_map()[var];

                data.kind
                    .map(|k| ctx.lower_ty(k))
                    .unwrap_or_else(|| ctx.fresh_kind(src))
            })
            .collect::<Vec<_>>();

        ctx.push_var_kind(var_kinds.clone().into());

        let ty_kind = if var_kinds.is_empty() {
            ctx.type_kind(src)
        } else {
            let ty_kind = ctx.type_kind(src);

            ctx.icx
                .types
                .insert(TyInfo::Func(var_kinds.clone().into(), ty_kind), src)
        };

        ctx.icx.result.self_type = ty_kind;

        for (_, ctor) in data.ctors.iter() {
            for &ty in ctor.types.iter() {
                let ty_ = ctx.lower_ty(ty);

                ctx.check_kind_type(ty_, ty);
            }
        }

        let type_kind = ctx.type_kind(src);

        for kind in var_kinds {
            let kind = ctx.subst_type(kind);

            if let TyInfo::Unknown(u) = ctx.icx.types[kind] {
                ctx.solve_type(u, type_kind);
            }
        }

        ctx.pop_var_kind();

        let ty = ctx.subst_type(ty_kind);

        ctx.finish(ty)
    }
}

pub(crate) fn kind_for_ctor_recover(db: &dyn HirDatabase, _cycle: &[String], id: &TypeCtorId) -> Arc<LowerResult<Ty>> {
    let data = db.type_ctor_data(*id);
    unimplemented!("{}", data.name);
}

pub(crate) fn lower_class_query(db: &dyn HirDatabase, id: ClassId) -> Arc<ClassLowerResult<Ty>> {
    let data = db.class_data(id);
    let type_map = data.type_map();
    let resolver = id.resolver(db.upcast());
    let mut icx = InferenceContext::new(db, resolver, TypeVarOwner::TypedDefId(id.into()));
    let mut ctx = LowerCtx::new(data.type_map(), &mut icx);
    let mut var_kinds_ = Vec::with_capacity(data.vars.len());
    let src = ctx.source(TypeOrigin::Synthetic);
    let vars = data
        .vars
        .iter()
        .enumerate()
        .map(|(i, &var)| {
            let type_var = TypeVar::new(i as u32, DebruijnIndex::INNER);
            let kind = type_map[var]
                .kind
                .map(|k| ctx.lower_ty(k))
                .unwrap_or_else(|| ctx.fresh_kind(src));

            var_kinds_.push(kind);

            (&type_map[var].name, type_var)
        })
        .collect::<FxHashMap<_, _>>();

    ctx.push_var_kind(var_kinds_.clone().into());

    let fundeps = data
        .fundeps
        .iter()
        .map(|dep| FunDep {
            determiners: dep.determiners.iter().map(|n| vars[n]).collect(),
            determined: dep.determined.iter().map(|n| vars[n]).collect(),
        })
        .collect();

    let diag_count = ctx.result.diagnostics.len();

    for &(_, id) in data.items.iter() {
        let (ty, origin) = match id {
            | AssocItemId::FuncId(id) => {
                let data = db.func_data(id);

                if let Some(ty) = data.ty {
                    ctx.for_assoc_item(TypeVarOwner::TypedDefId(id.into()), data.type_map(), |ctx| {
                        (ctx.lower_ty(ty), ty)
                    })
                } else {
                    continue;
                }
            },
            | AssocItemId::StaticId(id) => {
                let data = db.static_data(id);

                if let Some(ty) = data.ty {
                    ctx.for_assoc_item(TypeVarOwner::TypedDefId(id.into()), data.type_map(), |ctx| {
                        (ctx.lower_ty(ty), ty)
                    })
                } else {
                    continue;
                }
            },
        };

        ctx.check_kind_type(ty, origin);
    }

    let vars = var_kinds(&mut ctx, var_kinds_, src);

    ctx.result.diagnostics.truncate(diag_count);

    ctx.finish_class(Class { id, vars, fundeps })
}

pub(crate) fn lower_member_query(db: &dyn HirDatabase, id: MemberId) -> Arc<MemberLowerResult<Ty, Constraint>> {
    let data = db.member_data(id);
    let _type_map = data.type_map();
    let resolver = id.resolver(db.upcast());
    let mut icx = InferenceContext::new(db, resolver, TypeVarOwner::TypedDefId(id.into()));
    let mut ctx = LowerCtx::new(data.type_map(), &mut icx);
    let src = ctx.source(TypeOrigin::Synthetic);
    let vars = data
        .vars
        .iter()
        .enumerate()
        .map(|(_i, &_var)| ctx.fresh_kind(src))
        .collect::<Vec<_>>();

    ctx.push_var_kind(vars.clone().into());

    let (class, types) = if let Some(class) = ctx.lower_class_path(&data.class, ClassSource::Member(id)) {
        let lower = db.lower_class(class);

        (
            lower.class.id,
            data.types
                .iter()
                .zip(lower.class.vars.iter())
                .map(|(&ty, kind)| {
                    let kind = kind.to_info(db, &mut ctx.icx.types, src);
                    let ty_ = ctx.lower_ty(ty);

                    ctx.check_kind(ty_, kind, ty);
                    ty_
                })
                .collect(),
        )
    } else {
        (
            ClassId::dummy(),
            data.types.iter().map(|&ty| ctx.lower_ty(ty)).collect(),
        )
    };

    let constraints = data
        .constraints
        .iter()
        .enumerate()
        .filter_map(|(i, c)| ctx.lower_constraint(c, ClassSource::MemberCtnt(id, i)))
        .collect();

    let vars = var_kinds(&mut ctx, vars, src);

    ctx.finish_instance(Member {
        id,
        class,
        vars,
        types,
        constraints,
    })
}

fn var_kinds(ctx: &mut LowerCtx, vars: Vec<TyId>, src: TySource) -> Box<[TyId]> {
    let type_kind = std::lazy::OnceCell::new();
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

    ctx.pop_var_kind();
    vars
}

impl LowerResult<Ty> {
    pub fn add_diagnostics(&self, db: &dyn HirDatabase, owner: TypeVarOwner, sink: &mut DiagnosticSink) {
        self.diagnostics.iter().for_each(|it| it.add_to(db, owner, sink));
    }
}

impl ClassLowerResult<Ty> {
    pub fn add_diagnostics(&self, db: &dyn HirDatabase, owner: TypeVarOwner, sink: &mut DiagnosticSink) {
        self.diagnostics.iter().for_each(|it| it.add_to(db, owner, sink));
    }
}

impl MemberLowerResult<Ty, Constraint> {
    pub fn add_diagnostics(&self, db: &dyn HirDatabase, owner: TypeVarOwner, sink: &mut DiagnosticSink) {
        self.diagnostics.iter().for_each(|it| it.add_to(db, owner, sink));
    }
}
