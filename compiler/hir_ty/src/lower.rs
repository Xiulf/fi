use crate::class::{Class, FunDep, Instance};
use crate::db::HirDatabase;
use crate::display::HirDisplay;
use crate::infer::InferenceContext;
use crate::ty::*;
use base_db::input::FileId;
use diagnostics::LowerDiagnostic;
use hir_def::arena::ArenaMap;
use hir_def::diagnostic::DiagnosticSink;
use hir_def::id::{
    AssocItemId, ClassId, InstanceId, Lookup, TypeAliasId, TypeCtorId, TypeVarId, TypeVarOwner, TypedDefId,
};

use hir_def::name::Name;
use hir_def::path::Path;
use hir_def::resolver::HasResolver;
use hir_def::resolver::{Resolver, TypeNs};
use hir_def::type_ref::{LocalTypeRefId, PtrLen, TypeMap, TypeRef, TypeSourceMap};
use rustc_hash::FxHashMap;
use std::sync::Arc;

pub(crate) struct Ctx<'a> {
    type_map: &'a TypeMap,
    icx: InferenceContext<'a>,
    types: ArenaMap<LocalTypeRefId, Ty>,
    diagnostics: Vec<LowerDiagnostic>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct LowerResult {
    pub ty: Ty,
    pub types: ArenaMap<LocalTypeRefId, Ty>,
    pub diagnostics: Vec<LowerDiagnostic>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct ClassLowerResult {
    pub class: Class,
    pub diagnostics: Vec<LowerDiagnostic>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct InstanceLowerResult {
    pub instance: Instance,
    pub diagnostics: Vec<LowerDiagnostic>,
}

impl<'a> Ctx<'a> {
    pub(crate) fn new(db: &'a dyn HirDatabase, type_map: &'a TypeMap, icx: InferenceContext<'a>) -> Self {
        Self {
            type_map,
            icx,
            types: ArenaMap::default(),
            diagnostics: Vec::default(),
        }
    }

    pub fn finish(mut self, ty: Ty) -> Arc<LowerResult> {
        let icx_res = self.icx.finish();

        self.diagnostics
            .extend(icx_res.diagnostics.into_iter().map(LowerDiagnostic::Inference));

        Arc::new(LowerResult {
            ty,
            types: self.types,
            diagnostics: self.diagnostics,
        })
    }

    fn finish_class(mut self, class: Class) -> Arc<ClassLowerResult> {
        let icx_res = self.icx.finish();

        self.diagnostics
            .extend(icx_res.diagnostics.into_iter().map(LowerDiagnostic::Inference));

        Arc::new(ClassLowerResult {
            class,
            diagnostics: self.diagnostics,
        })
    }

    fn finish_instance(mut self, instance: Instance) -> Arc<InstanceLowerResult> {
        let icx_res = self.icx.finish();

        self.diagnostics
            .extend(icx_res.diagnostics.into_iter().map(LowerDiagnostic::Inference));

        Arc::new(InstanceLowerResult {
            instance,
            diagnostics: self.diagnostics,
        })
    }

    pub fn for_assoc_item<T>(&mut self, owner: TypedDefId, type_map: &TypeMap, f: impl FnOnce(&mut Ctx) -> T) -> T {
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

    pub fn lower_ty(&mut self, ty: LocalTypeRefId) -> Ty {
        self.lower_ty_ext(ty).0
    }

    pub fn lower_ty_ext(&mut self, ty: LocalTypeRefId) -> (Ty, Option<TypeNs>) {
        let mut res = None;
        let lowered = match &self.type_map[ty] {
            | TypeRef::Error => TyKind::Error.intern(self.db),
            | TypeRef::Placeholder => TyKind::Error.intern(self.db),
            | TypeRef::Path(path) => return self.lower_path(&path, ty),
            | TypeRef::Tuple(tys) => {
                let tys = tys.iter().map(|&t| self.lower_ty(t));

                TyKind::Tuple(tys.collect()).intern(self.db)
            },
            | TypeRef::Ptr(to, len) => {
                let to_ = self.lower_ty(*to);
                let lib = self.resolver.lib().unwrap();

                self.check_kind_type(to_, *to);

                match len {
                    | PtrLen::Single => {
                        let ptr_ty = self.db.lang_item(lib, "ptr-type".into()).unwrap();
                        let ptr_ty = ptr_ty.as_type_ctor().unwrap();
                        let ptr_ty = self.db.type_for_ctor(ptr_ty).ty;

                        TyKind::App(ptr_ty, to_).intern(self.db)
                    },
                    | PtrLen::Multiple(None) => {
                        let ptr_ty = self.db.lang_item(lib, "ptrb-type".into()).unwrap();
                        let ptr_ty = ptr_ty.as_type_ctor().unwrap();
                        let ptr_ty = self.db.type_for_ctor(ptr_ty).ty;

                        TyKind::App(ptr_ty, to_).intern(self.db)
                    },
                    | PtrLen::Multiple(Some(sentinel)) => {
                        let ptr_ty = self.db.lang_item(lib, "ptrbs-type".into()).unwrap();
                        let ptr_ty = ptr_ty.as_type_ctor().unwrap();
                        let ptr_ty = self.db.type_for_ctor(ptr_ty).ty;
                        let base = TyKind::App(ptr_ty, to_).intern(self.db);
                        let sentinel = TyKind::Figure(sentinel.0).intern(self.db);

                        TyKind::App(base, sentinel).intern(self.db)
                    },
                }
            },
            | TypeRef::App(base, arg) => {
                let base = self.lower_ty(*base);
                let arg = self.lower_ty(*arg);

                self.check_kind_for_app(base, arg, ty);

                TyKind::App(base, arg).intern(self.db)
            },
            | TypeRef::Func(arg, ret) => {
                let arg = self.lower_ty(*arg);
                let ret = self.lower_ty(*ret);

                self.fn_type(arg, ret)
            },
            | TypeRef::Forall(vars, inner) => {
                let new_resolver = Resolver::for_type(self.db.upcast(), self.owner.into(), *inner);
                let old_resolver = std::mem::replace(&mut self.resolver, new_resolver);

                let vars = vars
                    .iter()
                    .rev()
                    .map(|&var| {
                        let data = &self.type_map[var];
                        let kind = data.kind.map(|k| self.lower_ty(k)).unwrap_or_else(|| self.fresh_kind());

                        self.push_var_kind(kind);
                        kind
                    })
                    .rev()
                    .collect::<Vec<_>>();

                let inner = self.lower_ty(*inner);

                self.resolver = old_resolver;

                vars.into_iter().fold(inner, |inner, kind| {
                    self.pop_var_kind();
                    TyKind::ForAll(kind, inner).intern(self.db)
                })
            },
            | ty => unimplemented!("{:?}", ty),
        };

        self.types.insert(ty, lowered);

        (lowered, res)
    }

    pub(crate) fn lower_path(&mut self, path: &Path, type_ref: LocalTypeRefId) -> (Ty, Option<TypeNs>) {
        let (resolution, remaining) = match self.resolver.resolve_type(self.db.upcast(), path) {
            | Some(it) => it,
            | None => {
                self.diagnostics.push(LowerDiagnostic::UnresolvedType { id: type_ref });
                return (TyKind::Error.intern(self.db), None);
            },
        };

        self.lower_partly_resolved_path(resolution, remaining.unwrap_or(0), type_ref)
    }

    pub(crate) fn lower_partly_resolved_path(
        &self,
        resolution: TypeNs,
        remaining: usize,
        type_ref: LocalTypeRefId,
    ) -> (Ty, Option<TypeNs>) {
        let ty = match resolution {
            | TypeNs::Class(_) => {
                // @TODO: resport error
                TyKind::Error.intern(self.db)
            },
            | TypeNs::TypeVar(id) => {
                let depth = self.resolver.type_var_index(id).unwrap();
                let debruijn = DebruijnIndex::new(depth as u32);
                let type_var = TypeVar::new(debruijn);

                type_var.to_ty(self.db)
            },
            | TypeNs::TypeAlias(id) => self.db.type_for_alias(id).ty,
            | TypeNs::TypeCtor(id) => self.db.type_for_ctor(id).ty,
        };

        if remaining > 0 {
            (TyKind::Error.intern(self.db), None)
        } else {
            (ty, Some(resolution))
        }
    }

    pub(crate) fn lower_constraint(&mut self, ctnt: &hir_def::type_ref::Constraint) -> Option<Constraint> {
        let class = self.lower_class_path(&ctnt.class)?;
        let types = ctnt.types.iter().map(|&t| self.lower_ty(t)).collect();

        Some(Constraint { class, types })
    }

    fn lower_class_path(&mut self, path: &Path) -> Option<ClassId> {
        let (resolution, _) = match self.resolver.resolve_type(self.db.upcast(), path) {
            | Some(it) => it,
            | None => {
                // @TODO: report error: unresolved class
                return None;
            },
        };

        match resolution {
            | TypeNs::Class(id) => Some(id),
            | _ => {
                // @TODO: report error: unresolved class
                None
            },
        }
    }
}

impl<'a> std::ops::Deref for Ctx<'a> {
    type Target = InferenceContext<'a>;

    fn deref(&self) -> &Self::Target {
        &self.icx
    }
}

impl<'a> std::ops::DerefMut for Ctx<'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.icx
    }
}

pub(crate) fn type_for_alias(db: &dyn HirDatabase, id: TypeAliasId) -> Arc<LowerResult> {
    let data = db.type_alias_data(id);
    let resolver = id.resolver(db.upcast());
    let icx = InferenceContext::new(db, resolver, id.into());
    let mut ctx = Ctx::new(db, data.type_map(), icx);
    let var_kinds = data
        .vars
        .iter()
        .enumerate()
        .rev()
        .map(|(i, &var)| {
            let data = &data.type_map()[var];
            let kind = data.kind.map(|k| ctx.lower_ty(k)).unwrap_or_else(|| ctx.fresh_kind());

            ctx.push_var_kind(kind);
            kind
        })
        .collect::<Vec<_>>();

    let mut ty = ctx.lower_ty(data.alias);
    let type_kind = std::lazy::OnceCell::new();

    for kind in var_kinds {
        let kind = ctx.subst_type(kind);

        if let TyKind::Unknown(u) = kind.lookup(db) {
            let kind = *type_kind.get_or_init(|| ctx.lang_type("type-kind"));

            ctx.solve_type(u, kind);
            ty = TyKind::ForAll(kind, ty).intern(db);
        } else {
            ty = TyKind::ForAll(kind, ty).intern(db);
        }

        ctx.pop_var_kind();
    }

    let ty = ctx.subst_type(ty);

    ctx.finish(ty)
}

pub(crate) fn type_for_alias_recover(db: &dyn HirDatabase, _cycle: &[String], id: &TypeAliasId) -> Arc<LowerResult> {
    unimplemented!();
}

pub(crate) fn type_for_ctor(db: &dyn HirDatabase, id: TypeCtorId) -> Arc<LowerResult> {
    let data = db.type_ctor_data(id);
    let resolver = id.resolver(db.upcast());
    let icx = InferenceContext::new(db, resolver, id.into());
    let mut ctx = Ctx::new(db, data.type_map(), icx);
    let mut ty = TyKind::Ctor(id).intern(db);
    let var_kinds = data
        .vars
        .iter()
        .enumerate()
        .rev()
        .map(|(i, &var)| {
            let data = &data.type_map()[var];
            let kind = data.kind.map(|k| ctx.lower_ty(k)).unwrap_or_else(|| ctx.fresh_kind());
            let var = TypeVar::new(DebruijnIndex::new(i as u32));
            let arg = var.to_ty(db);

            ty = TyKind::App(ty, arg).intern(db);
            ctx.push_var_kind(kind);
            kind
        })
        .collect::<Vec<_>>();

    for (_, ctor) in data.ctors.iter() {
        for &ty in ctor.types.iter() {
            let ty_ = ctx.lower_ty(ty);

            ctx.check_kind_type(ty_, ty);
        }
    }

    let type_kind = std::lazy::OnceCell::new();

    for kind in var_kinds {
        let kind = ctx.subst_type(kind);

        if let TyKind::Unknown(u) = kind.lookup(db) {
            let kind = *type_kind.get_or_init(|| ctx.lang_type("type-kind"));

            ctx.solve_type(u, kind);
            ty = TyKind::ForAll(kind, ty).intern(db);
        } else {
            ty = TyKind::ForAll(kind, ty).intern(db);
        }

        ctx.pop_var_kind();
    }

    let ty = ctx.subst_type(ty);

    ctx.finish(ty)
}

pub(crate) fn type_for_ctor_recover(db: &dyn HirDatabase, _cycle: &[String], id: &TypeCtorId) -> Arc<LowerResult> {
    unimplemented!();
}

pub(crate) fn kind_for_ctor(db: &dyn HirDatabase, id: TypeCtorId) -> Ty {
    let data = db.type_ctor_data(id);
    let resolver = id.resolver(db.upcast());
    let icx = InferenceContext::new(db, resolver, id.into());
    let mut ctx = Ctx::new(db, data.type_map(), icx);

    if let Some(kind) = data.kind {
        ctx.lower_ty(kind)
    } else {
        let mut ty = db.type_for_ctor(id).ty;
        let mut vars = Vec::with_capacity(data.vars.len());

        while let TyKind::ForAll(var, inner) = ty.lookup(db) {
            vars.push(var);
            ty = inner;
        }

        let ty_kind = ctx.lang_type("type-kind");

        vars.into_iter().rev().fold(ty_kind, |ty, var| ctx.fn_type(var, ty))
    }
}

pub(crate) fn kind_for_ctor_recover(db: &dyn HirDatabase, _cycle: &[String], id: &TypeCtorId) -> Ty {
    let data = db.type_ctor_data(*id);
    unimplemented!("{}", data.name);
}

pub(crate) fn lower_class_query(db: &dyn HirDatabase, id: ClassId) -> Arc<ClassLowerResult> {
    let data = db.class_data(id);
    let type_map = data.type_map();
    let resolver = id.resolver(db.upcast());
    let icx = InferenceContext::new(db, resolver, id.into());
    let mut ctx = Ctx::new(db, data.type_map(), icx);
    let mut var_kinds_ = Vec::with_capacity(data.vars.len());
    let vars = data
        .vars
        .iter()
        .enumerate()
        .rev()
        .map(|(i, &var)| {
            let debruijn = DebruijnIndex::new(i as u32);
            let type_var = TypeVar::new(debruijn);
            let kind = type_map[var]
                .kind
                .map(|k| ctx.lower_ty(k))
                .unwrap_or_else(|| ctx.fresh_kind());

            var_kinds_.push(kind);
            ctx.push_var_kind(kind);

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

    let diag_count1 = ctx.diagnostics.len();
    let diag_count2 = ctx.icx.result.diagnostics.len();

    for &(_, id) in data.items.iter() {
        let (ty, origin) = match id {
            | AssocItemId::FuncId(id) => {
                let data = db.func_data(id);

                if let Some(ty) = data.ty {
                    ctx.for_assoc_item(id.into(), data.type_map(), |ctx| (ctx.lower_ty(ty), ty))
                } else {
                    continue;
                }
            },
            | AssocItemId::StaticId(id) => {
                let data = db.static_data(id);

                if let Some(ty) = data.ty {
                    ctx.for_assoc_item(id.into(), data.type_map(), |ctx| (ctx.lower_ty(ty), ty))
                } else {
                    continue;
                }
            },
        };

        ctx.check_kind_type(ty, origin);
    }

    let vars = var_kinds(&mut ctx, var_kinds_);

    ctx.diagnostics.truncate(diag_count1);
    ctx.icx.result.diagnostics.truncate(diag_count2);

    ctx.finish_class(Class { id, vars, fundeps })
}

pub(crate) fn lower_instance_query(db: &dyn HirDatabase, id: InstanceId) -> Arc<InstanceLowerResult> {
    let data = db.instance_data(id);
    let type_map = data.type_map();
    let resolver = id.resolver(db.upcast());
    let icx = InferenceContext::new(db, resolver, id.into());
    let mut ctx = Ctx::new(db, data.type_map(), icx);
    let vars = data
        .vars
        .iter()
        .enumerate()
        .rev()
        .map(|(i, &var)| {
            let kind = ctx.fresh_kind();

            ctx.push_var_kind(kind);
            kind
        })
        .rev()
        .collect::<Vec<_>>();

    let (class, types) = if let Some(class) = ctx.lower_class_path(&data.class) {
        let lower = db.lower_class(class);

        (
            lower.class.id,
            data.types
                .iter()
                .zip(lower.class.vars.iter())
                .map(|(&ty, &kind)| {
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
        .filter_map(|c| ctx.lower_constraint(c))
        .collect();

    let vars = var_kinds(&mut ctx, vars);

    ctx.finish_instance(Instance {
        id,
        class,
        vars,
        types,
        constraints,
    })
}

fn var_kinds(ctx: &mut Ctx, vars: Vec<Ty>) -> Box<[Ty]> {
    let type_kind = std::lazy::OnceCell::new();

    vars.into_iter()
        .map(|kind| {
            let kind = ctx.subst_type(kind);
            let type_kind = *type_kind.get_or_init(|| ctx.lang_type("type-kind"));

            ctx.pop_var_kind();

            if let TyKind::Unknown(u) = kind.lookup(ctx.db) {
                ctx.solve_type(u, type_kind);
                type_kind
            } else {
                ctx.replace_unknowns(kind, type_kind)
            }
        })
        .collect()
}

impl LowerResult {
    pub fn add_diagnostics(
        &self,
        db: &dyn HirDatabase,
        owner: TypeVarOwner,
        file_id: FileId,
        source_map: &TypeSourceMap,
        sink: &mut DiagnosticSink,
    ) {
        self.diagnostics
            .iter()
            .for_each(|it| it.add_to(db, owner, file_id, source_map, sink));
    }
}

impl ClassLowerResult {
    pub fn add_diagnostics(
        &self,
        db: &dyn HirDatabase,
        owner: TypeVarOwner,
        file_id: FileId,
        source_map: &TypeSourceMap,
        sink: &mut DiagnosticSink,
    ) {
        self.diagnostics
            .iter()
            .for_each(|it| it.add_to(db, owner, file_id, source_map, sink));
    }
}

impl InstanceLowerResult {
    pub fn add_diagnostics(
        &self,
        db: &dyn HirDatabase,
        owner: TypeVarOwner,
        file_id: FileId,
        source_map: &TypeSourceMap,
        sink: &mut DiagnosticSink,
    ) {
        self.diagnostics
            .iter()
            .for_each(|it| it.add_to(db, owner, file_id, source_map, sink));
    }
}

pub(crate) mod diagnostics {
    use crate::db::HirDatabase;
    use crate::diagnostics::*;
    use crate::infer::diagnostics::InferenceDiagnostic;
    use base_db::input::FileId;
    use hir_def::diagnostic::DiagnosticSink;
    use hir_def::id::TypeVarOwner;
    use hir_def::type_ref::{LocalTypeRefId, TypeSourceMap};

    #[derive(Debug, PartialEq, Eq)]
    pub enum LowerDiagnostic {
        Inference(InferenceDiagnostic),
        UnresolvedType { id: LocalTypeRefId },
    }

    impl LowerDiagnostic {
        pub(crate) fn add_to(
            &self,
            db: &dyn HirDatabase,
            owner: TypeVarOwner,
            file_id: FileId,
            source_map: &TypeSourceMap,
            sink: &mut DiagnosticSink,
        ) {
            match self {
                | LowerDiagnostic::Inference(i) => i.add_to(db, owner, sink),
                | LowerDiagnostic::UnresolvedType { id } => sink.push(UnresolvedType {
                    file: file_id,
                    ty: source_map.type_ref_syntax(*id).unwrap(),
                }),
            }
        }
    }
}
