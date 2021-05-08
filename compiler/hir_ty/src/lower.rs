use crate::db::HirDatabase;
use crate::display::HirDisplay;
use crate::infer::InferenceContext;
use crate::ty::*;
use base_db::input::FileId;
use diagnostics::LowerDiagnostic;
use hir_def::arena::ArenaMap;
use hir_def::diagnostic::DiagnosticSink;
use hir_def::id::{Lookup, TypeAliasId, TypeCtorId, TypeVarOwner};
use hir_def::name::Name;
use hir_def::path::Path;
use hir_def::resolver::HasResolver;
use hir_def::resolver::{Resolver, TypeNs};
use hir_def::type_ref::{LocalTypeRefId, PtrLen, TypeMap, TypeRef, TypeSourceMap};
use std::sync::Arc;

pub(crate) struct Ctx<'a> {
    resolver: &'a Resolver,
    type_map: &'a TypeMap,
    icx: InferenceContext<'a>,
    in_binders: DebruijnIndex,
    types: ArenaMap<LocalTypeRefId, Ty>,
    diagnostics: Vec<LowerDiagnostic>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct LowerResult {
    pub ty: Ty,
    pub types: ArenaMap<LocalTypeRefId, Ty>,
    pub diagnostics: Vec<LowerDiagnostic>,
}

impl<'a> Ctx<'a> {
    pub(crate) fn new(
        db: &'a dyn HirDatabase,
        resolver: &'a Resolver,
        type_map: &'a TypeMap,
        icx: InferenceContext<'a>,
    ) -> Self {
        Self {
            resolver,
            type_map,
            icx,
            in_binders: DebruijnIndex::INNER,
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

    pub fn with_debruijn<T>(&mut self, debruijn: DebruijnIndex, f: impl FnOnce(&mut Ctx) -> T) -> T {
        let in_binders = std::mem::replace(&mut self.in_binders, debruijn);
        let res = f(self);

        self.in_binders = in_binders;
        res
    }

    pub fn with_shifted_in<T>(&mut self, debruijn: DebruijnIndex, f: impl FnOnce(&mut Ctx) -> T) -> T {
        self.with_debruijn(self.in_binders.shifted_in_from(debruijn), f)
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
                let inner = self.with_shifted_in(DebruijnIndex::ONE, |ctx| ctx.lower_ty(*inner));

                vars.iter().fold(inner, |inner, var| {
                    let var = &self.type_map[*var];
                    let kind = var
                        .kind
                        .map(|k| self.lower_ty(k))
                        .unwrap_or_else(|| TyKind::Error.intern(self.db));

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

                TypeVar::new(debruijn).to_ty(self.db)
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
    let icx = InferenceContext::new(db, id.into());
    let mut ctx = Ctx::new(db, &resolver, data.type_map(), icx);
    let var_kinds = data
        .vars
        .iter()
        .enumerate()
        .rev()
        .map(|(i, &var)| {
            let data = &data.type_map()[var];
            let kind = data.kind.map(|k| ctx.lower_ty(k)).unwrap_or_else(|| ctx.fresh_kind());
            let var = TypeVar::new(DebruijnIndex::new(i as u32));

            ctx.set_var_kind(var, kind);
            kind
        })
        .collect::<Vec<_>>();

    let mut ty = ctx.with_shifted_in(DebruijnIndex::new(data.vars.len() as u32), |ctx| {
        ctx.lower_ty(data.alias)
    });

    let type_kind = std::lazy::OnceCell::new();

    for kind in var_kinds {
        let kind = ctx.subst_type(kind);

        if let TyKind::Unknown(_) = kind.lookup(db) {
            let kind = type_kind.get_or_init(|| ctx.lang_type("type-kind"));

            ty = TyKind::ForAll(*kind, ty).intern(db);
        } else {
            ty = TyKind::ForAll(kind, ty).intern(db);
        }
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
    let icx = InferenceContext::new(db, id.into());
    let mut ctx = Ctx::new(db, &resolver, data.type_map(), icx);
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
            ctx.set_var_kind(var, kind);
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

        if let TyKind::Unknown(_) = kind.lookup(db) {
            let kind = type_kind.get_or_init(|| ctx.lang_type("type-kind"));

            ty = TyKind::ForAll(*kind, ty).intern(db);
        } else {
            ty = TyKind::ForAll(kind, ty).intern(db);
        }
    }

    let ty = ctx.subst_type(ty);

    ctx.finish(ty)
}

pub(crate) fn type_for_ctor_recover(db: &dyn HirDatabase, _cycle: &[String], id: &TypeCtorId) -> Arc<LowerResult> {
    unimplemented!();
}

pub(crate) fn kind_for_ctor(db: &dyn HirDatabase, id: TypeCtorId) -> Ty {
    let data = db.type_ctor_data(id);
    let mut icx = InferenceContext::new(db, id.into());

    if let Some(kind) = data.kind {
        let resolver = id.resolver(db.upcast());
        let mut ctx = Ctx::new(db, &resolver, data.type_map(), icx);

        ctx.lower_ty(kind)
    } else {
        let ty = db.type_for_ctor(id).ty;

        icx.infer_kind(ty, LocalTypeRefId::DUMMY)
    }
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

mod diagnostics {
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
