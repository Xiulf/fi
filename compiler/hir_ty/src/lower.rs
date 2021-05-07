use crate::db::HirDatabase;
use crate::infer::InferenceContext;
use crate::ty::*;
use hir_def::id::{Lookup, TypeAliasId, TypeCtorId};
use hir_def::name::Name;
use hir_def::path::Path;
use hir_def::resolver::HasResolver;
use hir_def::resolver::{Resolver, TypeNs};
use hir_def::type_ref::{PtrLen, TypeRef, TypeRefId};

pub(crate) struct Ctx<'a> {
    pub db: &'a dyn HirDatabase,
    pub resolver: &'a Resolver,
    icx: InferenceContext<'a>,
    in_binders: DebruijnIndex,
    type_param_mode: TypeParamMode,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum TypeParamMode {
    Placeholder,
    Variable,
}

impl<'a> Ctx<'a> {
    pub(crate) fn new(db: &'a dyn HirDatabase, resolver: &'a Resolver, icx: InferenceContext<'a>) -> Self {
        Self {
            db,
            resolver,
            icx,
            in_binders: DebruijnIndex::INNER,
            type_param_mode: TypeParamMode::Placeholder,
        }
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

    pub fn with_type_param_node(self, type_param_mode: TypeParamMode) -> Self {
        Self {
            type_param_mode,
            ..self
        }
    }

    pub fn lower_ty(&mut self, ty: TypeRefId) -> Ty {
        self.lower_ty_ext(ty).0
    }

    pub fn lower_ty_ext(&mut self, ty: TypeRefId) -> (Ty, Option<TypeNs>) {
        let mut res = None;
        let ty = match ty.lookup(self.db.upcast()) {
            | TypeRef::Error => TyKind::Error.intern(self.db),
            | TypeRef::Placeholder => TyKind::Error.intern(self.db),
            | TypeRef::Path(path) => return self.lower_path(&path, ty),
            | TypeRef::Tuple(tys) => {
                let tys = tys.iter().map(|&t| self.lower_ty(t));

                TyKind::Tuple(tys.collect()).intern(self.db)
            },
            | TypeRef::Ptr(to, len) => {
                let to = self.lower_ty(to);
                let lib = self.resolver.lib().unwrap();

                self.check_kind_type(to);

                match len {
                    | PtrLen::Single => {
                        let ptr_ty = self.db.lang_item(lib, "ptr-type".into()).unwrap();
                        let ptr_ty = ptr_ty.as_type_ctor().unwrap();
                        let ptr_ty = self.db.type_for_ctor(ptr_ty);

                        TyKind::App(ptr_ty, to).intern(self.db)
                    },
                    | PtrLen::Multiple(None) => {
                        let ptr_ty = self.db.lang_item(lib, "ptrb-type".into()).unwrap();
                        let ptr_ty = ptr_ty.as_type_ctor().unwrap();
                        let ptr_ty = self.db.type_for_ctor(ptr_ty);

                        TyKind::App(ptr_ty, to).intern(self.db)
                    },
                    | PtrLen::Multiple(Some(sentinel)) => {
                        let ptr_ty = self.db.lang_item(lib, "ptrbs-type".into()).unwrap();
                        let ptr_ty = ptr_ty.as_type_ctor().unwrap();
                        let ptr_ty = self.db.type_for_ctor(ptr_ty);
                        let base = TyKind::App(ptr_ty, to).intern(self.db);
                        let sentinel = TyKind::Figure(sentinel.0).intern(self.db);

                        TyKind::App(base, sentinel).intern(self.db)
                    },
                }
            },
            | TypeRef::App(base, arg) => {
                let base = self.lower_ty(base);
                let arg = self.lower_ty(arg);

                self.check_kind_for_app(base, arg);

                TyKind::App(base, arg).intern(self.db)
            },
            | TypeRef::Forall(vars, inner) => {
                let inner = self.with_shifted_in(DebruijnIndex::ONE, |ctx| ctx.lower_ty(inner));

                vars.iter().fold(inner, |inner, var| {
                    let var = var.lookup(self.db.upcast());
                    let kind = var
                        .kind
                        .map(|k| self.lower_ty(k))
                        .unwrap_or_else(|| TyKind::Error.intern(self.db));

                    TyKind::ForAll(kind, inner).intern(self.db)
                })
            },
            | ty => unimplemented!("{:?}", ty),
        };

        (ty, res)
    }

    pub(crate) fn lower_path(&self, path: &Path, type_ref: TypeRefId) -> (Ty, Option<TypeNs>) {
        let (resolution, remaining) = match self.resolver.resolve_type(self.db.upcast(), path) {
            | Some(it) => it,
            | None => return (TyKind::Error.intern(self.db), None),
        };

        self.lower_partly_resolved_path(resolution, remaining.unwrap_or(0), type_ref)
    }

    pub(crate) fn lower_partly_resolved_path(
        &self,
        resolution: TypeNs,
        remaining: usize,
        type_ref: TypeRefId,
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
            | TypeNs::TypeAlias(id) => self.db.type_for_alias(id),
            | TypeNs::TypeCtor(id) => self.db.type_for_ctor(id),
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

pub(crate) fn type_for_alias(db: &dyn HirDatabase, id: TypeAliasId) -> Ty {
    let loc = id.lookup(db.upcast());
    let item_tree = db.item_tree(loc.id.file_id);
    let resolver = id.resolver(db.upcast());
    let data = &item_tree[loc.id.value];
    let icx = InferenceContext::new(db, id.into());
    let mut ctx = Ctx::new(db, &resolver, icx);
    let var_kinds = data
        .vars
        .iter()
        .enumerate()
        .rev()
        .map(|(i, &var)| {
            let data = var.lookup(db.upcast());
            let kind = data.kind.map(|k| ctx.lower_ty(k)).unwrap_or_else(|| ctx.fresh_kind());
            let var = TypeVar::new(DebruijnIndex::new(i as u32));

            ctx.set_var_kind(var, kind);
            kind
        })
        .collect::<Vec<_>>();

    let mut ty = ctx.with_shifted_in(DebruijnIndex::new(data.vars.len() as u32), |ctx| {
        ctx.lower_ty(data.alias)
    });

    for kind in var_kinds {
        ty = TyKind::ForAll(kind, ty).intern(db);
    }

    ctx.subst_type(ty)
}

pub(crate) fn type_for_alias_recover(db: &dyn HirDatabase, _cycle: &[String], id: &TypeAliasId) -> Ty {
    unimplemented!();
}

pub(crate) fn type_for_ctor(db: &dyn HirDatabase, id: TypeCtorId) -> Ty {
    let loc = id.lookup(db.upcast());
    let item_tree = db.item_tree(loc.id.file_id);
    let data = &item_tree[loc.id.value];
    let mut ty = TyKind::Ctor(id).intern(db);

    for (i, _) in data.vars.iter().enumerate().rev() {
        let index = DebruijnIndex::new(i as u32);
        let arg = TypeVar::new(index).to_ty(db);

        ty = TyKind::App(ty, arg).intern(db);
    }

    let resolver = id.resolver(db.upcast());
    let icx = InferenceContext::new(db, id.into());
    let mut ctx = Ctx::new(db, &resolver, icx);

    for var in data.vars.iter().rev() {
        let var = var.lookup(db.upcast());
        let kind = var.kind.map(|k| ctx.lower_ty(k)).unwrap_or_else(|| ctx.fresh_kind());

        ty = TyKind::ForAll(kind, ty).intern(db);
    }

    ctx.subst_type(ty)
}

pub(crate) fn type_for_ctor_recover(db: &dyn HirDatabase, _cycle: &[String], id: &TypeCtorId) -> Ty {
    unimplemented!();
}

pub(crate) fn kind_for_ctor(db: &dyn HirDatabase, id: TypeCtorId) -> Ty {
    let loc = id.lookup(db.upcast());
    let item_tree = db.item_tree(loc.id.file_id);
    let data = &item_tree[loc.id.value];
    let mut icx = InferenceContext::new(db, id.into());

    if let Some(kind) = data.kind {
        let resolver = id.resolver(db.upcast());
        let mut ctx = Ctx::new(db, &resolver, icx);

        ctx.lower_ty(kind)
    } else {
        let ty = db.type_for_ctor(id);

        icx.infer_kind(ty)
    }
}
