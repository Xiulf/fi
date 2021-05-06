use crate::db::HirDatabase;
use crate::ty::*;
use hir_def::id::{Lookup, TypeAliasId, TypeCtorId};
use hir_def::name::Name;
use hir_def::path::Path;
use hir_def::resolver::HasResolver;
use hir_def::resolver::{Resolver, TypeNs};
use hir_def::type_ref::{PtrLen, TypeRef, TypeRefId};

pub struct Ctx<'a> {
    pub db: &'a dyn HirDatabase,
    pub resolver: &'a Resolver,
    in_binders: DebruijnIndex,
    type_param_mode: TypeParamMode,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TypeParamMode {
    Placeholder,
    Variable,
}

impl<'a> Ctx<'a> {
    pub fn new(db: &'a dyn HirDatabase, resolver: &'a Resolver) -> Self {
        Self {
            db,
            resolver,
            in_binders: DebruijnIndex::INNER,
            type_param_mode: TypeParamMode::Placeholder,
        }
    }

    pub fn with_debruijn<T>(&self, debruijn: DebruijnIndex, f: impl FnOnce(&Ctx) -> T) -> T {
        let new_ctx = Self {
            in_binders: debruijn,
            ..*self
        };

        f(&new_ctx)
    }

    pub fn with_shifted_in<T>(&self, debruijn: DebruijnIndex, f: impl FnOnce(&Ctx) -> T) -> T {
        self.with_debruijn(self.in_binders.shifted_in_from(debruijn), f)
    }

    pub fn with_type_param_node(self, type_param_mode: TypeParamMode) -> Self {
        Self {
            type_param_mode,
            ..self
        }
    }

    pub fn lower_ty(&self, ty: TypeRefId) -> Ty {
        self.lower_ty_ext(ty).0
    }

    pub fn lower_ty_ext(&self, ty: TypeRefId) -> (Ty, Option<TypeNs>) {
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

                TyKind::App(base, arg).intern(self.db)
            },
            | TypeRef::Forall(vars, inner) => {
                let inner = self.with_shifted_in(DebruijnIndex::ONE, |ctx| ctx.lower_ty(inner));

                vars.iter()
                    .fold(inner, |inner, _| TyKind::ForAll(inner).intern(self.db))
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

                TypeVar::new(debruijn, 0).to_ty(self.db)
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

pub(crate) fn type_for_alias(db: &dyn HirDatabase, id: TypeAliasId) -> Ty {
    let loc = id.lookup(db.upcast());
    let item_tree = db.item_tree(loc.id.file_id);
    let resolver = id.resolver(db.upcast());
    let data = &item_tree[loc.id.value];
    let ctx = Ctx::new(db, &resolver);
    let mut ty = ctx.with_shifted_in(DebruijnIndex::new(data.vars.len() as u32), |ctx| {
        ctx.lower_ty(data.alias)
    });

    for _ in data.vars.iter().rev() {
        ty = TyKind::ForAll(ty).intern(db);
    }

    ty
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
        let arg = TypeVar::new(index, 0).to_ty(db);

        ty = TyKind::App(ty, arg).intern(db);
    }

    for _ in data.vars.iter().rev() {
        ty = TyKind::ForAll(ty).intern(db);
    }

    ty
}

pub(crate) fn type_for_ctor_recover(db: &dyn HirDatabase, _cycle: &[String], id: &TypeCtorId) -> Ty {
    unimplemented!();
}
