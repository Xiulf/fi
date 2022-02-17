use super::{InferenceContext, InferenceDiagnostic};
use crate::ty::*;
use hir_def::id::TypeVarOwner;
use hir_def::type_ref::LocalTypeRefId;

impl InferenceContext<'_> {
    /// Infer the kind of the type
    pub fn infer_kind(&mut self, ty: Ty, origin: LocalTypeRefId) -> Ty {
        match ty.lookup(self.db) {
            | TyKind::Error => ty,
            | TyKind::Unknown(u) => {
                let kind = self.subst.unsolved(u).1;

                self.subst_type(kind)
            },
            | TyKind::Skolem(_, kind) => kind,
            | TyKind::TypeVar(var) => {
                let idx = self.var_kinds.len() - var.debruijn().depth() as usize - 1;

                self.var_kinds[idx][var.idx() as usize]
            },
            | TyKind::Figure(_) => self.lang_type("figure-kind"),
            | TyKind::Symbol(_) => self.lang_type("symbol-kind"),
            | TyKind::Row(fields, tail) => {
                let elem_kind = self.fresh_kind();
                let row_kind = self.lang_type("row-kind");
                let kind = TyKind::App(row_kind, [elem_kind].into()).intern(self.db);

                for field in fields.iter() {
                    self.check_kind(field.ty, elem_kind, origin);
                }

                if let Some(tail) = tail {
                    self.check_kind(tail, kind, origin);
                }

                kind
            },
            | TyKind::Ctor(id) => {
                if TypeVarOwner::TypedDefId(id.into()) == self.owner {
                    self.result.self_type
                } else {
                    self.db.kind_for_ctor(id).ty
                }
            },
            | TyKind::App(base, args) => self.check_kind_for_app(base, &args, origin),
            | TyKind::Tuple(tys) => {
                let type_kind = self.lang_type("type-kind");

                for &ty in tys.iter() {
                    self.check_kind(ty, type_kind, origin);
                }

                type_kind
            },
            | TyKind::Func(args, ret) => {
                let type_kind = self.lang_type("type-kind");

                for &ty in args.iter() {
                    self.check_kind(ty, type_kind, origin);
                }

                self.check_kind(ret, type_kind, origin);
                type_kind
            },
            | TyKind::Ctnt(_, ty) => self.infer_kind(ty, origin),
            | TyKind::ForAll(kinds, inner) => {
                self.push_var_kind(kinds.clone());

                let inner_kind = self.infer_kind(inner, origin);

                self.pop_var_kind();
                self.fn_type(kinds, inner_kind)
            },
        }
    }

    /// Check that `ty` has kind `kind`
    pub fn check_kind(&mut self, ty: Ty, kind: Ty, origin: LocalTypeRefId) {
        let ty_kind = self.infer_kind(ty, origin);

        if !self.unify_types(ty_kind, kind) {
            self.report(InferenceDiagnostic::MismatchedKind {
                id: origin,
                expected: kind,
                found: ty_kind,
            });
        }
    }

    /// Check that `ty` has kind `Type`
    pub fn check_kind_type(&mut self, ty: Ty, origin: LocalTypeRefId) {
        let type_kind = self.lang_type("type-kind");

        self.check_kind(ty, type_kind, origin);
    }

    /// Check that base has kind `kind_of(arg) -> ?`
    pub fn check_kind_for_app(&mut self, base: Ty, args: &[Ty], origin: LocalTypeRefId) -> Ty {
        let base_kind = self.infer_kind(base, origin);
        let arg_kinds = args.iter().map(|&a| self.infer_kind(a, origin)).collect();
        let ret_kind = self.fresh_kind();
        let fun_kind = self.fn_type(arg_kinds, ret_kind);

        if !self.unify_types(base_kind, fun_kind) {
            self.report(InferenceDiagnostic::MismatchedKind {
                id: origin,
                expected: fun_kind,
                found: base_kind,
            });
        }

        ret_kind
    }
}
