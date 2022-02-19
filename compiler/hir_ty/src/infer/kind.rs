use super::{InferenceContext, InferenceDiagnostic};
use crate::info::{ToInfo, TyId, TyInfo};
use hir_def::id::TypeVarOwner;
use hir_def::type_ref::LocalTypeRefId;

impl InferenceContext<'_> {
    /// Infer the kind of the type
    pub fn infer_kind(&mut self, ty: TyId, origin: LocalTypeRefId) -> TyId {
        let src = self.source(origin);

        match self.types[ty].clone() {
            | TyInfo::Error => ty,
            | TyInfo::Unknown(u) => {
                let kind = self.subst.unsolved(u).1;

                self.subst_type(kind)
            },
            | TyInfo::Skolem(_, kind) => kind,
            | TyInfo::TypeVar(var) => {
                let idx = self.var_kinds.len() - var.debruijn().depth() as usize - 1;

                self.var_kinds[idx][var.idx() as usize]
            },
            | TyInfo::Figure(_) => self.lang_type("figure-kind", src),
            | TyInfo::Symbol(_) => self.lang_type("symbol-kind", src),
            | TyInfo::Row(fields, tail) => {
                let elem_kind = self.fresh_kind(src);
                let row_kind = self.lang_type("row-kind", src);
                let kind = self.types.insert(TyInfo::App(row_kind, [elem_kind].into()), src);

                for field in fields.iter() {
                    self.check_kind(field.ty, elem_kind, origin);
                }

                if let Some(tail) = tail {
                    self.check_kind(tail, kind, origin);
                }

                kind
            },
            | TyInfo::Ctor(id) => {
                if TypeVarOwner::TypedDefId(id.into()) == self.owner {
                    self.result.self_type
                } else {
                    self.db.kind_for_ctor(id).ty.to_info(self.db, &mut self.types, src)
                }
            },
            | TyInfo::App(base, args) => self.check_kind_for_app(base, &args, origin),
            | TyInfo::Tuple(tys) => {
                let type_kind = self.lang_type("type-kind", src);

                for &ty in tys.iter() {
                    self.check_kind(ty, type_kind, origin);
                }

                type_kind
            },
            | TyInfo::Func(args, ret) => {
                let type_kind = self.lang_type("type-kind", src);

                for &ty in args.iter() {
                    self.check_kind(ty, type_kind, origin);
                }

                self.check_kind(ret, type_kind, origin);
                type_kind
            },
            | TyInfo::Ctnt(_, ty) => self.infer_kind(ty, origin),
            | TyInfo::ForAll(kinds, inner) => {
                self.push_var_kind(kinds.clone());

                let inner_kind = self.infer_kind(inner, origin);

                self.pop_var_kind();
                self.fn_type(kinds, inner_kind, src)
            },
        }
    }

    /// Check that `ty` has kind `kind`
    pub fn check_kind(&mut self, ty: TyId, kind: TyId, origin: LocalTypeRefId) {
        let ty_kind = self.infer_kind(ty, origin);

        if !self.unify_types(ty_kind, kind) {
            // self.report(InferenceDiagnostic::MismatchedKind {
            //     id: origin,
            //     expected: kind,
            //     found: ty_kind,
            // });
        }
    }

    /// Check that `ty` has kind `Type`
    pub fn check_kind_type(&mut self, ty: TyId, origin: LocalTypeRefId) {
        let src = self.source(origin);
        let type_kind = self.lang_type("type-kind", src);

        self.check_kind(ty, type_kind, origin);
    }

    /// Check that base has kind `kind_of(arg) -> ?`
    pub fn check_kind_for_app(&mut self, base: TyId, args: &[TyId], origin: LocalTypeRefId) -> TyId {
        let src = self.source(origin);
        let base_kind = self.infer_kind(base, origin);
        let arg_kinds = args.iter().map(|&a| self.infer_kind(a, origin)).collect();
        let ret_kind = self.fresh_kind(src);
        let fun_kind = self.fn_type(arg_kinds, ret_kind, src);

        if !self.unify_types(base_kind, fun_kind) {
            // self.report(InferenceDiagnostic::MismatchedKind {
            //     id: origin,
            //     expected: fun_kind,
            //     found: base_kind,
            // });
        }

        ret_kind
    }
}
