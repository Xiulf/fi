use super::{InferenceContext, InferenceDiagnostic};
use crate::info::{ToInfo, TyId, TyInfo, TySource};
use hir_def::id::TypeVarOwner;

impl InferenceContext<'_> {
    /// Infer the kind of the type
    pub fn infer_kind(&mut self, ty: TyId) -> TyId {
        let ty = self.subst_type(ty);
        let src = self.types.source(ty);

        let kind = match self.types[ty].clone() {
            | TyInfo::Error => ty,
            | TyInfo::Unknown(u) => {
                let kind = self.subst.unsolved(u).1;

                self.subst_type(kind)
            },
            | TyInfo::Skolem(_, kind) => kind,
            | TyInfo::TypeVar(var) => self.type_vars.var_kinds(var.scope())[var.idx() as usize],
            | TyInfo::Figure(_) => self.lang_type("figure-kind", src),
            | TyInfo::Symbol(_) => self.lang_type("symbol-kind", src),
            | TyInfo::Row(fields, tail) => {
                let elem_kind = self.fresh_type(src);
                let row_kind = self.lang_type("row-kind", src);
                let kind = self.types.insert(TyInfo::App(row_kind, [elem_kind].into()), src);

                for field in fields.iter() {
                    self.check_kind(field.ty, elem_kind);
                }

                if let Some(tail) = tail {
                    self.check_kind(tail, kind);
                }

                kind
            },
            | TyInfo::Ctor(id) => {
                if TypeVarOwner::TypedDefId(id.into()) == self.owner {
                    self.result.self_type
                } else {
                    self.db
                        .kind_for_ctor(id)
                        .ty
                        .to_info(self.db, &mut self.types, &mut self.type_vars, src)
                }
            },
            | TyInfo::App(base, args) => self.check_kind_for_app(base, &args, src),
            | TyInfo::Tuple(tys) => {
                let type_kind = self.lang_type("type-kind", src);

                for &ty in tys.iter() {
                    self.check_kind(ty, type_kind);
                }

                type_kind
            },
            | TyInfo::Func(args, ret) => {
                let type_kind = self.lang_type("type-kind", src);

                for &ty in args.iter() {
                    self.check_kind(ty, type_kind);
                }

                self.check_kind(ret, type_kind);
                type_kind
            },
            | TyInfo::Where(_, ty) => self.infer_kind(ty),
            | TyInfo::ForAll(kinds, inner, scope) => {
                self.type_vars.push_scope(scope);

                let inner_kind = self.infer_kind(inner);

                self.type_vars.pop_scope();
                self.fn_type(kinds, inner_kind, src)
            },
        };

        kind
    }

    /// Check that `ty` has kind `kind`
    pub fn check_kind(&mut self, ty: TyId, kind: TyId) {
        let ty_kind = self.infer_kind(ty);

        if !self.unify_types(ty_kind, kind) {
            let expected_src = self.types.source(kind);
            let found_src = self.types.source(ty_kind);

            self.report(InferenceDiagnostic::MismatchedKind {
                expected: kind,
                found: ty_kind,
                expected_src,
                found_src,
            });
        }
    }

    /// Check that `ty` has kind `Type`
    pub fn check_kind_type(&mut self, ty: TyId) {
        let src = self.types.source(ty);
        let type_kind = self.lang_type("type-kind", src);

        self.check_kind(ty, type_kind);
    }

    /// Check that base has kind `kind_of(arg) -> ?`
    pub fn check_kind_for_app(&mut self, base: TyId, args: &[TyId], src: TySource) -> TyId {
        let base_src = self.types.source(base);
        let arg_kinds = args.iter().map(|&a| self.infer_kind(a)).collect();
        let ret_kind = self.fresh_type(src);
        let fun_kind = self.fn_type(arg_kinds, ret_kind, base_src);

        self.check_kind(base, fun_kind);
        ret_kind
    }
}
