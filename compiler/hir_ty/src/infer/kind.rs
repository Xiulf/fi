use hir_def::id::TypeVarOwner;
use hir_def::lang_item;
use tracing::trace;

use super::{InferenceContext, InferenceDiagnostic};
use crate::info::{ToInfo, TyId, TyInfo, TySource};

impl InferenceContext<'_> {
    /// Infer the kind of the type
    pub fn infer_kind(&mut self, ty: TyId) -> TyId {
        trace!("infer_kind({:?})", ty);
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
            | TyInfo::Figure(_) => self.lang_type(lang_item::FIGURE_KIND, src),
            | TyInfo::Symbol(_) => self.lang_type(lang_item::SYMBOL_KIND, src),
            | TyInfo::Row(fields, tail) => {
                let elem_kind = self.fresh_type(src);
                let row_kind = self.lang_type(lang_item::ROW_KIND, src);
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
                    self.result.self_type.ty
                } else {
                    let lower = self.db.kind_for_ctor(id);

                    lower.ty.ty.to_info(self.db, &mut self.types, &mut self.type_vars, src)
                }
            },
            | TyInfo::Alias(id) => {
                let ty = if TypeVarOwner::TypedDefId(id.into()) == self.owner {
                    self.result.self_type.ty
                } else {
                    let lower = self.db.type_for_alias(id);

                    lower.ty.ty.to_info(self.db, &mut self.types, &mut self.type_vars, src)
                };

                if let TyInfo::ForAll(kinds, inner, _) = self.types[ty].clone() {
                    let kind = self.infer_kind(inner);

                    self.fn_type(kinds.into_vec(), kind, src)
                } else {
                    self.infer_kind(ty)
                }
            },
            | TyInfo::App(base, args) => self.check_kind_for_app(base, &args, src),
            | TyInfo::Where(_, ty) => self.infer_kind(ty),
            | TyInfo::ForAll(kinds, inner, scope) => {
                for kind in kinds.into_vec() {
                    self.check_kind_type(kind);
                }

                self.type_vars.push_scope(scope);
                self.check_kind_type(inner);
                self.type_vars.pop_scope();
                self.type_kind(src)
            },
        };

        kind
    }

    /// Check that `ty` has kind `kind`
    pub fn check_kind(&mut self, ty: TyId, kind: TyId) {
        let ty = self.subst_type(ty);

        match self.types[ty] {
            | TyInfo::Ctor(id) if id == self.lang_ctor(lang_item::UNIT_TYPE) => {
                if let Some(_) = kind.match_ctor(&self.types, self.lang_ctor(lang_item::ROW_KIND)) {
                    return;
                }
            },
            | _ => {},
        }

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
        let type_kind = self.type_kind(src);

        self.check_kind(ty, type_kind);
    }

    /// Check that base has kind `kind_of(arg) -> ?`
    pub fn check_kind_for_app(&mut self, base: TyId, args: &[TyId], src: TySource) -> TyId {
        let base_src = self.types.source(base);
        let base_kind = self.infer_kind(base);
        let base_kind = self.subst_type(base_kind);
        let (params, base_kind) = self.fn_args(base_kind, args.len());
        let param_count = params.len();

        for (param, &arg) in params.into_vec().into_iter().zip(&args[..param_count]) {
            self.check_kind(arg, param);
        }

        let arg_kinds = args[param_count..]
            .iter()
            .map(|&a| self.infer_kind(a))
            .collect::<Vec<_>>();
        let ret_kind = self.fresh_type(src);
        let fun_kind = self.fn_type(arg_kinds, ret_kind, base_src);

        if !self.unify_types(base_kind, fun_kind) {
            self.report(InferenceDiagnostic::MismatchedKind {
                expected: fun_kind,
                found: base_kind,
                expected_src: base_src,
                found_src: base_src,
            });
        }

        ret_kind
    }
}
