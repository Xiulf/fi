use super::InferenceContext;
use crate::display::HirDisplay;
use crate::ty::*;

impl InferenceContext<'_> {
    /// Infer the kind of the type
    pub fn infer_kind(&mut self, ty: Ty) -> Ty {
        match ty.lookup(self.db) {
            | TyKind::Error => ty,
            | TyKind::Unknown(u) => {
                let kind = self.subst.unsolved(u).1;

                self.subst_type(kind)
            },
            | TyKind::TypeVar(var) => self.var_kinds[&var],
            | TyKind::Ctor(id) => self.db.kind_for_ctor(id),
            | TyKind::App(base, arg) => self.check_kind_for_app(base, arg),
            | _ => unimplemented!("{}", ty.display(self.db)),
        }
    }

    /// Check that `ty` has kind `kind`
    pub fn check_kind(&mut self, ty: Ty, kind: Ty) {
        let ty_kind = self.infer_kind(ty);

        self.unify_types(ty_kind, kind);
    }

    /// Check that `ty` has kind `Type`
    pub fn check_kind_type(&mut self, ty: Ty) {
        let type_kind = self.lang_type("type-kind");

        self.check_kind(ty, type_kind);
    }

    /// Check that base has kind `kind_of(arg) -> ?`
    pub fn check_kind_for_app(&mut self, base: Ty, arg: Ty) -> Ty {
        let base_kind = self.infer_kind(base);
        let arg_kind = self.infer_kind(arg);
        let ret_kind = self.fresh_kind();
        let fun_kind = self.fn_type(arg_kind, ret_kind);

        self.unify_types(base_kind, fun_kind);
        ret_kind
    }
}
