use crate::ctx::Ctx;
use crate::ty::{PrimitiveType, Ty, TyKind};
use crate::unify::{UnifyBindings, UnifyResult};

impl Ctx<'_> {
    pub(crate) fn check_kind(&self, ty: Ty, kind: Ty, bindings: &mut UnifyBindings) -> UnifyResult {
        let infer = self.infer_kind_inner(ty);

        self.unify_into(infer, kind, bindings)
    }

    fn infer_kind_inner(&self, ty: Ty) -> Ty {
        let kind = match ty.kind(self.db) {
            | TyKind::Error => ty,
            | TyKind::Never => self.type_kind(),
            | TyKind::Unknown(u, _) => self.subst.unsolved[u].1,
            | TyKind::Var(_) => self.error(),
            | TyKind::Ctor(id) => crate::type_ctor_ty(self.db, *id).kind,
            | TyKind::Primitive(prim) => match prim {
                | PrimitiveType::Integer(_) => self.int_tag_kind(),
                | PrimitiveType::Float(_) => self.float_tag_kind(),
            },
            | TyKind::App(base, args) => {
                let base = self.infer_kind_inner(*base);
                self.infer_app_kind(base, args)
            },
            | TyKind::Func(_) => self.type_kind(),
        };

        kind
    }

    fn infer_app_kind(&self, base: Ty, args: &[Ty]) -> Ty {
        let base = self.resolve_type_shallow(base);

        if let TyKind::Func(f) = base.kind(self.db) {
            if args.is_empty() {
                return f.ret;
            }

            return self.infer_app_kind(f.ret, &args[1..]);
        }

        return self.error();
    }
}
