use super::*;

impl<'db> Ctx<'db> {
    pub(crate) fn infer_pat(&mut self, pat: &ir::Pat) -> Ty {
        let ty = match &pat.kind {
            ir::PatKind::Error => Ty::error(),
            ir::PatKind::Wildcard => Ty::infer(self.db.new_infer_var()),
            ir::PatKind::Bind { sub, .. } => {
                if let Some(sub) = sub {
                    self.infer_pat(sub)
                } else {
                    Ty::infer(self.db.new_infer_var())
                }
            }
            ir::PatKind::Ctor { ctor, pats } => {
                let ctor_ty = self.db.typecheck(*ctor).ty.monomorphize(self.db);

                if let Type::Func(params, ret) = &*ctor_ty {
                    for (param, pat) in params.into_iter().zip(pats) {
                        let pat_ty = self.infer_pat(pat);

                        self.constrain(Constraint::Equal(pat_ty, pat.span, param, pat.span));
                    }

                    ret.clone()
                } else {
                    ctor_ty
                }
            }
            _ => unimplemented!(),
        };

        self.tys.insert(pat.id, ty.clone());
        ty
    }
}
