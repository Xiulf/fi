use super::*;

impl<'db> Ctx<'db> {
    pub(crate) fn infer_pat(&mut self, pat: &ir::Pat) -> Ty {
        let ty = match &pat.kind {
            ir::PatKind::Error => Ty::error(),
            ir::PatKind::Bind { sub, .. } => {
                if let Some(sub) = sub {
                    self.infer_pat(sub)
                } else {
                    Ty::infer(self.db.new_infer_var())
                }
            }
            ir::PatKind::Ctor { ctor, pats } => {
                let ctor_ty = self.db.typecheck(*ctor).ty.clone();
            }
            _ => unimplemented!(),
        };

        self.tys.insert(pat.id, ty.clone());
        ty
    }
}
