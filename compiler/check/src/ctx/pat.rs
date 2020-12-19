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
                let mut ctor_ty = self.db.typecheck(*ctor).ty.monomorphize(self.db);

                while let Type::App(ty, _, _) = &*ctor_ty {
                    ctor_ty = ty.clone();
                }

                if let Type::Func(params, ret) = &*ctor_ty {
                    assert_eq!(params.len(), pats.len());
                    for (param, pat) in params.into_iter().zip(pats) {
                        let pat_ty = self.infer_pat(pat);

                        self.constrain().equal(pat_ty, pat.span, param, pat.span);
                    }

                    ret.clone()
                } else {
                    ctor_ty
                }
            }
            ir::PatKind::Record { fields } => {
                let tail = Ty::infer(self.db.new_infer_var());
                let fields = fields
                    .iter()
                    .map(|f| Field {
                        span: f.span,
                        name: f.name.symbol,
                        ty: self.infer_pat(&f.val),
                    })
                    .collect();

                Ty::record(fields, Some(tail))
            }
            _ => unimplemented!(),
        };

        self.tys.insert(pat.id, (ty.clone(), pat.span));
        ty
    }
}
