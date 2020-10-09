use crate::constraint::Constraint;
use crate::tcx::Tcx;
use crate::ty::*;

impl<'tcx> Tcx<'tcx> {
    pub fn infer_pat(&self, id: &hir::Id) -> Ty<'tcx> {
        let pat = &self.package.pats[id];

        match &pat.kind {
            hir::PatKind::Err => self.builtin.error,
            hir::PatKind::Wildcard => self.new_var(),
            hir::PatKind::Bind { var, inner } => {
                let var_ty = self.type_of(var);

                if let Some(inner) = inner {
                    let inner_ty = self.type_of(inner);
                    let inner_span = self.span_of(inner);
                    let var_span = self.span_of(var);

                    self.constrain(Constraint::Equal(var_ty, var_span, inner_ty, inner_span));
                }

                var_ty
            }
            hir::PatKind::Ctor { id, pats } => {
                let ctor_ty = self.type_of(id).mono(self, Vec::new());
                let res = if let Some((_, params, ret)) = ctor_ty.func() {
                    if pats.len() != params.len() {
                        // TODO: log error
                    }

                    for (param, child) in params.iter().zip(pats) {
                        let child_ty = self.type_of(child);
                        let child_span = self.span_of(child);

                        self.constrain(Constraint::Equal(
                            child_ty, child_span, param.ty, param.span,
                        ));
                    }

                    ret
                } else {
                    if !pats.is_empty() {
                        // TODO: log error
                    }

                    ctor_ty
                };

                res
            }
        }
    }
}
