use crate::constraint::Constraint;
use crate::tcx::Tcx;
use crate::ty::*;

impl<'tcx> Tcx<'tcx> {
    pub fn infer_pat(&self, id: &hir::Id) -> Ty<'tcx> {
        let pat = &self.package.pats[id];

        match &pat.kind {
            hir::PatKind::Err => self.builtin.error,
            hir::PatKind::Wildcard => self.new_var(),
            hir::PatKind::Bind { var, inner, by_ref } => {
                let var_ty = self.type_of(var);
                let var_span = self.span_of(var);
                let ty = self.new_var();

                self.constrain(Constraint::Equal(
                    var_ty,
                    var_span,
                    if *by_ref {
                        self.intern_ty(Type::Ptr(PtrKind::Single, ty))
                    } else {
                        ty
                    },
                    pat.span,
                ));

                if let Some(inner) = inner {
                    let inner_ty = self.type_of(inner);
                    let inner_span = self.span_of(inner);

                    self.constrain(Constraint::Equal(ty, var_span, inner_ty, inner_span));
                }

                ty
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
