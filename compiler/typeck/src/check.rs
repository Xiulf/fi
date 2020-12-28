use crate::ctx::*;
use crate::error::*;
use crate::ty::*;
use hir::ir;

impl<'db> Ctx<'db> {
    crate fn check_body(&mut self, span: ir::Span, body: &ir::Body, ty: Ty) -> Result<()> {
        let infer = self.infer_body(span, body)?;

        self.subsumes(infer, ty).map(|_| ())
    }

    crate fn check_pat(&mut self, pat: &ir::Pat, ty: Ty) -> Result<()> {
        match (&pat.kind, &*ty) {
            (_, Type::Unknown(_)) => {
                let infer = self.infer_pat(pat)?;
                let infer = self.instantiate(infer);

                self.unify_types(infer, ty)
            }
            (ir::PatKind::Error, _) => self.unify_types(ty, Ty::error(pat.span)),
            (_, _) => {
                let infer = self.infer_pat(pat)?;

                self.subsumes(infer, ty).map(|_| ())
            }
        }
    }

    crate fn check_expr(&mut self, expr: &ir::Expr, ty: Ty) -> Result<()> {
        match (&expr.kind, &*ty) {
            (_, Type::Unknown(_)) => {
                let infer = self.infer_expr(expr)?;
                let infer = self.instantiate(infer);

                self.unify_types(infer, ty)
            }
            (ir::ExprKind::Error, _) => self.unify_types(ty, Ty::error(expr.span)),
            (_, _) => {
                let infer = self.infer_expr(expr)?;
                let elaborate = self.subsumes(infer, ty)?;

                elaborate(expr);

                Ok(())
            }
        }
    }
}
