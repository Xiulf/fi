use crate::ctx::*;
use crate::error::*;
use crate::ty::*;
use hir::ir;

impl<'db> Ctx<'db> {
    crate fn check_body(&mut self, span: ir::Span, body: &ir::Body, ty: Ty) -> Result<()> {
        let infer = self.infer_body(span, body)?;

        self.subsumes(ty, infer).map(|_| ())
    }

    crate fn check_pat(&mut self, pat: &ir::Pat, ty: Ty) -> Result<()> {
        match (&pat.kind, &*ty) {
            (_, Type::Unknown(_)) => {
                let infer = self.infer_pat(pat)?;
                let infer = self.instantiate(infer);

                self.unify_types(infer, ty)
            }
            (ir::PatKind::Error, _) => self.unify_types(ty, Ty::error(pat.span, self.file)),
            (_, _) => {
                let infer = self.infer_pat(pat)?;

                self.subsumes(infer, ty).map(|_| ())
            }
        }
    }

    crate fn check_expr(&mut self, expr: &ir::Expr, ty: Ty) -> Result<()> {
        match (&expr.kind, &*ty) {
            (_, Type::ForAll(vars, t1, _)) => {
                let scope = self.new_skolem_scope();
                let skolems = (0..vars.len())
                    .map(|_| self.new_skolem_constant())
                    .collect();
                let sk = self.skolemize(t1.span(), t1.file(), vars, skolems, t1.clone(), scope);

                self.check_expr(expr, sk)
            }
            (_, Type::Unknown(_)) => {
                let infer = self.infer_expr(expr)?;
                let infer = self.instantiate(infer);

                self.unify_types(infer, ty)
            }
            (ir::ExprKind::Error, _) => self.unify_types(ty, Ty::error(expr.span, self.file)),
            (_, _) => {
                let infer = self.infer_expr(expr)?;
                let elaborate = self.subsumes(infer, ty)?;

                elaborate(expr);

                Ok(())
            }
        }
    }

    crate fn check_binders(&mut self, tys: List<Ty>, ret: Ty, arms: &[ir::CaseArm]) -> Result<()> {
        if arms.is_empty() {
            Ok(())
        } else {
            arms.iter()
                .map(|arm| {
                    for (pat, ty) in arm.pats.iter().zip(&tys) {
                        self.check_pat(pat, ty)?;
                    }

                    self.check_guarded(&arm.val, ret.clone())
                })
                .collect()
        }
    }

    crate fn check_guarded(&mut self, guarded: &ir::Guarded, ty: Ty) -> Result<()> {
        match guarded {
            ir::Guarded::Unconditional(expr) => self.check_expr(expr, ty),
            ir::Guarded::Guarded(exprs) => {
                for expr in exprs {
                    let bool_ty = self.db.lang_items().bool();
                    let bool_ty = Ty::ctor(expr.guard.span, self.file, bool_ty.owner);

                    self.check_expr(&expr.guard, bool_ty)?;
                    self.check_expr(&expr.val, ty.clone())?;
                }

                Ok(())
            }
        }
    }
}
