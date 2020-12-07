use crate::ctx::Ctx;
use crate::error::TypeError;
use crate::ty::*;
use crate::unify::UnifyError;
use hir::ir::Span;
use std::fmt;

pub struct Constrain<'ctx, 'db> {
    pub(crate) ctx: &'ctx mut Ctx<'db>,
}

pub enum Constraint {
    Equal(Ty, Ty),
}

impl<'ctx, 'db> Constrain<'ctx, 'db> {
    pub fn new(ctx: &'ctx mut Ctx<'db>) -> Self {
        Constrain { ctx }
    }

    pub fn equal(&mut self, a: Ty, a_span: Span, b: Ty, b_span: Span) {
        let result = self.unify_one(Constraint::Equal(a.clone(), b.clone()));

        match result {
            Ok(subst) => {
                subst.apply_ty(&a);
                subst.apply_ty(&b);

                for (_, (ty, _)) in &mut self.ctx.tys {
                    subst.apply_ty(ty);
                }
            }
            Err(UnifyError::Mismatch) => {
                TypeError::Mismatched {
                    a_ty: a,
                    a_span,
                    b_ty: b,
                    b_span,
                }
                .report(self.ctx.file, self.ctx.db);
            }
        }
    }
}

pub struct CsDisplay<'a>(&'a Constraint, &'a dyn crate::TypeDatabase);

impl Constraint {
    pub fn display<'a>(&'a self, db: &'a dyn crate::TypeDatabase) -> CsDisplay<'a> {
        CsDisplay(self, db)
    }
}

impl fmt::Display for CsDisplay<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.0 {
            Constraint::Equal(a, b) => write!(f, "{} == {}", a.display(self.1), b.display(self.1)),
        }
    }
}
