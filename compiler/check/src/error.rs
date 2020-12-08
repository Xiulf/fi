use crate::subst::Subst;
use crate::ty::Ty;
use hir::ir::Span;

pub type UnifyResult = Result<Subst, UnifyError>;

pub enum UnifyError {
    Mismatch,
}

pub enum TypeError {
    Mismatched {
        a_ty: Ty,
        a_span: Span,
        b_ty: Ty,
        b_span: Span,
    },
}

impl TypeError {
    pub fn report(self, file: source::FileId, db: &dyn crate::TypeDatabase) {
        use diagnostics::Label;

        match self {
            TypeError::Mismatched {
                a_ty,
                a_span,
                b_ty,
                b_span,
            } => db
                .to_diag_db()
                .error(format!(
                    "mismatched types: `{}` != `{}`",
                    a_ty.display(db),
                    b_ty.display(db)
                ))
                .with_label(Label::primary(file, a_span))
                .with_label(Label::secondary(file, b_span))
                .finish(),
        }
    }
}
