use crate::ty::*;
use hir::ir::Ident;

pub type Result<T> = std::result::Result<T, TypeError>;

pub enum TypeError {
    Internal(String),
    CyclicType(Ty),
    Mismatch(Ty, Ty),
    KindMismatch(Ty, Ty),
    HoleType(Ident, Ty),
}

impl TypeError {
    pub fn report(self, db: &dyn crate::TypeDatabase) {
        use crate::display::Typed;
        use diagnostics::Label;

        let diags = db.to_diag_db();

        match self {
            TypeError::Internal(err) => diags.error(err).finish(),
            TypeError::CyclicType(ty) => diags
                .error(format!("cyclic type {}", Typed(db, &(), &ty)))
                .with_label(Label::primary(ty.file(), ty.span()))
                .finish(),
            TypeError::Mismatch(a, b) => diags
                .error(format!(
                    "mismatched types {} != {}",
                    Typed(db, &(), &a),
                    Typed(db, &(), &b)
                ))
                .with_label(Label::primary(a.file(), a.span()))
                .with_label(Label::secondary(b.file(), b.span()))
                .finish(),
            TypeError::KindMismatch(a, b) => diags
                .error(format!(
                    "mismatched kinds {} != {}",
                    Typed(db, &(), &a),
                    Typed(db, &(), &b)
                ))
                .with_label(Label::primary(a.file(), a.span()))
                .with_label(Label::secondary(b.file(), b.span()))
                .finish(),
            TypeError::HoleType(name, ty) => diags
                .error(format!(
                    "type hole {} was found to be of type {}",
                    name,
                    Typed(db, &(), &ty)
                ))
                .with_label(Label::primary(ty.file(), ty.span()))
                .finish(),
        }
    }
}
