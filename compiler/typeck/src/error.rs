use crate::ty::*;
use hir::ir::{Ident, Span};

pub type Result<T> = std::result::Result<T, TypeError>;

pub enum TypeError {
    Internal(String),
    CyclicType(Ty),
    Mismatch(Ty, Ty),
    KindMismatch(Ty, Ty),
    HoleType(Ident, Ty),
    NoImpl(Ctnt),
    IncorrectArity(source::FileId, Span, Ty),
}

impl TypeError {
    pub fn report(self, db: &dyn crate::TypeDatabase) {
        use crate::display::Typed;
        use diagnostics::Label;

        let diags = db.to_diag_db();

        match self {
            | TypeError::Internal(err) => diags.error(err).finish(),
            | TypeError::CyclicType(ty) => diags
                .error(format!("cyclic type `{}`", Typed(db, &(), &ty)))
                .with_label(Label::primary(ty.file(), ty.span()))
                .finish(),
            | TypeError::Mismatch(a, b) => diags
                .error(format!("mismatched types `{}` != `{}`", Typed(db, &(), &a), Typed(db, &(), &b)))
                .with_label(Label::primary(a.file(), a.span()))
                .with_label(Label::secondary(b.file(), b.span()).with_message(format!("type `{}` specified here", Typed(db, &(), &b))))
                .finish(),
            | TypeError::KindMismatch(a, b) => diags
                .error(format!("mismatched kinds `{}` != `{}`", Typed(db, &(), &a), Typed(db, &(), &b)))
                .with_label(Label::primary(a.file(), a.span()))
                .with_label(Label::secondary(b.file(), b.span()))
                .finish(),
            | TypeError::HoleType(name, ty) => diags
                .error(format!("hole '?{}' was found to be of type `{}`", name, Typed(db, &(), &ty)))
                .with_label(Label::primary(ty.file(), ty.span()))
                .finish(),
            | TypeError::NoImpl(ctnt) => diags
                .error(format!("no instance found for `{}`", Typed(db, &(), &ctnt)))
                .with_label(Label::primary(ctnt.file, ctnt.span))
                .finish(),
            | TypeError::IncorrectArity(file, span, ty) => diags
                .error(format!("incorrect arity"))
                .with_label(Label::primary(file, span))
                .with_label(Label::secondary(ty.file(), ty.span()))
                .finish(),
        }
    }
}
