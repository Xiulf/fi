use crate::ty::*;
use hir::ir::Span;
use std::fmt;

pub enum Constraint {
    Equal(Ty, Span, Ty, Span),
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
            Constraint::Equal(a, _, b, _) => {
                write!(f, "{} == {}", a.display(self.1), b.display(self.1))
            }
        }
    }
}
