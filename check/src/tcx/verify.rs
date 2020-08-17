use crate::tcx::Tcx;
use crate::ty::*;
use diagnostics::{Diagnostic, Severity};

impl<'tcx> Tcx<'tcx> {
    pub fn verify(&self) {
        let types = self.types.borrow();

        for (id, ty) in types.iter() {
            if let Type::Var(_) = ty {
                self.reporter.add(
                    Diagnostic::new(Severity::Error, 0006, "type annotation needed").label(
                        Severity::Error,
                        self.span_of(id),
                        None::<String>,
                    ),
                );
            }
        }
    }
}
