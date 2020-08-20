use crate::tcx::Tcx;
use crate::ty::*;
use diagnostics::{Diagnostic, Severity};
use std::iter::FromIterator;

impl<'tcx> Tcx<'tcx> {
    pub fn verify(&self) {
        let mut types = self.types.borrow_mut();
        let mut subst = Vec::new();

        for (id, ty) in types.iter() {
            if let Type::Var(_) = ty {
                self.reporter.add(
                    Diagnostic::new(Severity::Error, 0006, "type annotation needed").label(
                        Severity::Error,
                        self.span_of(id),
                        None::<String>,
                    ),
                );
            } else if let Type::VInt(tvar) = ty {
                subst.push((*tvar, self.builtin.isize));
            } else if let Type::VUInt(tvar) = ty {
                subst.push((*tvar, self.builtin.usize));
            } else if let Type::VFloat(tvar) = ty {
                subst.push((*tvar, self.builtin.f32));
            }
        }

        let subst = crate::subst::Subst::from_iter(subst);

        for (_, ty) in types.iter_mut() {
            subst.apply_ty(ty);
        }
    }
}
