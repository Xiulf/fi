use diagnostics::{Diagnostic, ToDiagnostic};
use hir_def::display::HirDisplay;
use hir_def::id::TypedItemId;

use crate::ty::Ty;
use crate::{Db, TyOrigin};

pub struct TypeMismatch {
    pub a: Ty,
    pub b: Ty,
    pub owner: TypedItemId,
    pub origin: TyOrigin,
}

impl ToDiagnostic for TypeMismatch {
    type Db<'t> = dyn Db + 't;

    fn to_diagnostic(self, db: &Self::Db<'_>) -> diagnostics::Diagnostic {
        let range = self.origin.to_text_range(db, self.owner);

        Diagnostic::new(
            format!("expected type `{}`, found `{}`", self.a.display(db), self.b.display(db)),
            range.file,
            range.value,
        )
    }
}
