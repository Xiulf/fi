use diagnostics::{Diagnostic, ToDiagnostic};
use hir_def::display::HirDisplay;
use hir_def::id::TypedItemId;

use crate::ty::Ty;
use crate::{Db, TyOrigin};

pub struct RecursiveType {
    pub ty: Ty,
    pub owner: TypedItemId,
    pub origin: TyOrigin,
}

impl ToDiagnostic for RecursiveType {
    type Db<'t> = dyn Db + 't;

    fn to_diagnostic(self, db: &Self::Db<'_>) -> Diagnostic {
        let range = self.origin.to_text_range(db, self.owner);

        Diagnostic::new(
            format!("recursive type `{}`", self.ty.display(db)),
            range.file,
            range.value,
        )
    }
}
