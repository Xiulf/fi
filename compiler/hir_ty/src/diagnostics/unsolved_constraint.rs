use diagnostics::{Diagnostic, ToDiagnostic};
use hir_def::display::HirDisplay;
use hir_def::id::TypedItemId;

use crate::ty::{Constraint, ConstraintOrigin};
use crate::Db;

pub struct UnsolvedConstraint {
    pub constraint: Constraint,
    pub origin: ConstraintOrigin,
    pub owner: TypedItemId,
}

impl ToDiagnostic for UnsolvedConstraint {
    type Db<'t> = dyn Db + 't;

    fn to_diagnostic(self, db: &Self::Db<'_>) -> Diagnostic {
        let range = self.origin.to_text_range(db, self.owner);
        let trait_ = self.constraint.trait_id.it(db);
        let item_tree = hir_def::item_tree::query(db, trait_.file);
        let trait_ = item_tree[trait_.value].name;
        let msg = if self.constraint.args.len() == 1 {
            format!(
                "no impl of {} found for type `{}`",
                trait_.display(db),
                self.constraint.args[0].display(db)
            )
        } else {
            let args = self
                .constraint
                .args
                .iter()
                .map(|a| format!("`{}`", a.display(db)))
                .collect::<Vec<_>>()
                .join(", ");

            format!("no impl of {} found for types {}", trait_.display(db), args)
        };

        Diagnostic::new(msg, range.file, range.value)
    }
}
