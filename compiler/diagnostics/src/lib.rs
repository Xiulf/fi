mod mismatched_type;
mod unsolved_constraint;

use hir::InFile;
use syntax::TextRange;

pub trait Diagnostic {
    fn title(&self) -> String;

    fn range(&self) -> TextRange;

    fn primary_annotation(&self) -> Option<SourceAnnotation> {
        None
    }

    fn secondary_annotations(&self) -> Vec<SecondaryAnnotation> {
        Vec::new()
    }

    fn notes(&self) -> Vec<String> {
        Vec::new()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SourceAnnotation {
    pub range: TextRange,
    pub message: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SecondaryAnnotation {
    pub range: InFile<TextRange>,
    pub message: String,
}

pub trait DiagnosticFor {
    fn with_diagnostic<R, F: FnOnce(&dyn Diagnostic) -> R>(&self, f: F) -> R;
}

pub trait DiagnosticForWith<W> {
    fn with_diagnostic<R, F: FnOnce(&dyn Diagnostic) -> R>(&self, with: &W, f: F) -> R;
}

impl From<SecondaryAnnotation> for SourceAnnotation {
    fn from(src: SecondaryAnnotation) -> Self {
        SourceAnnotation {
            range: src.range.value,
            message: src.message,
        }
    }
}

impl<DB: hir::db::HirDatabase> DiagnosticForWith<DB> for dyn hir::diagnostic::Diagnostic {
    fn with_diagnostic<R, F: FnOnce(&dyn Diagnostic) -> R>(&self, with: &DB, f: F) -> R {
        if let Some(v) = self.as_any().downcast_ref::<hir::diagnostic::MismatchedType>() {
            f(&mismatched_type::MismatchedType::new(with, v))
        } else if let Some(v) = self.as_any().downcast_ref::<hir::diagnostic::UnsolvedConstraint>() {
            f(&unsolved_constraint::UnsolvedConstraint::new(with, v))
        } else {
            f(&GenericDiagnostic { diagnostic: self })
        }
    }
}

struct GenericDiagnostic<'d> {
    diagnostic: &'d dyn hir::diagnostic::Diagnostic,
}

impl<'d> Diagnostic for GenericDiagnostic<'d> {
    fn title(&self) -> String {
        self.diagnostic.message()
    }

    fn range(&self) -> TextRange {
        self.diagnostic.display_source().value.range()
    }
}
