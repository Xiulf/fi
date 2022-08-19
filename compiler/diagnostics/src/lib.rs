mod duplicate_declaration;
mod inference_cycle;
mod mismatched_kind;
mod mismatched_type;
mod private_operator;
mod unresolved_operator;
mod unsolved_constraint;
mod value_hole;

use hir::InFile;
use syntax::{ast, AstNode, NameOwner, Parsed, SyntaxKind, SyntaxNodePtr, TextRange};

pub trait Diagnostic {
    fn title(&self) -> String;

    fn range(&self) -> TextRange;

    fn level(&self) -> Level {
        Level::Error
    }

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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Level {
    Error,
    Warning,
    Info,
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
        } else if let Some(v) = self.as_any().downcast_ref::<hir::diagnostic::MismatchedKind>() {
            f(&mismatched_kind::MismatchedKind::new(with, v))
        } else if let Some(v) = self.as_any().downcast_ref::<hir::diagnostic::UnsolvedConstraint>() {
            f(&unsolved_constraint::UnsolvedConstraint::new(with, v))
        } else if let Some(v) = self.as_any().downcast_ref::<hir::diagnostic::UnresolvedOperator>() {
            f(&unresolved_operator::UnresolvedOperator::new(with, v))
        } else if let Some(v) = self.as_any().downcast_ref::<hir::diagnostic::PrivateOperator>() {
            f(&private_operator::PrivateOperator::new(with, v))
        } else if let Some(v) = self.as_any().downcast_ref::<hir::diagnostic::DuplicateDeclaration>() {
            f(&duplicate_declaration::DuplicateDeclaration::new(with, v))
        } else if let Some(v) = self.as_any().downcast_ref::<hir::diagnostic::InferenceCycle>() {
            f(&inference_cycle::InferenceCycle::new(with, v))
        } else if let Some(v) = self.as_any().downcast_ref::<hir::diagnostic::ValueHole>() {
            f(&value_hole::ValueHole::new(with, v))
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

fn item_name(ptr: SyntaxNodePtr, parse: &Parsed<ast::SourceFile>) -> TextRange {
    match ptr.kind() {
        | SyntaxKind::ITEM_FUN => ast::ItemFun::cast(ptr.to_node(parse.tree().syntax()))
            .and_then(|i| Some(i.name()?.syntax().text_range()))
            .unwrap_or_else(|| ptr.range()),
        | _ => ptr.range(),
    }
}
