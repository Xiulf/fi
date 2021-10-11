use crate::ty::{Constraint, Ty};
use base_db::input::FileId;
use hir_def::diagnostic::Diagnostic;
use hir_def::in_file::InFile;
use std::any::Any;
use syntax::{ast, AstPtr, SyntaxNodePtr};

#[derive(Debug)]
pub struct UnresolvedValue {
    pub file: FileId,
    pub src: SyntaxNodePtr,
}

impl Diagnostic for UnresolvedValue {
    fn message(&self) -> String {
        "unknown value".into()
    }

    fn display_source(&self) -> InFile<SyntaxNodePtr> {
        InFile::new(self.file, self.src)
    }

    fn as_any(&self) -> &(dyn Any + Send + 'static) {
        self
    }
}

#[derive(Debug)]
pub struct UnresolvedType {
    pub file: FileId,
    pub ty: AstPtr<ast::Type>,
}

impl Diagnostic for UnresolvedType {
    fn message(&self) -> String {
        "unknown type".into()
    }

    fn display_source(&self) -> InFile<SyntaxNodePtr> {
        InFile::new(self.file, self.ty.syntax_node_ptr())
    }

    fn as_any(&self) -> &(dyn Any + Send + 'static) {
        self
    }
}

#[derive(Debug)]
pub struct UnresolvedOperator {
    pub file: FileId,
    pub src: SyntaxNodePtr,
}

impl Diagnostic for UnresolvedOperator {
    fn message(&self) -> String {
        "unknown operator".into()
    }

    fn display_source(&self) -> InFile<SyntaxNodePtr> {
        InFile::new(self.file, self.src)
    }

    fn as_any(&self) -> &(dyn Any + Send + 'static) {
        self
    }
}

#[derive(Debug)]
pub struct PrivateValue {
    pub file: FileId,
    pub src: SyntaxNodePtr,
}

impl Diagnostic for PrivateValue {
    fn message(&self) -> String {
        "value is not exported".to_string()
    }

    fn display_source(&self) -> InFile<SyntaxNodePtr> {
        InFile::new(self.file, self.src)
    }

    fn as_any(&self) -> &(dyn Any + Send + 'static) {
        self
    }
}

#[derive(Debug)]
pub struct PrivateType {
    pub file: FileId,
    pub src: AstPtr<ast::Type>,
}

impl Diagnostic for PrivateType {
    fn message(&self) -> String {
        "type is not exported".to_string()
    }

    fn display_source(&self) -> InFile<SyntaxNodePtr> {
        InFile::new(self.file, self.src.syntax_node_ptr())
    }

    fn as_any(&self) -> &(dyn Any + Send + 'static) {
        self
    }
}

#[derive(Debug)]
pub struct PrivateOperator {
    pub file: FileId,
    pub src: SyntaxNodePtr,
}

impl Diagnostic for PrivateOperator {
    fn message(&self) -> String {
        "operator is not exported".to_string()
    }

    fn display_source(&self) -> InFile<SyntaxNodePtr> {
        InFile::new(self.file, self.src)
    }

    fn as_any(&self) -> &(dyn Any + Send + 'static) {
        self
    }
}

#[derive(Debug)]
pub struct MismatchedKind {
    pub file: FileId,
    pub src: SyntaxNodePtr,
    pub expected: Ty,
    pub found: Ty,
}

impl Diagnostic for MismatchedKind {
    fn message(&self) -> String {
        "mismatched kinds".into()
    }

    fn display_source(&self) -> InFile<SyntaxNodePtr> {
        InFile::new(self.file, self.src)
    }

    fn as_any(&self) -> &(dyn Any + Send + 'static) {
        self
    }
}

#[derive(Debug)]
pub struct MismatchedType {
    pub file: FileId,
    pub src: SyntaxNodePtr,
    pub expected: Ty,
    pub found: Ty,
}

impl Diagnostic for MismatchedType {
    fn message(&self) -> String {
        "mismatched types".into()
    }

    fn display_source(&self) -> InFile<SyntaxNodePtr> {
        InFile::new(self.file, self.src)
    }

    fn as_any(&self) -> &(dyn Any + Send + 'static) {
        self
    }
}

#[derive(Debug)]
pub struct UnresolvedClass {
    pub file: FileId,
    pub src: SyntaxNodePtr,
}

impl Diagnostic for UnresolvedClass {
    fn message(&self) -> String {
        "unknown class".into()
    }

    fn display_source(&self) -> InFile<SyntaxNodePtr> {
        InFile::new(self.file, self.src)
    }

    fn as_any(&self) -> &(dyn Any + Send + 'static) {
        self
    }
}

#[derive(Debug)]
pub struct UnsolvedConstraint {
    pub file: FileId,
    pub src: SyntaxNodePtr,
    pub ctnt: Constraint,
}

impl Diagnostic for UnsolvedConstraint {
    fn message(&self) -> String {
        "unresolved constraint".into()
    }

    fn display_source(&self) -> InFile<SyntaxNodePtr> {
        InFile::new(self.file, self.src)
    }

    fn as_any(&self) -> &(dyn Any + Send + 'static) {
        self
    }
}

#[derive(Debug)]
pub struct BreakOutsideLoop {
    pub file: FileId,
    pub src: SyntaxNodePtr,
}

impl Diagnostic for BreakOutsideLoop {
    fn message(&self) -> String {
        "cannot break outside a loop".into()
    }

    fn display_source(&self) -> InFile<SyntaxNodePtr> {
        InFile::new(self.file, self.src)
    }

    fn as_any(&self) -> &(dyn Any + Send + 'static) {
        self
    }
}

#[derive(Debug)]
pub struct CannotBreakWithValue {
    pub file: FileId,
    pub src: SyntaxNodePtr,
}

impl Diagnostic for CannotBreakWithValue {
    fn message(&self) -> String {
        "cannot break with a value inside a while loop".into()
    }

    fn display_source(&self) -> InFile<SyntaxNodePtr> {
        InFile::new(self.file, self.src)
    }

    fn as_any(&self) -> &(dyn Any + Send + 'static) {
        self
    }
}

#[derive(Debug)]
pub struct NextOutsideLoop {
    pub file: FileId,
    pub src: SyntaxNodePtr,
}

impl Diagnostic for NextOutsideLoop {
    fn message(&self) -> String {
        "cannot use next outside a loop".into()
    }

    fn display_source(&self) -> InFile<SyntaxNodePtr> {
        InFile::new(self.file, self.src)
    }

    fn as_any(&self) -> &(dyn Any + Send + 'static) {
        self
    }
}

#[derive(Debug)]
pub struct CannotNextWithValue {
    pub file: FileId,
    pub src: SyntaxNodePtr,
}

impl Diagnostic for CannotNextWithValue {
    fn message(&self) -> String {
        "cannot use next with a value inside loops".into()
    }

    fn display_source(&self) -> InFile<SyntaxNodePtr> {
        InFile::new(self.file, self.src)
    }

    fn as_any(&self) -> &(dyn Any + Send + 'static) {
        self
    }
}
