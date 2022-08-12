use std::any::Any;
use std::sync::Arc;

use base_db::input::FileId;
use hir_def::diagnostic::Diagnostic;
use hir_def::in_file::InFile;
use hir_def::resolver::ValueNs;
use syntax::{ast, AstPtr, SyntaxNodePtr};

use crate::search::TypeSearchResult;
use crate::ty::{Constraint, Ty};

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
pub struct UnresolvedClass {
    pub file: FileId,
    pub src: AstPtr<ast::Path>,
}

impl Diagnostic for UnresolvedClass {
    fn message(&self) -> String {
        "unknown class".into()
    }

    fn display_source(&self) -> InFile<SyntaxNodePtr> {
        InFile::new(self.file, self.src.syntax_node_ptr())
    }

    fn as_any(&self) -> &(dyn Any + Send + 'static) {
        self
    }
}

#[derive(Debug)]
pub struct UnresolvedOperator {
    pub file: FileId,
    pub src: SyntaxNodePtr,
    pub idx: usize,
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
pub struct PrivateClass {
    pub file: FileId,
    pub src: AstPtr<ast::Path>,
}

impl Diagnostic for PrivateClass {
    fn message(&self) -> String {
        "class is not exported".to_string()
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
    pub idx: usize,
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
pub struct UninferredType {
    pub src: InFile<SyntaxNodePtr>,
}

impl Diagnostic for UninferredType {
    fn message(&self) -> String {
        "could not infer type".into()
    }

    fn display_source(&self) -> InFile<SyntaxNodePtr> {
        self.src
    }

    fn as_any(&self) -> &(dyn Any + Send + 'static) {
        self
    }
}

#[derive(Debug)]
pub struct MismatchedKind {
    pub expected: Ty,
    pub found: Ty,
    pub expected_src: Option<InFile<SyntaxNodePtr>>,
    pub found_src: InFile<SyntaxNodePtr>,
}

impl Diagnostic for MismatchedKind {
    fn message(&self) -> String {
        "mismatched kinds".into()
    }

    fn display_source(&self) -> InFile<SyntaxNodePtr> {
        self.found_src
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
    pub expected_src: Option<InFile<SyntaxNodePtr>>,
    pub found_src: Option<InFile<SyntaxNodePtr>>,
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
pub struct UnsolvedConstraint {
    pub file: FileId,
    pub src: SyntaxNodePtr,
    pub expected: Option<InFile<SyntaxNodePtr>>,
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
pub struct RecursiveTypeAlias {
    pub file: FileId,
    pub src: SyntaxNodePtr,
}

impl Diagnostic for RecursiveTypeAlias {
    fn message(&self) -> String {
        "recursive type alias".into()
    }

    fn display_source(&self) -> InFile<SyntaxNodePtr> {
        InFile::new(self.file, self.src)
    }

    fn as_any(&self) -> &(dyn Any + Send + 'static) {
        self
    }
}

#[derive(Debug)]
pub struct ValueHole {
    pub file: FileId,
    pub src: SyntaxNodePtr,
    pub ty: Ty,
    pub search: Arc<TypeSearchResult<ValueNs>>,
}

impl Diagnostic for ValueHole {
    fn message(&self) -> String {
        "value hole".into()
    }

    fn display_source(&self) -> InFile<SyntaxNodePtr> {
        InFile::new(self.file, self.src)
    }

    fn as_any(&self) -> &(dyn Any + Send + 'static) {
        self
    }
}
