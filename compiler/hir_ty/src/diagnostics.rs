use crate::ty::Ty;
use base_db::input::FileId;
use hir_def::diagnostic::Diagnostic;
use hir_def::in_file::InFile;
use std::any::Any;
use syntax::{ast, AstPtr, SyntaxNodePtr};

#[derive(Debug)]
pub struct UnresolvedValue {
    pub file: FileId,
    pub expr: SyntaxNodePtr,
}

impl Diagnostic for UnresolvedValue {
    fn message(&self) -> String {
        "unknown value".into()
    }

    fn display_source(&self) -> InFile<SyntaxNodePtr> {
        InFile::new(self.file, self.expr)
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
