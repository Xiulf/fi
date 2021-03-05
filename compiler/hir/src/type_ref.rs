use crate::name::Name;
use crate::path::ModPath;
use syntax::ast;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeRef {
    Placeholder,
    Tuple(Vec<TypeRef>),
    Path(ModPath),
    Ptr(Box<TypeRef>, PtrLen),
    Slice(Box<TypeRef>),
    Array(Box<TypeRef>, usize),
    Func(Vec<TypeRef>),
    Error,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum PtrLen {
    Single,
    Multiple,
}

impl TypeRef {
    pub(crate) fn from_ast(node: ast::Type) -> Self {
        TypeRef::Error
    }

    pub(crate) fn from_ast_opt(node: Option<ast::Type>) -> Self {
        node.map(Self::from_ast).unwrap_or(TypeRef::Error)
    }
}
