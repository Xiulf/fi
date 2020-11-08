mod debug;

pub use crate::symbol::{Ident, Symbol};
pub use codespan::Span;

#[derive(PartialEq, Eq)]
pub struct Module {
    pub span: Span,
    pub name: Ident,
    pub exports: Exports,
    pub imports: Vec<ImportDecl>,
    pub decls: Vec<Decl>,
}

#[derive(PartialEq, Eq)]
pub enum Exports {
    All,
    Some(Vec<Export>),
}

#[derive(PartialEq, Eq)]
pub struct Export {
    pub span: Span,
    pub name: Ident,
    pub kind: ExportKind,
}

#[derive(Debug, PartialEq, Eq)]
pub enum ExportKind {
    Value,
    Type,
    Module,
}

#[derive(PartialEq, Eq)]
pub struct ImportDecl {
    pub span: Span,
    pub module: Ident,
    pub names: Option<(bool, Vec<Import>)>,
    pub qual: Option<Ident>,
}

#[derive(PartialEq, Eq)]
pub struct Import {
    pub span: Span,
    pub name: Ident,
    pub alias: Option<Ident>,
    pub kind: ImportKind,
}

#[derive(Debug, PartialEq, Eq)]
pub enum ImportKind {
    Value,
    Type,
}

#[derive(PartialEq, Eq)]
pub struct Decl {
    pub span: Span,
    pub name: Ident,
    pub kind: DeclKind,
}

#[derive(PartialEq, Eq)]
pub enum DeclKind {
    FuncTy {
        ty: Type,
    },
    Func {
        pats: Vec<Pat>,
        val: Guarded,
    },
    AliasKind {
        kind: Type,
    },
    Alias {
        vars: Vec<TypeVar>,
        ty: Type,
    },
    Iface {
        head: IfaceHead,
        body: Option<IfaceBody>,
    },
    ImplChain {
        impls: Vec<Impl>,
    },
}

#[derive(PartialEq, Eq)]
pub struct IfaceHead {
    pub span: Span,
    pub parent: Option<Vec<Constraint>>,
    pub vars: Vec<TypeVar>,
}

#[derive(PartialEq, Eq)]
pub struct IfaceBody {
    pub span: Span,
    pub decls: Vec<IfaceDecl>,
}

#[derive(PartialEq, Eq)]
pub struct IfaceDecl {
    pub span: Span,
    pub name: Ident,
    pub kind: IfaceDeclKind,
}

#[derive(PartialEq, Eq)]
pub enum IfaceDeclKind {
    FuncTy { ty: Type },
}

#[derive(PartialEq, Eq)]
pub struct Impl {
    pub span: Span,
    pub head: ImplHead,
    pub body: Option<ImplBody>,
}

#[derive(PartialEq, Eq)]
pub struct ImplHead {
    pub span: Span,
    pub cs: Option<Vec<Constraint>>,
    pub iface: Ident,
    pub tys: Vec<Type>,
}

#[derive(PartialEq, Eq)]
pub struct ImplBody {
    pub span: Span,
    pub decls: Vec<ImplDecl>,
}

#[derive(PartialEq, Eq)]
pub struct ImplDecl {
    pub span: Span,
    pub name: Ident,
    pub kind: ImplDeclKind,
}

#[derive(PartialEq, Eq)]
pub enum ImplDeclKind {
    FuncTy { ty: Type },
    Func { pats: Vec<Pat>, val: Guarded },
}

#[derive(PartialEq, Eq)]
pub struct Pat {
    pub span: Span,
    pub kind: PatKind,
}

#[derive(PartialEq, Eq)]
pub enum PatKind {
    Parens { inner: Box<Pat> },
    Wildcard,
    Ident { name: Ident },
    App { base: Box<Pat>, args: Vec<Pat> },
    Tuple { pats: Vec<Pat> },
}

#[derive(PartialEq, Eq)]
pub enum Guarded {
    Unconditional(Expr),
    Guarded(Vec<GuardedExpr>),
}

#[derive(PartialEq, Eq)]
pub struct GuardedExpr {
    pub span: Span,
    pub guard: Expr,
    pub val: Expr,
}

#[derive(PartialEq, Eq)]
pub struct Expr {
    pub span: Span,
    pub kind: ExprKind,
}

#[derive(PartialEq, Eq)]
pub enum ExprKind {
    Parens { inner: Box<Expr> },
    Ident { name: Ident },
    Int { val: u128 },
    Float { bits: u64 },
    Char { val: char },
    Str { val: String },
    App { base: Box<Expr>, args: Vec<Expr> },
    Tuple { exprs: Vec<Expr> },
    Typed { expr: Box<Expr>, ty: Type },
}

#[derive(PartialEq, Eq)]
pub struct Type {
    pub span: Span,
    pub kind: TypeKind,
}

#[derive(PartialEq, Eq)]
pub enum TypeKind {
    Parens { inner: Box<Type> },
    Hole,
    Var { name: Ident },
    Ident { name: Ident },
    App { base: Box<Type>, args: Vec<Type> },
    Tuple { tys: Vec<Type> },
    Func { params: Vec<Type>, ret: Box<Type> },
    Forall { vars: Vec<TypeVar>, ret: Box<Type> },
    Cons { cs: Constraint, ty: Box<Type> },
    Record { row: Row },
}

#[derive(PartialEq, Eq)]
pub struct Row {
    pub span: Span,
    pub fields: Vec<RowField>,
    pub tail: Option<Box<Type>>,
}

#[derive(PartialEq, Eq)]
pub struct RowField {
    pub span: Span,
    pub name: Ident,
    pub ty: Type,
}

#[derive(PartialEq, Eq)]
pub enum TypeVar {
    Name { name: Ident },
    Kind { name: Ident, kind: Type },
}

#[derive(PartialEq, Eq)]
pub enum Constraint {
    CS { iface: Ident, tys: Vec<Type> },
    Parens { inner: Box<Constraint> },
}
