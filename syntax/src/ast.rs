pub use crate::symbol::{Ident, Symbol};
pub use diagnostics::Span;
pub use parser::literal::*;

#[derive(Debug)]
pub struct Package {
    pub span: Span,
    pub module: Module,
}

#[derive(Debug)]
pub struct Module {
    pub span: Span,
    pub items: Vec<Item>,
}

#[derive(Debug)]
pub struct Item {
    pub span: Span,
    pub name: Ident,
    pub kind: ItemKind,
}

#[derive(Debug)]
pub enum ItemKind {
    Module {
        module: Module,
    },
    Extern {
        abi: StringLiteral,
        ty: Type,
    },
    Func {
        params: Vec<Param>,
        ret: Option<Type>,
        body: Block,
    },
    Var {
        ty: Option<Type>,
        value: Expr,
    },
}

#[derive(Debug)]
pub struct Param {
    pub span: Span,
    pub name: Ident,
    pub ty: Option<Type>,
}

#[derive(Debug)]
pub struct Block {
    pub span: Span,
    pub stmts: Vec<Stmt>,
}

#[derive(Debug)]
pub struct Stmt {
    pub span: Span,
    pub kind: StmtKind,
}

#[derive(Debug)]
pub enum StmtKind {
    Item(Item),
    Semi(Expr),
    Expr(Expr),
}

#[derive(Debug)]
pub struct Path {
    pub span: Span,
    pub segs: Vec<PathSeg>,
}

#[derive(Debug)]
pub struct PathSeg {
    pub span: Span,
    pub name: Ident,
}

#[derive(Debug)]
pub struct Expr {
    pub span: Span,
    pub kind: ExprKind,
}

#[derive(Debug)]
pub enum ExprKind {
    Path {
        path: Path,
    },
    Literal {
        lit: Literal,
    },
    Parens {
        inner: Box<Expr>,
    },
    Type {
        ty: Type,
    },
    Array {
        exprs: Vec<Expr>,
    },
    Tuple {
        exprs: Vec<Expr>,
    },
    Init {
        fields: Vec<InitField>,
    },
    Block {
        block: Block,
    },

    Call {
        func: Box<Expr>,
        args: Vec<Arg>,
    },
    MethodCall {
        func: Box<Expr>,
        method: Ident,
        args: Vec<Arg>,
    },
    Field {
        obj: Box<Expr>,
        field: Ident,
    },
    Ref {
        expr: Box<Expr>,
    },
    Deref {
        expr: Box<Expr>,
    },
    TypeOf {
        expr: Box<Expr>,
    },
    Cast {
        expr: Box<Expr>,
        ty: Type,
    },
    BinOp {
        op: BinOp,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    UnOp {
        op: UnOp,
        rhs: Box<Expr>,
    },

    IfElse {
        cond: Box<Expr>,
        then: Block,
        else_: Option<Block>,
    },
    While {
        label: Option<Ident>,
        cond: Box<Expr>,
        body: Block,
    },
    Loop {
        label: Option<Ident>,
        body: Block,
    },
    Break {
        label: Option<Ident>,
        expr: Option<Box<Expr>>,
    },
    Continue {
        label: Option<Ident>,
    },
    Return {
        expr: Option<Box<Expr>>,
    },
    Defer {
        expr: Box<Expr>,
    },
}

#[derive(Debug)]
pub struct InitField {
    pub span: Span,
    pub name: Ident,
    pub value: Expr,
}

#[derive(Debug)]
pub struct Arg {
    pub span: Span,
    pub name: Option<Ident>,
    pub value: Expr,
}

#[derive(Debug)]
pub enum BinOp {
    And,
    Or,
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Lt,
    Le,
    Gt,
    Ge,
    Eq,
    Ne,
    BitAnd,
    BitOr,
    BitXOr,
    Shl,
    Shr,
}

#[derive(Debug)]
pub enum UnOp {
    Neg,
    Not,
}

#[derive(Debug)]
pub struct Type {
    pub span: Span,
    pub kind: TypeKind,
}

#[derive(Debug)]
pub enum TypeKind {
    Path { path: Path },
}
