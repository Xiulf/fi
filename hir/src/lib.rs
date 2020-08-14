pub mod resolve;

pub use diagnostics::Span;
use std::collections::BTreeMap;
pub use syntax::ast::{BinOp, Ident, Literal, Symbol, UnOp};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Id(pub(crate) u64);

#[derive(Debug)]
pub struct Package {
    pub items: BTreeMap<Id, Item>,
    pub exprs: BTreeMap<Id, Expr>,
    pub types: BTreeMap<Id, Type>,
}

#[derive(Debug)]
pub struct Item {
    pub span: Span,
    pub id: Id,
    pub name: Ident,
    pub kind: ItemKind,
}

#[derive(Debug)]
pub enum ItemKind {
    Extern {
        abi: Abi,
        ty: Id,
    },
    Func {
        params: Vec<Id>,
        ret: Id,
        body: Block,
        captures: Vec<Id>,
    },
    Param {
        ty: Id,
    },
    Var {
        ty: Option<Id>,
        val: Option<Id>,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Abi {
    None,
    C,
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
    Item(Id),
    Semi(Id),
    Expr(Id),
}

#[derive(Debug)]
pub struct Expr {
    pub span: Span,
    pub id: Id,
    pub kind: ExprKind,
}

#[derive(Debug)]
pub enum ExprKind {
    Item {
        id: Id,
    },
    Literal {
        lit: Literal,
    },
    Type {
        ty: Id,
    },
    Array {
        exprs: Vec<Id>,
    },
    Tuple {
        exprs: Vec<Id>,
    },
    Block {
        block: Block,
    },
    Call {
        func: Id,
        args: Vec<Arg>,
    },
    MethodCall {
        obj: Id,
        method: Ident,
        args: Vec<Arg>,
    },
    Field {
        obj: Id,
        field: Ident,
    },
    Index {
        list: Id,
        index: Id,
    },
    Slice {
        list: Id,
        low: Option<Id>,
        high: Option<Id>,
    },
    Ref {
        expr: Id,
    },
    Deref {
        expr: Id,
    },
    TypeOf {
        expr: Id,
    },
    Cast {
        expr: Id,
        ty: Id,
    },
    Assign {
        lhs: Id,
        rhs: Id,
    },
    BinOp {
        op: BinOp,
        lhs: Id,
        rhs: Id,
    },
    UnOp {
        op: UnOp,
        rhs: Id,
    },
    IfElse {
        cond: Id,
        then: Block,
        else_: Option<Block>,
    },
    While {
        label: Option<Id>,
        cond: Id,
        body: Block,
    },
    Loop {
        label: Option<Id>,
        body: Block,
    },
    Break {
        label: Option<Id>,
        expr: Option<Id>,
    },
    Continue {
        label: Option<Id>,
    },
    Return {
        expr: Option<Id>,
    },
    Defer {
        expr: Id,
    },
}

#[derive(Debug)]
pub struct Arg {
    pub span: Span,
    pub name: Option<Ident>,
    pub value: Id,
}

#[derive(Debug)]
pub struct Type {
    pub span: Span,
    pub id: Id,
    pub kind: TypeKind,
}

#[derive(Debug)]
pub enum TypeKind {
    Never,
    Bool,
    Int { bits: u8 },
    Float { bits: u8 },
    Ptr { gc: bool, to: Id },
    Func { params: Vec<TypeParam>, ret: Id },
    Tuple { tys: Vec<Id> },
}

impl TypeKind {
    pub const SIGN_MASK: u8 = 0b10000000;
    pub const BITS_MASK: u8 = 0b01111111;
}

#[derive(Debug)]
pub struct TypeParam {
    pub span: Span,
    pub name: Ident,
    pub ty: Id,
}
