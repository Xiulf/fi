pub use crate::symbol::{Ident, Symbol};
pub use diagnostics::Span;
pub use parser::literal::*;

#[derive(Debug, derivative::Derivative)]
#[derivative(Hash)]
pub struct Package {
    #[derivative(Hash = "ignore")]
    pub span: Span,
    pub module: Module,
}

#[derive(Debug, derivative::Derivative)]
#[derivative(Hash)]
pub struct Module {
    #[derivative(Hash = "ignore")]
    pub span: Span,
    pub items: Vec<Item>,
}

#[derive(Debug, derivative::Derivative)]
#[derivative(Hash)]
pub struct Item {
    #[derivative(Hash = "ignore")]
    pub span: Span,
    #[derivative(Hash(hash_with = "hash_ident"))]
    pub name: Ident,
    pub kind: ItemKind,
}

#[derive(Debug, Hash)]
pub enum ItemKind {
    Module {
        module: Module,
    },
    Extern {
        abi: Abi,
        ty: Type,
    },
    Func {
        params: Vec<Param>,
        ret: Type,
        body: Block,
    },
    Var {
        ty: Type,
        val: Option<Expr>,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Abi {
    None,
    C,
}

#[derive(Debug, derivative::Derivative)]
#[derivative(Hash)]
pub struct Param {
    #[derivative(Hash = "ignore")]
    pub span: Span,
    #[derivative(Hash(hash_with = "hash_ident"))]
    pub name: Ident,
    pub ty: Type,
}

#[derive(Debug, derivative::Derivative)]
#[derivative(Hash)]
pub struct Block {
    #[derivative(Hash = "ignore")]
    pub span: Span,
    pub stmts: Vec<Stmt>,
}

#[derive(Debug, derivative::Derivative)]
#[derivative(Hash)]
pub struct Stmt {
    #[derivative(Hash = "ignore")]
    pub span: Span,
    pub kind: StmtKind,
}

#[derive(Debug, Hash)]
pub enum StmtKind {
    Item(Item),
    Semi(Expr),
    Expr(Expr),
}

#[derive(Debug, derivative::Derivative)]
#[derivative(Hash)]
pub struct Path {
    #[derivative(Hash = "ignore")]
    pub span: Span,
    pub root: bool,
    pub segs: Vec<PathSeg>,
}

#[derive(Debug, derivative::Derivative)]
#[derivative(Hash)]
pub struct PathSeg {
    #[derivative(Hash = "ignore")]
    pub span: Span,
    #[derivative(Hash(hash_with = "hash_ident"))]
    pub name: Ident,
}

#[derive(Debug, derivative::Derivative)]
#[derivative(Hash)]
pub struct Expr {
    #[derivative(Hash = "ignore")]
    pub span: Span,
    pub kind: ExprKind,
}

#[derive(Debug, Hash)]
pub enum ExprKind {
    Path {
        path: Path,
    },
    Int {
        val: u128,
    },
    Float {
        bits: u64,
    },
    Char {
        val: char,
    },
    String {
        val: String,
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
        obj: Box<Expr>,
        method: Ident,
        args: Vec<Arg>,
    },
    Field {
        obj: Box<Expr>,
        field: Ident,
    },
    Index {
        list: Box<Expr>,
        index: Box<Expr>,
    },
    Slice {
        list: Box<Expr>,
        low: Option<Box<Expr>>,
        high: Option<Box<Expr>>,
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
    Assign {
        lhs: Box<Expr>,
        rhs: Box<Expr>,
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

#[derive(Debug, derivative::Derivative)]
#[derivative(Hash)]
pub struct InitField {
    #[derivative(Hash = "ignore")]
    pub span: Span,
    #[derivative(Hash(hash_with = "hash_ident"))]
    pub name: Ident,
    pub value: Expr,
}

#[derive(Debug, derivative::Derivative)]
#[derivative(Hash)]
pub struct Arg {
    #[derivative(Hash = "ignore")]
    pub span: Span,
    #[derivative(Hash(hash_with = "hash_option_ident"))]
    pub name: Option<Ident>,
    pub value: Expr,
}

#[derive(Debug, Clone, Copy, Hash)]
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

#[derive(Debug, Clone, Copy, Hash)]
pub enum UnOp {
    Neg,
    Not,
}

#[derive(Debug, derivative::Derivative)]
#[derivative(Hash)]
pub struct Type {
    #[derivative(Hash = "ignore")]
    pub span: Span,
    pub kind: TypeKind,
}

#[derive(Debug, Hash)]
pub enum TypeKind {
    Infer,
    Path {
        path: Path,
    },
    Func {
        params: Vec<TypeParam>,
        ret: Box<Type>,
    },
    Ref {
        ty: Box<Type>,
    },
    Gc {
        ty: Box<Type>,
    },
}

#[derive(derivative::Derivative, Debug)]
#[derivative(Hash)]
pub struct TypeParam {
    #[derivative(Hash = "ignore")]
    pub span: Span,
    #[derivative(Hash(hash_with = "hash_ident"))]
    pub name: Ident,
    pub ty: Type,
}

fn hash_ident<H: std::hash::Hasher>(ident: &Ident, state: &mut H) {
    std::hash::Hash::hash(&*ident.symbol, state);
}

fn hash_option_ident<H: std::hash::Hasher>(ident: &Option<Ident>, state: &mut H) {
    std::hash::Hash::hash(&std::mem::discriminant(ident), state);

    if let Some(ident) = ident {
        std::hash::Hash::hash(&*ident.symbol, state);
    }
}
