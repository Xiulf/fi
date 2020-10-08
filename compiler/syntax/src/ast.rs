pub use crate::symbol::{Ident, Symbol};
pub use diagnostics::Span;
pub use parser::literal::*;

#[derive(Debug, Hash)]
pub struct Package {
    pub modules: Vec<Module>,
}

#[derive(Debug, Clone, derivative::Derivative)]
#[derivative(Hash)]
pub struct Module {
    #[derivative(Hash = "ignore")]
    pub span: Span,
    #[derivative(Hash(hash_with = "hash_ident"))]
    pub name: Ident,
    pub exports: Exports,
    pub imports: Vec<Import>,
    pub items: Vec<Item>,
}

#[derive(Debug, Clone, Hash)]
pub enum Exports {
    All,
    Some(Vec<Export>),
}

#[derive(Debug, Clone, derivative::Derivative)]
#[derivative(Hash)]
pub struct Export {
    #[derivative(Hash(hash_with = "hash_ident"))]
    pub name: Ident,
}
#[derive(Debug, Clone, derivative::Derivative)]
#[derivative(Hash)]
pub struct Import {
    #[derivative(Hash = "ignore")]
    pub span: Span,
    #[derivative(Hash(hash_with = "hash_ident"))]
    pub module: Ident,
    #[derivative(Hash(hash_with = "hash_option_ident"))]
    pub alias: Option<Ident>,
    pub hiding: bool,
    pub imports: Option<Vec<ImportItem>>,
}

#[derive(Debug, Clone, derivative::Derivative)]
#[derivative(Hash)]
pub struct ImportItem {
    #[derivative(Hash = "ignore")]
    pub span: Span,
    #[derivative(Hash(hash_with = "hash_ident"))]
    pub name: Ident,
    #[derivative(Hash(hash_with = "hash_option_ident"))]
    pub alias: Option<Ident>,
}

#[derive(Debug, Clone, derivative::Derivative, serde::Serialize, serde::Deserialize)]
#[derivative(Hash)]
pub struct Attribute {
    pub span: Span,
    pub kind: AttrKind,
}

#[derive(Debug, Clone, Hash, serde::Serialize, serde::Deserialize)]
pub enum AttrKind {
    Doc(String),
    NoMangle,
    Lang(StringLiteral),
    Intrinsic,
    Main,
    Poly,
    Macro,
}

#[derive(Debug, Clone, derivative::Derivative)]
#[derivative(Hash)]
pub struct Item {
    #[derivative(Hash = "ignore")]
    pub span: Span,
    pub attrs: Vec<Attribute>,
    #[derivative(Hash(hash_with = "hash_ident"))]
    pub name: Ident,
    pub kind: ItemKind,
}

#[derive(Debug, Clone, derivative::Derivative)]
#[derivative(Hash)]
pub enum ItemKind {
    Extern {
        abi: Abi,
        ty: Type,
    },
    Func {
        generics: Generics,
        params: Vec<Param>,
        ret: Type,
        body: Block,
    },
    Var {
        ty: Type,
        val: Option<Expr>,
    },
    Const {
        ty: Type,
        val: Expr,
    },
    Struct {
        generics: Generics,
        fields: Vec<StructField>,
        methods: Vec<Method>,
    },
    Enum {
        generics: Generics,
        variants: Vec<EnumVariant>,
        methods: Vec<Method>,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
pub enum Abi {
    None,
    C,
}

#[derive(Debug, Clone, derivative::Derivative)]
#[derivative(Hash)]
pub struct Generics {
    #[derivative(Hash = "ignore")]
    pub span: Span,
    pub params: Vec<Generic>,
}

#[derive(Debug, Clone, derivative::Derivative)]
#[derivative(Hash)]
pub struct Generic {
    #[derivative(Hash = "ignore")]
    pub span: Span,
    #[derivative(Hash(hash_with = "hash_ident"))]
    pub name: Ident,
}

#[derive(Debug, Clone, derivative::Derivative)]
#[derivative(Hash)]
pub struct Param {
    #[derivative(Hash = "ignore")]
    pub span: Span,
    #[derivative(Hash(hash_with = "hash_ident"))]
    pub name: Ident,
    pub ty: Type,
}

#[derive(Debug, Clone, derivative::Derivative)]
#[derivative(Hash)]
pub struct StructField {
    #[derivative(Hash = "ignore")]
    pub span: Span,
    #[derivative(Hash(hash_with = "hash_ident"))]
    pub name: Ident,
    pub ty: Type,
}

#[derive(Debug, Clone, derivative::Derivative)]
#[derivative(Hash)]
pub struct EnumVariant {
    #[derivative(Hash = "ignore")]
    pub span: Span,
    #[derivative(Hash(hash_with = "hash_ident"))]
    pub name: Ident,
    pub fields: Option<Vec<StructField>>,
}

#[derive(Debug, Clone, derivative::Derivative)]
#[derivative(Hash)]
pub struct Method {
    #[derivative(Hash = "ignore")]
    pub span: Span,
    #[derivative(Hash(hash_with = "hash_ident"))]
    pub name: Ident,
    pub generics: Generics,
    pub params: Vec<Param>,
    pub ret: Type,
    pub body: Block,
}

#[derive(Debug, Clone, derivative::Derivative)]
#[derivative(Hash)]
pub struct Block {
    #[derivative(Hash = "ignore")]
    pub span: Span,
    pub stmts: Vec<Stmt>,
}

#[derive(Debug, Clone, derivative::Derivative)]
#[derivative(Hash)]
pub struct Stmt {
    #[derivative(Hash = "ignore")]
    pub span: Span,
    pub kind: StmtKind,
}

#[derive(Debug, Clone, Hash)]
pub enum StmtKind {
    Item(Item),
    Semi(Expr),
    Expr(Expr),
}

#[derive(Debug, Clone, derivative::Derivative)]
#[derivative(Hash)]
pub struct Expr {
    #[derivative(Hash = "ignore")]
    pub span: Span,
    pub kind: ExprKind,
}

#[derive(Debug, Clone, Hash)]
pub enum ExprKind {
    Ident {
        name: Ident,
    },
    Apply {
        expr: Box<Expr>,
        args: Vec<Type>,
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
    Range {
        lo: Box<Expr>,
        hi: Box<Expr>,
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
    Box {
        expr: Box<Expr>,
    },
    Unbox {
        expr: Box<Expr>,
    },
    Assign {
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    AssignOp {
        op: BinOp,
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

#[derive(Debug, Clone, derivative::Derivative)]
#[derivative(Hash)]
pub struct InitField {
    #[derivative(Hash = "ignore")]
    pub span: Span,
    #[derivative(Hash(hash_with = "hash_ident"))]
    pub name: Ident,
    pub value: Expr,
}

#[derive(Debug, Clone, derivative::Derivative)]
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

#[derive(Debug, Clone, derivative::Derivative)]
#[derivative(Hash)]
pub struct Type {
    #[derivative(Hash = "ignore")]
    pub span: Span,
    pub kind: TypeKind,
}

#[derive(Debug, Clone, derivative::Derivative)]
#[derivative(Hash)]
pub enum TypeKind {
    Infer,
    Parens {
        inner: Box<Type>,
    },
    Path {
        #[derivative(Hash(hash_with = "hash_ident"))]
        module: Ident,
        #[derivative(Hash(hash_with = "hash_ident"))]
        name: Ident,
    },
    Ident {
        #[derivative(Hash(hash_with = "hash_ident"))]
        name: Ident,
    },
    Func {
        params: Vec<TypeParam>,
        ret: Box<Type>,
    },
    Ptr {
        kind: PtrKind,
        ty: Box<Type>,
    },
    Array {
        of: Box<Type>,
        len: usize,
    },
    Slice {
        of: Box<Type>,
    },
    Tuple {
        tys: Vec<Type>,
    },
    Subst {
        ty: Box<Type>,
        args: Vec<Type>,
    },
    Forall {
        gen: Generics,
        ty: Box<Type>,
    },
}

#[derive(Debug, Clone, derivative::Derivative)]
#[derivative(Hash)]
pub struct TypeParam {
    #[derivative(Hash = "ignore")]
    pub span: Span,
    #[derivative(Hash(hash_with = "hash_ident"))]
    pub name: Ident,
    pub ty: Type,
}

#[derive(Debug, Clone, Copy, Hash)]
pub enum PtrKind {
    Single,
    Multiple(bool),
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
