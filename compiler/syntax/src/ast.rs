mod debug;

pub use crate::symbol::{Ident, Symbol};
pub use codespan::Span;
pub use parser::literal::Literal;

#[derive(PartialEq, Eq)]
pub struct Module {
    pub span: Span,
    pub attrs: Vec<Attribute>,
    pub name: Ident,
    pub exports: Exports,
    pub imports: Vec<ImportDecl>,
    pub decls: Vec<Decl>,
}

#[derive(Clone, PartialEq, Eq)]
pub struct Attribute {
    pub span: Span,
    pub name: Ident,
    pub body: Option<AttrBody>,
}

#[derive(Clone, PartialEq, Eq)]
pub struct AttrBody {
    pub span: Span,
    pub args: Vec<AttrArg>,
}

#[derive(Clone, PartialEq, Eq)]
pub enum AttrArg {
    Literal(Literal),
    Field(Ident, Literal),
    Call(Ident, Vec<AttrArg>),
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
    Any,
    Module,
    Group(ExportGroup),
}

#[derive(Debug, PartialEq, Eq)]
pub enum ExportGroup {
    All,
    Some(Vec<Ident>),
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
    pub kind: ImportKind,
}

#[derive(Debug, PartialEq, Eq)]
pub enum ImportKind {
    Any,
    Group(ImportGroup),
}

#[derive(Debug, PartialEq, Eq)]
pub enum ImportGroup {
    All,
    Some(Vec<Ident>),
}

#[derive(PartialEq, Eq)]
pub struct Decl {
    pub span: Span,
    pub attrs: Vec<Attribute>,
    pub name: Ident,
    pub kind: DeclKind,
}

#[derive(PartialEq, Eq)]
pub enum DeclKind {
    Foreign {
        ty: Type,
        kind: ForeignKind,
    },
    FuncTy {
        ty: Type,
    },
    Func {
        pats: Vec<Pat>,
        val: Guarded,
    },
    ConstTy {
        ty: Type,
    },
    Const {
        val: Expr,
    },
    StaticTy {
        ty: Type,
    },
    Static {
        val: Expr,
    },
    AliasKind {
        kind: Type,
    },
    Alias {
        vars: Vec<TypeVar>,
        ty: Type,
    },
    DataKind {
        kind: Type,
    },
    Data {
        head: DataHead,
        body: Option<Vec<DataCtor>>,
    },
    Trait {
        head: TraitHead,
        body: Option<TraitBody>,
    },
    ImplChain {
        impls: Vec<Impl>,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ForeignKind {
    Func,
    Static,
}

#[derive(PartialEq, Eq)]
pub struct DataHead {
    pub span: Span,
    pub vars: Vec<TypeVar>,
}

#[derive(PartialEq, Eq)]
pub struct DataCtor {
    pub span: Span,
    pub name: Ident,
    pub tys: Vec<Type>,
}

#[derive(PartialEq, Eq)]
pub struct TraitHead {
    pub span: Span,
    pub parent: Option<Vec<Constraint>>,
    pub vars: Vec<TypeVar>,
}

#[derive(PartialEq, Eq)]
pub struct TraitBody {
    pub span: Span,
    pub decls: Vec<TraitDecl>,
}

#[derive(PartialEq, Eq)]
pub struct TraitDecl {
    pub span: Span,
    pub name: Ident,
    pub kind: TraitDeclKind,
}

#[derive(PartialEq, Eq)]
pub enum TraitDeclKind {
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
    pub name: Ident,
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
    Int { val: u128 },
    Float { bits: u64 },
    Char { val: char },
    Str { val: String },
    Ident { name: Ident },
    Named { name: Ident, pat: Box<Pat> },
    Ctor { name: Ident, pats: Vec<Pat> },
    Array { pats: Vec<Pat> },
    Tuple { pats: Vec<Pat> },
    Record { fields: Vec<RecordField<Pat>> },
    Typed { pat: Box<Pat>, ty: Type },
}

#[derive(PartialEq, Eq)]
pub enum RecordField<T> {
    Pun { name: Ident },
    Field { name: Ident, val: T },
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
    Hole {
        name: Ident,
    },
    Parens {
        inner: Box<Expr>,
    },
    Ident {
        name: Ident,
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
    Str {
        val: String,
    },
    App {
        base: Box<Expr>,
        args: Vec<Expr>,
    },
    Array {
        exprs: Vec<Expr>,
    },
    Tuple {
        exprs: Vec<Expr>,
    },
    Record {
        fields: Vec<RecordField<Expr>>,
    },
    Field {
        base: Box<Expr>,
        field: Ident,
    },
    Index {
        base: Box<Expr>,
        index: Box<Expr>,
    },
    Assign {
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    Infix {
        op: InfixOp,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    Prefix {
        op: PrefixOp,
        rhs: Box<Expr>,
    },
    Postfix {
        op: PostfixOp,
        lhs: Box<Expr>,
    },
    Let {
        bindings: Vec<LetBinding>,
        body: Box<Expr>,
    },
    If {
        cond: Box<Expr>,
        then: Box<Expr>,
        else_: Box<Expr>,
    },
    Case {
        pred: Vec<Expr>,
        arms: Vec<CaseArm>,
    },
    Loop {
        body: Block,
    },
    While {
        cond: Box<Expr>,
        body: Block,
    },
    Break {},
    Next {},
    Do {
        block: Block,
    },
    Return {
        val: Box<Expr>,
    },
    Typed {
        expr: Box<Expr>,
        ty: Type,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum InfixOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    And,
    Or,
    BitAnd,
    BitOr,
    BitXor,
    Shl,
    Shr,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum PrefixOp {
    Neg,
    Not,
    BitNot,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum PostfixOp {}

#[derive(PartialEq, Eq)]
pub struct Block {
    pub span: Span,
    pub stmts: Vec<Stmt>,
}

#[derive(PartialEq, Eq)]
pub struct Stmt {
    pub span: Span,
    pub kind: StmtKind,
}

#[derive(PartialEq, Eq)]
pub enum StmtKind {
    Discard { expr: Expr },
    Bind { pat: Pat, val: Expr },
}

#[derive(PartialEq, Eq)]
pub struct LetBinding {
    pub span: Span,
    pub kind: LetBindingKind,
}

#[derive(PartialEq, Eq)]
pub enum LetBindingKind {
    Type { name: Ident, ty: Type },
    Value { pat: Pat, val: Expr },
}

#[derive(PartialEq, Eq)]
pub struct CaseArm {
    pub span: Span,
    pub pats: Vec<Pat>,
    pub val: Guarded,
}

#[derive(PartialEq, Eq)]
pub struct Type {
    pub span: Span,
    pub kind: TypeKind,
}

#[derive(PartialEq, Eq)]
pub enum TypeKind {
    Hole { name: Ident },
    Parens { inner: Box<Type> },
    Int { val: u128 },
    Ident { name: Ident },
    Qual { module: Ident, name: Ident },
    App { base: Box<Type>, args: Vec<Type> },
    Tuple { tys: Vec<Type> },
    Func { params: Vec<Type>, ret: Box<Type> },
    Forall { vars: Vec<TypeVar>, ret: Box<Type> },
    Cons { cs: Constraint, ty: Box<Type> },
    Record { row: Row },
    Kinded { ty: Box<Type>, kind: Box<Type> },
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

impl Attribute {
    pub fn str_arg(&self) -> Option<&str> {
        let body = self.body.as_ref()?;

        if let [AttrArg::Literal(Literal::String(s))] = &body.args[..] {
            Some(&s.text)
        } else {
            None
        }
    }
}
