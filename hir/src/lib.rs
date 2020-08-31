#![feature(label_break_value)]

pub mod convert;
pub mod lang;
mod printing;
pub mod resolve;

pub use diagnostics::Span;
pub use resolve::{PrimTy, PrimVal, Res};
use std::collections::BTreeMap;
use std::hash::{Hash, Hasher};
pub use syntax::ast::{Abi, AttrKind, Attribute, BinOp, Ident, Literal, Symbol, UnOp};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ItemId(u64);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Id(ItemId, u64);

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
    pub attrs: Vec<Attribute>,
    pub name: Ident,
    pub kind: ItemKind,
}

#[derive(Debug, Hash)]
pub enum ItemKind {
    Extern {
        abi: Abi,
        ty: Id,
    },
    Func {
        generics: Generics,
        params: Vec<Id>,
        ret: Id,
        body: Block,
    },
    Param {
        ty: Id,
    },
    Var {
        global: bool,
        ty: Id,
        val: Option<Id>,
    },
    Struct {
        generics: Generics,
        fields: Vec<StructField>,
    },
    Enum {
        generics: Generics,
        variants: Vec<EnumVariant>,
    },
    Cons {
        item: Id,
        variant: usize,
        params: Option<Vec<StructField>>,
    },
}

#[derive(Debug, Clone)]
pub struct Generics {
    pub span: Span,
    pub params: Vec<Generic>,
}

#[derive(Debug, Clone)]
pub struct Generic {
    pub span: Span,
    pub id: Id,
    pub name: Ident,
}

#[derive(Debug, Clone)]
pub struct StructField {
    pub span: Span,
    pub name: Ident,
    pub ty: Id,
}

#[derive(Debug, Clone)]
pub struct EnumVariant {
    pub span: Span,
    pub name: Ident,
    pub ctor: Id,
    pub fields: Option<Vec<StructField>>,
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

#[derive(Debug, Hash)]
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

#[derive(Debug, Hash)]
pub enum ExprKind {
    Err,
    Path {
        res: Res,
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

#[derive(Debug, Hash)]
pub enum TypeKind {
    Err,
    Infer,
    Path { res: Res },
    Ref { mut_: bool, to: Id },
    Func { params: Vec<TypeParam>, ret: Id },
    Tuple { tys: Vec<Id> },
    Array { of: Id, len: usize },
    Slice { of: Id },
    Subst { ty: Id, args: Vec<Id> },
}

#[derive(Debug)]
pub struct TypeParam {
    pub span: Span,
    pub name: Ident,
    pub ty: Id,
}

impl ItemId {
    pub fn new<T: Hash>(src: &T) -> Self {
        let mut hasher = seahash::SeaHasher::new();

        src.hash(&mut hasher);

        ItemId(hasher.finish())
    }

    pub const fn is_null(&self) -> bool {
        self.0 == 0
    }

    pub const fn as_u32(&self) -> u32 {
        (self.0 >> 32) as u32 & (self.0 & 0x00000000FFFFFFFF) as u32
    }
}

impl Id {
    pub const fn item(item_id: ItemId) -> Self {
        Id(item_id, 0)
    }

    pub const fn item_id(&self) -> ItemId {
        self.0
    }
}

impl Hash for Item {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state);
        self.name.symbol.hash(state);
        self.kind.hash(state);
    }
}

impl Hash for Generics {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.params.hash(state);
    }
}

impl Hash for Generic {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state);
        self.name.symbol.hash(state);
    }
}

impl Hash for StructField {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.symbol.hash(state);
        self.ty.hash(state);
    }
}

impl Hash for EnumVariant {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.symbol.hash(state);
        self.ctor.hash(state);
        self.fields.hash(state);
    }
}

impl Hash for Block {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.stmts.hash(state);
    }
}

impl Hash for Stmt {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.kind.hash(state);
    }
}

impl Hash for Expr {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.kind.hash(state);
    }
}

impl Hash for Arg {
    fn hash<H: Hasher>(&self, state: &mut H) {
        std::mem::discriminant(&self.name).hash(state);

        if let Some(name) = &self.name {
            name.symbol.hash(state);
        }

        self.value.hash(state);
    }
}

impl Hash for Type {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.kind.hash(state);
    }
}

impl Hash for TypeParam {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.symbol.hash(state);
        self.ty.hash(state);
    }
}
