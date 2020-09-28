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

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize,
)]
pub struct ItemId(u64);

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize,
)]
pub struct Id(ItemId, u64);

#[derive(Debug)]
pub struct Package {
    pub name: Symbol,
    pub items: BTreeMap<Id, Item>,
    pub exprs: BTreeMap<Id, Expr>,
    pub types: BTreeMap<Id, Type>,
    pub imports: Imports,
}

#[derive(Debug, serde::Serialize, serde::Deserialize)]
#[serde(transparent)]
pub struct Imports(pub BTreeMap<Id, Import>);

#[derive(Debug, serde::Serialize, serde::Deserialize)]
pub struct Import {
    pub id: Id,
    pub name: Ident,
    pub path: String,
    pub symbol: String,
    pub attrs: Vec<Attribute>,
    pub kind: ImportKind,
}

#[derive(Debug, serde::Serialize, serde::Deserialize)]
pub enum ImportKind {
    Extern { abi: Abi },
    Func,
    Var,
    Struct,
    Enum,
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
    Const {
        ty: Id,
        val: Id,
    },
    Struct {
        generics: Generics,
        fields: Vec<StructField>,
    },
    Enum {
        generics: Generics,
        variants: Vec<EnumVariant>,
    },
    Ctor {
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
    Apply {
        expr: Id,
        args: Vec<Id>,
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
    Range {
        lo: Id,
        hi: Id,
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
    Box {
        expr: Id,
    },
    Unbox {
        expr: Id,
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
    Forall { gen: Generics, ty: Id },
}

#[derive(Debug)]
pub struct TypeParam {
    pub span: Span,
    pub name: Ident,
    pub ty: Id,
}

impl Package {
    pub fn collect_exports(&self, module_structure: &resolve::ModuleStructure) -> Imports {
        Imports(
            self.items
                .iter()
                .filter_map(|(id, item)| {
                    let mut path = Vec::new();

                    module_structure.find_path(id, &mut path);

                    let path = path
                        .into_iter()
                        .chain(std::iter::once(module_structure.name))
                        .rev()
                        .map(|s| s.to_string())
                        .collect::<Vec<_>>()
                        .join("/");

                    let symbol = if let ItemKind::Extern { .. } = item.kind {
                        item.name.to_string()
                    } else if item.no_mangle() {
                        item.name.to_string()
                    } else if item.is_main() {
                        String::from("main")
                    } else {
                        mangling::mangle(path.bytes())
                    };

                    Some((
                        *id,
                        Import {
                            id: item.id,
                            name: item.name,
                            path,
                            symbol,
                            attrs: item.attrs.clone(),
                            kind: match item.kind {
                                ItemKind::Extern { abi, .. } => ImportKind::Extern { abi },
                                ItemKind::Func { .. } => ImportKind::Func,
                                ItemKind::Var { global: true, .. } => ImportKind::Var,
                                ItemKind::Struct { .. } => ImportKind::Struct,
                                ItemKind::Enum { .. } => ImportKind::Enum,
                                _ => return None,
                            },
                        },
                    ))
                })
                .collect(),
        )
    }
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

impl Imports {
    pub fn store(&self, path: impl AsRef<std::path::Path>) {
        let file = std::fs::File::create(path).unwrap();

        bincode::serialize_into(file, self).unwrap();
    }

    pub fn load(path: impl AsRef<std::path::Path>) -> Self {
        let file = std::fs::File::open(path).unwrap();

        bincode::deserialize_from(file).unwrap()
    }
}

impl Item {
    pub fn no_mangle(&self) -> bool {
        self.attrs
            .iter()
            .any(|attr| matches!(&attr.kind, AttrKind::NoMangle))
    }

    pub fn is_main(&self) -> bool {
        self.attrs
            .iter()
            .any(|attr| matches!(&attr.kind, AttrKind::Main))
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
