mod debug;

pub use codespan::Span;
use data_structures::stable_hasher;
pub use source::LibId;
use std::collections::BTreeMap;
pub use syntax::ast::{AttrArg, Attribute, InfixOp, PostfixOp, PrefixOp};
pub use syntax::symbol::{Ident, Symbol};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ModuleId(u64, u64);

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct DefIndex(u64, u64);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum DefPath {
    Value(Symbol),
    Type(Symbol),
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct DefId {
    pub lib: LibId,
    pub index: DefIndex,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct HirId {
    pub owner: DefId,
    pub local_id: LocalId,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LocalId(pub u32);

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BodyId(pub HirId);

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct IfaceItemId(pub HirId);

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ImplItemId(pub HirId);

#[derive(PartialEq, Eq)]
pub struct Module {
    pub id: ModuleId,
    pub span: Span,
    pub attrs: Vec<Attribute>,
    pub name: Ident,
    pub exports: Vec<Export>,
    pub items: BTreeMap<HirId, Item>,
    pub iface_items: BTreeMap<IfaceItemId, IfaceItem>,
    pub impl_items: BTreeMap<ImplItemId, ImplItem>,
    pub bodies: BTreeMap<BodyId, Body>,
    pub body_ids: Vec<BodyId>,
}

#[derive(PartialEq, Eq)]
pub struct Export {
    pub name: Symbol,
    pub res: Res,
    pub module: source::FileId,
    pub ns: crate::resolve::Ns,
    pub group: Option<Vec<Export>>,
}

#[derive(PartialEq, Eq)]
pub struct Item {
    pub id: HirId,
    pub span: Span,
    pub attrs: Vec<Attribute>,
    pub name: Ident,
    pub kind: ItemKind,
}

#[derive(Debug, PartialEq, Eq)]
pub enum ItemKind {
    Func {
        ty: Type,
        body: BodyId,
    },
    Const {
        ty: Type,
        body: BodyId,
    },
    Static {
        ty: Type,
        body: BodyId,
    },
    Alias {
        kind: Type,
        vars: Vec<TypeVar>,
        value: Type,
    },
    Data {
        head: DataHead,
        body: Vec<DataCtor>,
    },
    Iface {
        head: IfaceHead,
        body: IfaceBody,
    },
    Impl {
        chain: Vec<HirId>,
        index: usize,
        head: ImplHead,
        body: ImplBody,
    },
}

#[derive(PartialEq, Eq)]
pub struct DataHead {
    pub id: HirId,
    pub span: Span,
    pub vars: Vec<TypeVar>,
    pub kind: Type,
}

#[derive(PartialEq, Eq)]
pub struct DataCtor {
    pub id: HirId,
    pub span: Span,
    pub name: Ident,
    pub tys: Vec<Type>,
}

#[derive(PartialEq, Eq)]
pub struct IfaceHead {
    pub id: HirId,
    pub span: Span,
    pub parent: Vec<Constraint>,
    pub vars: Vec<TypeVar>,
}

#[derive(PartialEq, Eq)]
pub struct IfaceBody {
    pub id: HirId,
    pub span: Span,
    pub items: Vec<IfaceItemRef>,
}

#[derive(PartialEq, Eq)]
pub struct IfaceItemRef {
    pub id: IfaceItemId,
    pub span: Span,
    pub name: Ident,
    pub kind: AssocItemKind,
}

#[derive(PartialEq, Eq)]
pub struct ImplHead {
    pub id: HirId,
    pub span: Span,
    pub cs: Vec<Constraint>,
    pub iface: DefId,
    pub tys: Vec<Type>,
}

#[derive(PartialEq, Eq)]
pub struct ImplBody {
    pub id: HirId,
    pub span: Span,
    pub items: Vec<ImplItemRef>,
}

#[derive(PartialEq, Eq)]
pub struct ImplItemRef {
    pub id: ImplItemId,
    pub span: Span,
    pub name: Ident,
    pub kind: AssocItemKind,
}

#[derive(Debug, PartialEq, Eq)]
pub enum AssocItemKind {
    Func,
}

#[derive(PartialEq, Eq)]
pub struct IfaceItem {
    pub id: HirId,
    pub span: Span,
    pub name: Ident,
    pub kind: IfaceItemKind,
}

#[derive(Debug, PartialEq, Eq)]
pub enum IfaceItemKind {
    Func { ty: Type },
}

#[derive(PartialEq, Eq)]
pub struct ImplItem {
    pub id: HirId,
    pub span: Span,
    pub name: Ident,
    pub kind: ImplItemKind,
}

#[derive(Debug, PartialEq, Eq)]
pub enum ImplItemKind {
    Func { ty: Type, body: BodyId },
}

#[derive(PartialEq, Eq)]
pub struct Body {
    pub id: BodyId,
    pub params: Vec<Param>,
    pub value: Expr,
}

#[derive(PartialEq, Eq)]
pub struct Param {
    pub id: HirId,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Res {
    Error,
    Def(DefKind, DefId),
    Local(HirId),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DefKind {
    Func,
    Const,
    Static,
    Alias,
    Data,
    Ctor,
    Iface,
}

#[derive(PartialEq, Eq)]
pub struct Pat {
    pub id: HirId,
    pub span: Span,
    pub kind: PatKind,
}

#[derive(Debug, PartialEq, Eq)]
pub enum PatKind {
    Error,
    Wildcard,
    Int { val: u128 },
    Float { bits: u64 },
    Char { val: char },
    Str { val: String },
    Bind { name: Ident, sub: Option<Box<Pat>> },
    Ctor { ctor: DefId, pats: Vec<Pat> },
    Array { pats: Vec<Pat> },
    Tuple { pats: Vec<Pat> },
    Record { fields: Vec<RecordField<Pat>> },
    Typed { pat: Box<Pat>, ty: Type },
}

#[derive(PartialEq, Eq)]
pub struct RecordField<T> {
    pub id: HirId,
    pub span: Span,
    pub name: Ident,
    pub val: T,
}

#[derive(PartialEq, Eq)]
pub enum Guarded {
    Unconditional(Expr),
    Guarded(Vec<GuardedExpr>),
}

#[derive(PartialEq, Eq)]
pub struct GuardedExpr {
    pub id: HirId,
    pub span: Span,
    pub guard: Expr,
    pub val: Expr,
}

#[derive(PartialEq, Eq)]
pub struct Expr {
    pub id: HirId,
    pub span: Span,
    pub kind: ExprKind,
}

#[derive(Debug, PartialEq, Eq)]
pub enum ExprKind {
    Error,
    Hole {
        name: Ident,
    },
    Ident {
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
        bindings: Vec<Binding>,
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

#[derive(PartialEq, Eq)]
pub struct Block {
    pub id: HirId,
    pub span: Span,
    pub stmts: Vec<Stmt>,
}

#[derive(PartialEq, Eq)]
pub struct Stmt {
    pub id: HirId,
    pub span: Span,
    pub kind: StmtKind,
}

#[derive(Debug, PartialEq, Eq)]
pub enum StmtKind {
    Discard { expr: Expr },
    Bind { binding: Binding },
}

#[derive(PartialEq, Eq)]
pub struct Binding {
    pub id: HirId,
    pub span: Span,
    pub pat: Pat,
    pub val: Expr,
    pub ty: Type,
}

#[derive(PartialEq, Eq)]
pub struct CaseArm {
    pub id: HirId,
    pub span: Span,
    pub pats: Vec<Pat>,
    pub val: Guarded,
}

#[derive(PartialEq, Eq)]
pub struct Type {
    pub id: HirId,
    pub span: Span,
    pub kind: TypeKind,
}

#[derive(Debug, PartialEq, Eq)]
pub enum TypeKind {
    Error,
    Infer,
    Hole { name: Ident },
    Ident { res: Res },
    Int { val: u128 },
    App { base: Box<Type>, args: Vec<Type> },
    Tuple { tys: Vec<Type> },
    Record { row: Row },
    Func { params: Vec<Type>, ret: Box<Type> },
    Forall { vars: Vec<TypeVar>, ty: Box<Type> },
    Cons { cs: Constraint, ty: Box<Type> },
    Kinded { ty: Box<Type>, kind: Box<Type> },
}

#[derive(PartialEq, Eq)]
pub struct Row {
    pub id: HirId,
    pub span: Span,
    pub fields: Vec<RowField>,
    pub tail: Option<Box<Type>>,
}

#[derive(PartialEq, Eq)]
pub struct RowField {
    pub id: HirId,
    pub span: Span,
    pub name: Ident,
    pub ty: Type,
}

#[derive(PartialEq, Eq)]
pub struct TypeVar {
    pub id: HirId,
    pub span: Span,
    pub name: Ident,
    pub kind: Type,
}

#[derive(PartialEq, Eq)]
pub struct Constraint {
    pub id: HirId,
    pub span: Span,
    pub iface: DefId,
    pub tys: Vec<Type>,
}

impl ModuleId {
    pub fn from_name(module: Symbol) -> Self {
        use stable_hasher::HashStable;
        let mut hasher = stable_hasher::StableHasher::new();
        module.hash_stable(&mut (), &mut hasher);
        hasher.finish()
    }
}

impl stable_hasher::StableHasherResult for ModuleId {
    fn finish(hasher: stable_hasher::StableHasher) -> Self {
        let (_0, _1) = hasher.finalize();
        ModuleId(_0, _1)
    }
}

impl DefId {
    pub fn new(lib: LibId, index: DefIndex) -> Self {
        DefId { lib, index }
    }

    pub fn dummy() -> Self {
        DefId {
            lib: LibId(0),
            index: DefIndex(0, 0),
        }
    }
}

impl DefIndex {
    pub fn from_path(module: Symbol, path: &[DefPath]) -> Self {
        use stable_hasher::HashStable;
        let mut hasher = stable_hasher::StableHasher::new();
        (module, path).hash_stable(&mut (), &mut hasher);
        hasher.finish()
    }
}

impl stable_hasher::StableHasherResult for DefIndex {
    fn finish(hasher: stable_hasher::StableHasher) -> Self {
        let (_0, _1) = hasher.finalize();
        DefIndex(_0, _1)
    }
}

impl<CTX> stable_hasher::HashStable<CTX> for DefPath {
    fn hash_stable(&self, ctx: &mut CTX, hasher: &mut stable_hasher::StableHasher) {
        std::mem::discriminant(self).hash_stable(ctx, hasher);

        match self {
            DefPath::Value(s) | DefPath::Type(s) => {
                s.hash_stable(ctx, hasher);
            }
        }
    }
}
