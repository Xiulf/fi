mod debug;

pub use codespan::Span;
use data_structures::stable_hasher;
use serde::{Deserialize, Serialize};
pub use source::LibId;
use std::collections::BTreeMap;
pub use syntax::ast::{Assoc, AttrArg, Attribute, ForeignKind, Prec};
pub use syntax::symbol::{Ident, Symbol};

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct ModuleId(u64, u64);

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct DefIndex(u64, u64);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum DefPath {
    Value(Symbol),
    Type(Symbol),
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct DefId {
    pub lib: LibId,
    pub module: ModuleId,
    pub index: DefIndex,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct HirId {
    pub owner: DefId,
    pub local_id: LocalId,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct LocalId(pub u32);

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BodyId(pub HirId);

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ClassItemId(pub HirId);

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct InstanceItemId(pub HirId);

#[derive(PartialEq, Eq)]
pub struct Module {
    pub id: ModuleId,
    pub span: Span,
    pub attrs: Vec<Attribute>,
    pub name: Ident,
    pub exports: Vec<Export>,
    pub imports: Vec<DefId>,
    pub items: BTreeMap<HirId, Item>,
    pub class_items: BTreeMap<ClassItemId, ClassItem>,
    pub instance_items: BTreeMap<InstanceItemId, InstanceItem>,
    pub bodies: BTreeMap<BodyId, Body>,
    pub body_ids: Vec<BodyId>,
}

#[derive(Clone, PartialEq, Eq, Serialize, Deserialize)]
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
    Foreign {
        ty: Type,
        kind: ForeignKind,
    },
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
    Fixity {
        assoc: Assoc,
        prec: Prec,
        func: Res,
    },
    Alias {
        kind: Type,
        vars: Vec<TypeVar>,
        value: Type,
    },
    Data {
        head: DataHead,
        body: Vec<HirId>,
    },
    DataCtor {
        data: HirId,
        tys: Vec<Type>,
    },
    Class {
        head: ClassHead,
        body: ClassBody,
    },
    Instance {
        chain: Vec<HirId>,
        index: usize,
        head: InstanceHead,
        body: InstanceBody,
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
pub struct ClassHead {
    pub id: HirId,
    pub span: Span,
    pub parent: Vec<Constraint>,
    pub vars: Vec<TypeVar>,
    pub fundeps: Vec<FunDep>,
}

#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct FunDep {
    pub determiners: Vec<usize>,
    pub determined: Vec<usize>,
}

#[derive(PartialEq, Eq)]
pub struct ClassBody {
    pub id: HirId,
    pub span: Span,
    pub items: Vec<ClassItemRef>,
}

#[derive(PartialEq, Eq)]
pub struct ClassItemRef {
    pub id: ClassItemId,
    pub span: Span,
    pub name: Ident,
    pub kind: AssocItemKind,
}

#[derive(Clone, PartialEq, Eq)]
pub struct InstanceHead {
    pub id: HirId,
    pub span: Span,
    pub cs: Vec<Constraint>,
    pub class: DefId,
    pub tys: Vec<Type>,
}

#[derive(PartialEq, Eq)]
pub struct InstanceBody {
    pub id: HirId,
    pub span: Span,
    pub items: Vec<InstanceItemRef>,
}

#[derive(PartialEq, Eq)]
pub struct InstanceItemRef {
    pub id: InstanceItemId,
    pub span: Span,
    pub name: Ident,
    pub kind: AssocItemKind,
}

#[derive(Debug, PartialEq, Eq)]
pub enum AssocItemKind {
    Func,
}

#[derive(PartialEq, Eq)]
pub struct ClassItem {
    pub id: HirId,
    pub owner: HirId,
    pub span: Span,
    pub name: Ident,
    pub kind: ClassItemKind,
}

#[derive(Debug, PartialEq, Eq)]
pub enum ClassItemKind {
    Func { ty: Type },
}

#[derive(PartialEq, Eq)]
pub struct InstanceItem {
    pub id: HirId,
    pub owner: HirId,
    pub span: Span,
    pub name: Ident,
    pub kind: InstanceItemKind,
}

#[derive(Debug, PartialEq, Eq)]
pub enum InstanceItemKind {
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
    pub span: Span,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub enum Res {
    Error,
    Def(DefKind, DefId),
    Local(HirId),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub enum DefKind {
    Func,
    Const,
    Static,
    Fixity,
    Alias,
    Data,
    Ctor,
    Class,
    Instance,
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
    Hole { name: Ident },
    Ident { name: Ident, res: Res },
    Int { val: u128 },
    Float { bits: u64 },
    Char { val: char },
    Str { val: String },
    App { base: Box<Expr>, arg: Box<Expr> },
    Array { exprs: Vec<Expr> },
    Tuple { exprs: Vec<Expr> },
    Record { fields: Vec<RecordField<Expr>> },
    Field { base: Box<Expr>, field: Ident },
    Index { base: Box<Expr>, index: Box<Expr> },
    Assign { lhs: Box<Expr>, rhs: Box<Expr> },
    Let { bindings: Vec<Binding>, body: Box<Expr> },
    If { cond: Box<Expr>, then: Box<Expr>, else_: Box<Expr> },
    Case { pred: Vec<Expr>, arms: Vec<CaseArm> },
    Do { block: Block },
    Typed { expr: Box<Expr>, ty: Type },
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

#[derive(Clone, PartialEq, Eq)]
pub struct Type {
    pub id: HirId,
    pub span: Span,
    pub kind: TypeKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeKind {
    Error,
    Infer,
    Hole { name: Ident },
    Ident { res: Res },
    Int { val: u128 },
    Str { val: String },
    App { base: Box<Type>, arg: Box<Type> },
    Tuple { tys: Vec<Type> },
    Record { row: Row },
    Func { param: Box<Type>, ret: Box<Type> },
    Forall { vars: Vec<TypeVar>, ty: Box<Type> },
    Cons { cs: Constraint, ty: Box<Type> },
    Kinded { ty: Box<Type>, kind: Box<Type> },
}

#[derive(Clone, PartialEq, Eq)]
pub struct Row {
    pub id: HirId,
    pub span: Span,
    pub fields: Vec<RowField>,
    pub tail: Option<Box<Type>>,
}

#[derive(Clone, PartialEq, Eq)]
pub struct RowField {
    pub id: HirId,
    pub span: Span,
    pub name: Ident,
    pub ty: Type,
}

#[derive(Clone, PartialEq, Eq)]
pub struct TypeVar {
    pub id: HirId,
    pub span: Span,
    pub name: Ident,
    pub kind: Type,
}

#[derive(Clone, PartialEq, Eq)]
pub struct Constraint {
    pub id: HirId,
    pub span: Span,
    pub trait_: DefId,
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
    pub fn new(lib: LibId, module: ModuleId, index: DefIndex) -> Self {
        DefId { lib, module, index }
    }

    pub fn dummy() -> Self {
        DefId {
            lib: LibId(0, 0),
            module: ModuleId(0, 0),
            index: DefIndex(0, 0),
        }
    }
}

impl Into<HirId> for DefId {
    fn into(self) -> HirId {
        HirId {
            owner: self,
            local_id: LocalId(0),
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
            | DefPath::Value(s) | DefPath::Type(s) => {
                s.hash_stable(ctx, hasher);
            },
        }
    }
}

pub enum Def<'a> {
    Item(&'a Item),
    ClassItem(&'a ClassItem),
    InstanceItem(&'a InstanceItem),
}

impl Module {
    pub fn def(&self, id: DefId) -> Def {
        let hir_id = HirId {
            owner: id,
            local_id: LocalId(0),
        };

        if let Some(item) = self.items.get(&hir_id) {
            Def::Item(item)
        } else if let Some(trait_item) = self.class_items.get(&ClassItemId(hir_id)) {
            Def::ClassItem(trait_item)
        } else {
            Def::InstanceItem(&self.instance_items[&InstanceItemId(hir_id)])
        }
    }

    pub fn out_type(&self) -> Option<&str> {
        let out_type = Symbol::new("out_type");

        self.attrs
            .iter()
            .filter_map(|a| if a.name.symbol == out_type { a.str_arg() } else { None })
            .next()
    }
}

impl std::ops::Index<BodyId> for Module {
    type Output = Body;

    fn index(&self, id: BodyId) -> &Self::Output {
        &self.bodies[&id]
    }
}

impl Item {
    pub fn is_intrinsic(&self) -> bool {
        let intrinsic = Symbol::new("intrinsic");

        self.attrs.iter().any(|a| a.name.symbol == intrinsic)
    }

    pub fn is_main(&self) -> bool {
        let main = Symbol::new("main");

        self.attrs.iter().any(|a| a.name.symbol == main)
    }

    pub fn is_no_mangle(&self) -> bool {
        let no_mangle = Symbol::new("no_mangle");

        self.attrs.iter().any(|a| a.name.symbol == no_mangle)
    }

    pub fn repr(&self) -> Option<&str> {
        let repr = Symbol::new("repr");

        self.attrs.iter().filter_map(|a| if a.name.symbol == repr { a.str_arg() } else { None }).next()
    }

    pub fn abi(&self) -> Option<&str> {
        let repr = Symbol::new("abi");

        self.attrs.iter().filter_map(|a| if a.name.symbol == repr { a.str_arg() } else { None }).next()
    }

    pub fn fixity(&self) -> (Assoc, Prec, Res) {
        match self.kind {
            | ItemKind::Fixity { assoc, prec, func } => (assoc, prec, func),
            | _ => unreachable!(),
        }
    }

    pub fn data(&self) -> &DataHead {
        match &self.kind {
            | ItemKind::Data { head, .. } => head,
            | _ => unreachable!(),
        }
    }

    pub fn data_body(&self) -> &[HirId] {
        match &self.kind {
            | ItemKind::Data { body, .. } => body,
            | _ => unreachable!(),
        }
    }

    pub fn ctor(&self) -> (HirId, &[Type]) {
        match &self.kind {
            | ItemKind::DataCtor { data, tys } => (*data, tys),
            | _ => unreachable!(),
        }
    }

    pub fn class(&self) -> &ClassHead {
        match &self.kind {
            | ItemKind::Class { head, .. } => head,
            | _ => unreachable!(),
        }
    }

    pub fn class_body(&self) -> &ClassBody {
        match &self.kind {
            | ItemKind::Class { body, .. } => body,
            | _ => unreachable!(),
        }
    }

    pub fn instance(&self) -> &InstanceHead {
        match &self.kind {
            | ItemKind::Instance { head, .. } => head,
            | _ => unreachable!(),
        }
    }

    pub fn instance_body(&self) -> &InstanceBody {
        match &self.kind {
            | ItemKind::Instance { body, .. } => body,
            | _ => unreachable!(),
        }
    }
}

impl Def<'_> {
    pub fn name(&self) -> Ident {
        match self {
            | Def::Item(item) => item.name,
            | Def::ClassItem(item) => item.name,
            | Def::InstanceItem(item) => item.name,
        }
    }

    pub fn span(&self) -> Span {
        match self {
            | Def::Item(item) => item.span,
            | Def::ClassItem(item) => item.span,
            | Def::InstanceItem(item) => item.span,
        }
    }
}
