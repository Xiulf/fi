pub mod display;

pub use check::ty::Ty;
pub use hir::ir::{DefId, ModuleId};
use index_vec::IndexVec;

#[derive(Debug, PartialEq, Eq)]
pub struct Module {
    pub foreigns: Vec<Foreign>,
    pub bodies: Vec<Body>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Foreign {
    pub def: DefId,
    pub kind: ForeignKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ForeignKind {
    Func,
    Static,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Body {
    pub def: DefId,
    pub kind: BodyKind,
    pub locals: IndexVec<Local, LocalData>,
    pub blocks: IndexVec<Block, BlockData>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BodyKind {
    Func,
    Static,
    Const,
}

index_vec::define_index_type! {
    pub struct Local = u32;
}

impl Local {
    pub const RET: Self = unsafe { std::mem::transmute(0u32) };
}

#[derive(Debug, PartialEq, Eq)]
pub struct LocalData {
    pub id: Local,
    pub ty: Ty,
    pub kind: LocalKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LocalKind {
    Ret,
    Arg,
    Tmp,
    Var,
}

index_vec::define_index_type! {
    pub struct Block = u32;
}

#[derive(Debug, PartialEq, Eq)]
pub struct BlockData {
    pub id: Block,
    pub stmts: Vec<Stmt>,
    pub term: Term,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Stmt {
    Nop,
    VarLive(Local),
    VarDead(Local),
    Assign(Place, RValue),
    SetDiscr(Place, usize),
}

#[derive(Debug, PartialEq, Eq)]
pub enum Term {
    Unset,
    Abort,
    Return,
    Jump(Block),
    Call(Place, Operand, Vec<Operand>, Block),
    Switch(Operand, Vec<u128>, Vec<Block>),
}

#[derive(Debug, PartialEq, Eq)]
pub enum RValue {
    Use(Operand),
    AddrOf(Place),
    Discr(Place),
    Init(Ty, Vec<Operand>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Operand {
    Copy(Place),
    Move(Place),
    Const(Const, Ty),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Place {
    pub base: PlaceBase,
    pub elems: Vec<PlaceElem>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PlaceBase {
    Local(Local),
    Static(DefId),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PlaceElem {
    Deref,
    Field(usize),
    Index(Operand),
    Downcast(usize),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Const {
    Undefined,
    Ref(Box<Const>),
    Tuple(Vec<Const>),
    Array(Vec<Const>),
    Scalar(u128),
    FuncAddr(DefId),
    Bytes(Box<[u8]>),
}

impl Body {
    pub fn args(&self) -> impl Iterator<Item = &LocalData> {
        self.locals.iter().filter(|l| l.kind == LocalKind::Arg)
    }
}
