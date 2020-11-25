pub mod display;

pub use check::ty::Ty;
pub use hir::ir::DefId;
use index_vec::IndexVec;

#[derive(Debug, PartialEq, Eq)]
pub struct Module {
    pub bodies: Vec<Body>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Body {
    pub def: DefId,
    pub locals: IndexVec<Local, LocalData>,
    pub blocks: IndexVec<Block, BlockData>,
}

index_vec::define_index_type! {
    pub struct Local = u32;
}

#[derive(Debug, PartialEq, Eq)]
pub struct LocalData {
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

#[derive(Debug, PartialEq, Eq)]
pub enum Operand {
    Copy(Place),
    Move(Place),
    Const(Const, Ty),
}

#[derive(Debug, PartialEq, Eq)]
pub struct Place {
    pub base: PlaceBase,
    pub elems: Vec<PlaceElem>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum PlaceBase {
    Local(Local),
    Static(DefId),
}

#[derive(Debug, PartialEq, Eq)]
pub enum PlaceElem {
    Deref,
    Field(usize),
    Index(Operand),
    Downcast(usize),
}

#[derive(Debug, PartialEq, Eq)]
pub enum Const {
    Undefined,
    Ref(Box<Const>),
    Tuple(Vec<Const>),
    Array(Vec<Const>),
    Scalar(u128),
    FuncAddr(DefId),
    Bytes(Box<[u8]>),
}
