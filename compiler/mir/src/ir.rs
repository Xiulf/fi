use crate::layout::Layout;
use hir::arena::{Arena, Idx};
use std::sync::Arc;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Body {
    pub locals: Arena<Local>,
    pub blocks: Arena<Block>,
}

pub type LocalId = Idx<Local>;
pub type BlockId = Idx<Block>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Local {
    pub layout: Arc<Layout>,
    pub kind: LocalKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LocalKind {
    Ret,
    Arg,
    Var,
    Tmp,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub term: Term,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Stmt {
    Assign(Place, RValue),
    SetDiscr(Place, u128),
    Call(Place, Operand, Vec<Operand>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Term {
    Abort,
    Return,
    Jump(BlockId),
    Switch(Operand, Vec<u128>, Vec<BlockId>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum RValue {
    Use(Operand),
    AddrOf(Place),
    GetDiscr(Place),
    Intrinsic(String, Vec<Operand>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Operand {
    Place(Place),
    Const(Const),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Place {
    pub local: LocalId,
    pub elems: Vec<PlaceElem>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum PlaceElem {
    Deref,
    Field(usize),
    Index(Operand),
    Downcast(usize),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Const {
    Undefined(Arc<Layout>),
    Scalar(u128, Arc<Layout>),
    FuncAddr(hir::Func),
    StaticAddr(hir::Static),
}
