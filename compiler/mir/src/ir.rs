pub use hir::ir::DefId;
use index_vec::IndexVec;

pub type Ty<'tcx> = std::marker::PhantomData<&'tcx ()>;

pub struct Module<'tcx> {
    pub bodies: Vec<Body<'tcx>>,
}

pub struct Body<'tcx> {
    pub def: DefId,
    pub locals: IndexVec<Local, LocalData<'tcx>>,
    pub blocks: IndexVec<Block, BlockData<'tcx>>,
}

index_vec::define_index_type! {
    pub struct Local = u32;
}

pub struct LocalData<'tcx> {
    pub id: Local,
    pub ty: Ty<'tcx>,
    pub kind: LocalKind,
}

pub enum LocalKind {
    Ret,
    Arg,
    Tmp,
    Var,
}

index_vec::define_index_type! {
    pub struct Block = u32;
}

pub struct BlockData<'tcx> {
    pub id: Block,
    pub stmts: Vec<Stmt<'tcx>>,
    pub term: Term<'tcx>,
}

pub enum Stmt<'tcx> {
    Nop,
    VarLive(Local),
    VarDead(Local),
    Assign(Place<'tcx>, RValue<'tcx>),
    SetDiscr(Place<'tcx>, usize),
}

pub enum Term<'tcx> {
    Unset,
    Abort,
    Return,
    Jump(Block),
    Call(Place<'tcx>, Operand<'tcx>, Vec<Operand<'tcx>>, Block),
    Switch(Operand<'tcx>, Vec<u128>, Vec<Block>),
}

pub enum RValue<'tcx> {
    Use(Operand<'tcx>),
    Ref(Place<'tcx>),
    Discr(Place<'tcx>),
    Init(Ty<'tcx>, Vec<Operand<'tcx>>),
}

pub enum Operand<'tcx> {
    Copy(Place<'tcx>),
    Move(Place<'tcx>),
    Const(Const, Ty<'tcx>),
}

pub struct Place<'tcx> {
    pub base: PlaceBase,
    pub elems: Vec<PlaceElem<'tcx>>,
}

pub enum PlaceBase {
    Local(Local),
    Static(DefId),
}

pub enum PlaceElem<'tcx> {
    Deref,
    Field(usize),
    Index(Operand<'tcx>),
    Downcast(usize),
}

pub enum Const {
    Undefined,
    Ref(Box<Const>),
    Tuple(Vec<Const>),
    Array(Vec<Const>),
    Scalar(u128),
    FuncAddr(DefId),
    Bytes(Box<[u8]>),
}
