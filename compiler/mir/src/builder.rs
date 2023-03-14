use arena::Arena;
use hir_def::id::CtorId;
use triomphe::Arc;

use crate::ir::{
    BinOp, Block, BlockData, Body, CastKind, JumpTarget, Local, LocalData, LocalKind, NullOp, Operand, Place,
    Projection, RValue, Stmt, Term,
};
use crate::repr::Repr;
use crate::Db;

#[derive(Default)]
pub struct Builder {
    locals: Arena<LocalData>,
    blocks: Arena<BlockData>,
    block: Option<Block>,
}

#[derive(Default)]
pub struct SwitchBuilder {
    values: Vec<i128>,
    targets: Vec<JumpTarget>,
}

impl Builder {
    pub fn build(self, db: &dyn Db) -> Body {
        Body::new(db, self.locals, self.blocks)
    }

    pub fn current_block(&self) -> Block {
        self.block.unwrap()
    }

    pub fn create_block(&mut self) -> Block {
        Block(self.blocks.alloc(BlockData {
            params: Vec::new(),
            stmts: Vec::new(),
            term: Term::None,
        }))
    }

    pub fn switch_block(&mut self, block: Block) {
        self.block = Some(block);
    }

    pub fn add_block_param(&mut self, block: Block, param: Local) {
        self.blocks[block.0].params.push(param);
    }

    pub fn add_local(&mut self, kind: LocalKind, repr: Arc<Repr>) -> Local {
        Local(self.locals.alloc(LocalData { kind, repr }))
    }

    fn block(&self) -> &BlockData {
        &self.blocks[self.block.unwrap().0]
    }

    fn block_mut(&mut self) -> &mut BlockData {
        &mut self.blocks[self.block.unwrap().0]
    }

    fn stmt(&mut self, stmt: Stmt) {
        self.block_mut().stmts.push(stmt);
    }

    fn term(&mut self, term: Term) {
        let block = self.block_mut();

        if let Term::None = block.term {
            block.term = term;
        }
    }

    pub fn unreachable(&mut self) {
        self.term(Term::Unreachable);
    }

    pub fn abort(&mut self) {
        self.term(Term::Abort);
    }

    pub fn ret(&mut self, op: impl Into<Operand>) {
        self.term(Term::Return(op.into()));
    }

    pub fn jump(&mut self, target: impl Into<JumpTarget>) {
        self.term(Term::Jump(target.into()));
    }

    pub fn switch(&mut self) -> SwitchBuilder {
        SwitchBuilder::default()
    }

    pub fn init(&mut self, local: Local) {
        self.stmt(Stmt::Init(local));
    }

    pub fn drop(&mut self, place: Place) {
        self.stmt(Stmt::Drop(place));
    }

    pub fn set_discriminant(&mut self, place: Place, ctor: CtorId) {
        self.stmt(Stmt::SetDiscriminant(place, ctor));
    }

    pub fn intrinsic(&mut self, place: Place, name: String, args: impl Into<Vec<Operand>>) {
        let args = args.into();
        self.stmt(Stmt::Intrinsic { place, name, args });
    }

    pub fn call(&mut self, place: Place, func: impl Into<Operand>, args: impl Into<Vec<Operand>>) {
        let func = func.into();
        let args = args.into();
        self.stmt(Stmt::Call { place, func, args });
    }

    pub fn assign(&mut self, res: Place, op: impl Into<Operand>) {
        self.stmt(Stmt::Assign(res, RValue::Use(op.into())));
    }

    pub fn addrof(&mut self, res: Place, place: Place) {
        self.stmt(Stmt::Assign(res, RValue::AddrOf(place)));
    }

    pub fn get_discriminant(&mut self, res: Place, place: Place) {
        self.stmt(Stmt::Assign(res, RValue::Discriminant(place)));
    }

    pub fn cast(&mut self, res: Place, kind: CastKind, op: impl Into<Operand>) {
        self.stmt(Stmt::Assign(res, RValue::Cast(kind, op.into())));
    }

    pub fn binop(&mut self, res: Place, op: BinOp, lhs: impl Into<Operand>, rhs: impl Into<Operand>) {
        self.stmt(Stmt::Assign(res, RValue::BinOp(op, lhs.into(), rhs.into())));
    }

    pub fn nullop(&mut self, res: Place, op: NullOp, repr: Arc<Repr>) {
        self.stmt(Stmt::Assign(res, RValue::NullOp(op, repr)));
    }
}

impl SwitchBuilder {
    pub fn build(mut self, builder: &mut Builder, discr: impl Into<Operand>, default_branch: impl Into<JumpTarget>) {
        self.targets.push(default_branch.into());
        builder.term(Term::Switch {
            discr: discr.into(),
            values: self.values,
            targets: self.targets,
        });
    }

    pub fn branch(&mut self, value: i128, target: impl Into<JumpTarget>) {
        self.values.push(value);
        self.targets.push(target.into());
    }
}

impl Place {
    pub fn new(local: Local) -> Self {
        Self {
            local,
            projection: Vec::new(),
        }
    }

    pub fn deref(mut self) -> Self {
        self.projection.push(Projection::Deref);
        self
    }

    pub fn field(mut self, index: usize) -> Self {
        self.projection.push(Projection::Field(index));
        self
    }

    pub fn index(mut self, index: impl Into<Operand>) -> Self {
        self.projection.push(Projection::Index(index.into()));
        self
    }

    pub fn slice(mut self, lo: impl Into<Operand>, hi: impl Into<Operand>) -> Self {
        self.projection.push(Projection::Slice(lo.into(), hi.into()));
        self
    }

    pub fn downcast(mut self, ctor: CtorId) -> Self {
        self.projection.push(Projection::Downcast(ctor));
        self
    }
}
