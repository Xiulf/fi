use hir::{Ctor, DefWithBody};

use crate::repr::Repr;
use crate::syntax::*;

pub struct Builder {
    body: BodyData,
    block: Option<Block>,
}

impl Builder {
    pub fn new(origin: BodyOrigin) -> Self {
        Self {
            body: BodyData {
                origin,
                locals: Default::default(),
                blocks: Default::default(),
            },
            block: None,
        }
    }

    pub fn build(self) -> BodyData {
        self.body
    }

    pub fn body(&self) -> &BodyData {
        &self.body
    }

    pub fn origin(&self) -> BodyOrigin {
        self.body.origin
    }

    pub fn current_block(&self) -> Block {
        self.block.unwrap()
    }

    pub fn create_block(&mut self) -> Block {
        let id = Block(self.body.blocks.next_idx());

        self.body.blocks.alloc(BlockData {
            id,
            params: Vec::new(),
            stmts: Vec::new(),
            term: Term::None,
        });

        id
    }

    pub fn switch_block(&mut self, block: Block) {
        self.block = Some(block);
    }

    pub fn add_block_param(&mut self, block: Block, param: Local) {
        self.body.blocks[block.0].params.push(param);
    }

    pub fn add_local(&mut self, kind: LocalKind, repr: Repr) -> Local {
        let id = Local(self.body.locals.next_idx());

        self.body.locals.alloc(LocalData { id, kind, repr });
        id
    }

    fn block(&mut self) -> &mut BlockData {
        &mut self.body.blocks[self.block.unwrap().0]
    }

    fn stmt(&mut self, stmt: Stmt) {
        self.block().stmts.push(stmt);
    }

    fn term(&mut self, term: Term) {
        let block = self.block();

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

    pub fn return_(&mut self, op: impl Into<Operand>) {
        self.term(Term::Return(op.into()));
    }

    pub fn jump(&mut self, target: impl Into<JumpTarget>) {
        self.term(Term::Jump(target.into()));
    }

    pub fn switch(&mut self, discr: impl Into<Operand>, values: Vec<i128>, targets: impl Into<Vec<JumpTarget>>) {
        self.term(Term::Switch {
            discr: discr.into(),
            values,
            targets: targets.into(),
        });
    }

    pub fn init(&mut self, local: Local) {
        self.stmt(Stmt::Init(local));
    }

    pub fn drop(&mut self, local: Local) {
        self.stmt(Stmt::Drop(local));
    }

    pub fn set_discriminant(&mut self, place: Place, ctor: Ctor) {
        self.stmt(Stmt::SetDiscriminant(place, ctor));
    }

    pub fn call(&mut self, place: Place, func: impl Into<Operand>, args: impl Into<Vec<Operand>>) {
        self.stmt(Stmt::Call {
            place,
            func: func.into(),
            args: args.into(),
        });
    }

    pub fn assign(&mut self, res: Place, op: impl Into<Operand>) {
        self.stmt(Stmt::Assign(res, Rvalue::Use(op.into())));
    }

    pub fn ref_(&mut self, res: Place, place: Place) {
        self.stmt(Stmt::Assign(res, Rvalue::Ref(place)));
    }

    pub fn discriminant(&mut self, res: Place, place: Place) {
        self.stmt(Stmt::Assign(res, Rvalue::Discriminant(place)));
    }

    pub fn cast(&mut self, res: Place, kind: CastKind, op: Operand) {
        self.stmt(Stmt::Assign(res, Rvalue::Cast(kind, op)));
    }

    pub fn body_ref(&mut self, res: Place, body: Body) {
        self.stmt(Stmt::Assign(res, Rvalue::BodyRef(body)));
    }

    pub fn def_ref(&mut self, res: Place, def: DefWithBody) {
        self.stmt(Stmt::Assign(res, Rvalue::DefRef(def)));
    }

    pub fn binop(&mut self, res: Place, op: BinOp, lhs: impl Into<Operand>, rhs: impl Into<Operand>) {
        self.stmt(Stmt::Assign(res, Rvalue::BinOp(op, lhs.into(), rhs.into())));
    }

    pub fn place_repr(&self, place: &Place) -> Repr {
        let mut repr = self.body.locals[place.local.0].repr.clone();

        for proj in place.projection.iter() {
            match *proj {
                | Projection::Deref => match repr {
                    | Repr::Box(inner) => repr = *inner,
                    | Repr::Ptr(inner, false, _) => repr = *inner,
                    | _ => unreachable!(),
                },
                | Projection::Field(i) => match repr {
                    | Repr::Struct(fields) => repr = fields[i].clone(),
                    | _ => unreachable!(),
                },
                | Projection::Index(_) => match repr {
                    | Repr::Array(_, e) => repr = *e.clone(),
                    | _ => unreachable!(),
                },
                | Projection::Downcast(_) => todo!(),
            }
        }

        repr
    }

    pub fn operand_repr(&self, op: &Operand) -> Repr {
        match op {
            | Operand::Copy(place) | Operand::Move(place) => self.place_repr(place),
            | Operand::Const(_, repr) => repr.clone(),
        }
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

    pub fn downcast(mut self, ctor: Ctor) -> Self {
        self.projection.push(Projection::Downcast(ctor));
        self
    }
}

impl From<(Const, Repr)> for Operand {
    fn from((c, r): (Const, Repr)) -> Self {
        Self::Const(c, r)
    }
}

impl From<Block> for JumpTarget {
    fn from(block: Block) -> Self {
        Self {
            block,
            args: Vec::new(),
        }
    }
}

impl<T: Into<Vec<Operand>>> From<(Block, T)> for JumpTarget {
    fn from((block, args): (Block, T)) -> Self {
        Self {
            block,
            args: args.into(),
        }
    }
}
