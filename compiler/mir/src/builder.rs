use crate::ir::*;
use index_vec::IndexVec;

pub struct Builder {
    body: Body,
    current_block: Block,
}

impl Builder {
    pub fn new(def: DefId, kind: BodyKind) -> Self {
        Builder {
            body: Body {
                def,
                kind,
                locals: IndexVec::new(),
                blocks: IndexVec::new(),
            },
            current_block: Block::new(0),
        }
    }

    pub fn finish(self) -> Body {
        self.body
    }

    fn block(&mut self) -> &mut BlockData {
        &mut self.body.blocks[self.current_block]
    }

    pub fn set_bock(&mut self, block: Block) {
        self.current_block = block;
    }

    pub fn get_block(&self) -> Block {
        self.current_block
    }

    pub fn term_unset(&self) -> bool {
        matches!(self.body.blocks[self.current_block].term, Term::Unset)
    }

    pub fn create_block(&mut self) -> Block {
        self.body.blocks.push(BlockData {
            id: self.body.blocks.next_idx(),
            stmts: Vec::new(),
            term: Term::Unset,
        })
    }

    pub fn create_ret(&mut self, ty: Ty) -> Local {
        self.body.locals.push(LocalData {
            id: self.body.locals.next_idx(),
            ty,
            kind: LocalKind::Ret,
        })
    }

    pub fn create_arg(&mut self, ty: Ty) -> Local {
        self.body.locals.push(LocalData {
            id: self.body.locals.next_idx(),
            ty,
            kind: LocalKind::Arg,
        })
    }

    pub fn create_tmp(&mut self, ty: Ty) -> Local {
        self.body.locals.push(LocalData {
            id: self.body.locals.next_idx(),
            ty,
            kind: LocalKind::Tmp,
        })
    }

    pub fn create_var(&mut self, ty: Ty) -> Local {
        self.body.locals.push(LocalData {
            id: self.body.locals.next_idx(),
            ty,
            kind: LocalKind::Var,
        })
    }

    pub fn local_ty(&self, local: Local) -> Ty {
        self.body.locals[local].ty.clone()
    }

    pub fn placed(&mut self, op: Operand) -> Place {
        match op {
            Operand::Copy(p) => p,
            Operand::Move(p) => p,
            Operand::Const(c, ty) => {
                let tmp = self.create_tmp(ty.clone());
                let tmp = Place::local(tmp);

                self.use_op(tmp.clone(), Operand::Const(c, ty));
                tmp
            }
        }
    }

    pub fn nop(&mut self) {
        self.block().stmts.push(Stmt::Nop);
    }

    pub fn var_live(&mut self, local: Local) {
        self.block().stmts.push(Stmt::VarLive(local));
    }

    pub fn var_dead(&mut self, local: Local) {
        self.block().stmts.push(Stmt::VarDead(local));
    }

    pub fn use_op(&mut self, place: Place, op: Operand) {
        self.block()
            .stmts
            .push(Stmt::Assign(place, RValue::Use(op)));
    }

    pub fn addrof(&mut self, place: Place, to: Place) {
        self.block()
            .stmts
            .push(Stmt::Assign(place, RValue::AddrOf(to)));
    }

    pub fn get_discr(&mut self, place: Place, from: Place) {
        self.block()
            .stmts
            .push(Stmt::Assign(place, RValue::Discr(from)));
    }

    pub fn init(&mut self, place: Place, ty: Ty, args: Vec<Operand>) {
        self.block()
            .stmts
            .push(Stmt::Assign(place, RValue::Init(ty, args)));
    }

    pub fn set_discr(&mut self, place: Place, variant: usize) {
        self.block().stmts.push(Stmt::SetDiscr(place, variant));
    }

    pub fn abort(&mut self) {
        self.block().term = Term::Abort;
    }

    pub fn return_(&mut self) {
        self.block().term = Term::Return;
    }

    pub fn jump(&mut self, to: Block) {
        self.block().term = Term::Jump(to);
    }

    pub fn call(&mut self, place: Place, op: Operand, args: Vec<Operand>, to: Block) {
        self.block().term = Term::Call(place, op, args, to);
    }

    pub fn switch(&mut self, op: Operand, vals: Vec<u128>, tos: Vec<Block>) {
        self.block().term = Term::Switch(op, vals, tos);
    }
}

impl Place {
    pub fn local(id: Local) -> Self {
        Place {
            base: PlaceBase::Local(id),
            elems: Vec::new(),
        }
    }

    pub fn static_(id: DefId) -> Self {
        Place {
            base: PlaceBase::Static(id),
            elems: Vec::new(),
        }
    }

    pub fn deref(mut self) -> Self {
        self.elems.push(PlaceElem::Deref);
        self
    }

    pub fn field(mut self, i: usize) -> Self {
        self.elems.push(PlaceElem::Field(i));
        self
    }

    pub fn index(mut self, idx: Operand) -> Self {
        self.elems.push(PlaceElem::Index(idx));
        self
    }

    pub fn downcast(mut self, variant: usize) -> Self {
        self.elems.push(PlaceElem::Downcast(variant));
        self
    }
}
