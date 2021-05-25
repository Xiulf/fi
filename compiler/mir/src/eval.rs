use crate::db::MirDatabase;
use crate::ir::*;
use crate::layout::*;
use hir_def::arena::ArenaMap;
use std::sync::Arc;

pub fn eval_query(db: &dyn MirDatabase, def: hir::id::DefWithBodyId) -> EvalResult<Const> {
    let body = db.body_mir(def);
    let mut vm = VM::new(db);

    vm.init(&body);
    vm.eval(&body)
}

pub struct VM<'a> {
    db: &'a dyn MirDatabase,
    stack: Vec<u8>,
    locals: ArenaMap<LocalId, usize>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum EvalResult<T> {
    Finished(T),
    Next(BlockId),
    Abort,
}

impl<'a> VM<'a> {
    pub fn new(db: &'a dyn MirDatabase) -> Self {
        VM {
            db,
            stack: Vec::new(),
            locals: ArenaMap::default(),
        }
    }

    pub fn init(&mut self, body: &Body) {
        for (id, local) in body.locals.iter() {
            let loc = self.push_undefined(&local.layout);

            self.locals.insert(id, loc);
        }
    }

    pub fn eval(&mut self, body: &Body) -> EvalResult<Const> {
        let mut block = body.entry;

        while let Some(id) = block {
            match self.eval_block(body, id) {
                | EvalResult::Finished(_) => {
                    let ret = body.ret.unwrap();
                    let layout = body.locals[ret].layout.clone();
                    let val = self.read_const(self.locals[ret], layout);

                    return EvalResult::Finished(val);
                },
                | EvalResult::Abort => return EvalResult::Abort,
                | EvalResult::Next(id) => block = Some(id),
            }
        }

        EvalResult::Abort
    }

    fn eval_block(&mut self, body: &Body, block: BlockId) -> EvalResult<()> {
        let Block { stmts, term } = &body.blocks[block];

        for stmt in stmts {
            self.eval_stmt(body, stmt);
        }

        self.eval_term(body, term)
    }

    fn eval_term(&mut self, body: &Body, term: &Term) -> EvalResult<()> {
        match term {
            | Term::Abort => EvalResult::Abort,
            | Term::Return => EvalResult::Finished(()),
            | Term::Jump(id) => EvalResult::Next(*id),
            | Term::Switch(..) => unimplemented!(),
        }
    }

    fn eval_stmt(&mut self, body: &Body, stmt: &Stmt) {
        match stmt {
            | Stmt::Assign(place, rvalue) => self.eval_assign(body, place, rvalue),
            | Stmt::SetDiscr(place, discr) => unimplemented!(),
            | Stmt::Call(..) => unimplemented!(),
        }
    }

    fn eval_assign(&mut self, body: &Body, place: &Place, rvalue: &RValue) {
        let (loc, lyt) = self.eval_place(body, place);

        match rvalue {
            | RValue::Use(op) => {
                let val = self.eval_operand(body, op);
                let buf = &mut self.stack[loc..loc + lyt.size.bytes() as usize];

                val.write(buf, lyt);
            },
            | _ => unimplemented!(),
        }
    }

    fn eval_place(&mut self, body: &Body, place: &Place) -> (usize, Arc<Layout>) {
        let mut lyt = body.locals[place.local].layout.clone();
        let mut loc = self.locals[place.local];

        for elem in &place.elems {
            match elem {
                | PlaceElem::Deref => unimplemented!(),
                | PlaceElem::Downcast(idx) => {
                    lyt = lyt.variant(*idx);
                },
                | PlaceElem::Field(idx) => match &lyt.fields {
                    | Fields::Primitive => unreachable!(),
                    | Fields::Union { fields } => {
                        lyt = fields[*idx].clone();
                    },
                    | Fields::Arbitrary { fields } => {
                        loc += fields[*idx].0.bytes() as usize;
                        lyt = fields[*idx].1.clone();
                    },
                    | Fields::Array { stride, .. } => {
                        loc += *idx * stride.bytes() as usize;
                    },
                },
                | PlaceElem::Index(_) => unimplemented!(),
            }
        }

        (loc, lyt)
    }

    fn eval_operand(&mut self, body: &Body, op: &Operand) -> Const {
        match op {
            | Operand::Place(place) => {
                let (loc, lyt) = self.eval_place(body, place);

                self.read_const(loc, lyt)
            },
            | Operand::Const(c) => c.clone(),
        }
    }

    fn push_undefined(&mut self, layout: &Layout) -> usize {
        let loc = self.stack.len();

        self.stack.resize(self.stack.len() + layout.size.bytes() as usize, 0);
        loc
    }

    fn read_const(&self, loc: usize, layout: Arc<Layout>) -> Const {
        let bytes = &self.stack[loc..loc + layout.size.bytes() as usize];

        match &layout.abi {
            | Abi::Uninhabited => Const::Undefined(layout),
            | Abi::Scalar(s) => match s.value {
                | Primitive::Int(Integer::I8, _) => Const::Scalar(bytes[0] as u128, layout),
                | Primitive::Int(Integer::I16, _) => {
                    let ptr = bytes.as_ptr() as *const u16;

                    Const::Scalar(unsafe { *ptr } as u128, layout)
                },
                | Primitive::Int(Integer::I32, _) => {
                    let ptr = bytes.as_ptr() as *const u32;

                    Const::Scalar(unsafe { *ptr } as u128, layout)
                },
                | Primitive::Int(Integer::I64, _) => {
                    let ptr = bytes.as_ptr() as *const u64;

                    Const::Scalar(unsafe { *ptr } as u128, layout)
                },
                | Primitive::Int(Integer::I128, _) => {
                    let ptr = bytes.as_ptr() as *const u128;

                    Const::Scalar(unsafe { *ptr } as u128, layout)
                },
                | _ => unimplemented!(),
            },
            | Abi::Aggregate { sized: true } => match &layout.variants {
                | &Variants::Single { index: 0 } => match &layout.fields {
                    | Fields::Arbitrary { fields } => {
                        let vals = fields
                            .iter()
                            .map(|(offset, field)| self.read_const(loc + offset.bytes() as usize, field.clone()))
                            .collect();

                        Const::Tuple(vals)
                    },
                    | _ => unimplemented!(),
                },
                | _ => unimplemented!(),
            },
            | _ => unimplemented!(),
        }
    }
}

impl Const {
    pub fn write(&self, buf: &mut [u8], lyt: Arc<Layout>) {
        match self {
            | Const::Undefined(_) => {},
            | Const::Scalar(s, l) => match l.size.bytes() {
                | 1 => buf[0] = *s as u8,
                | 2 => {
                    let ptr = buf.as_mut_ptr() as *mut u16;

                    unsafe {
                        *ptr = *s as u16;
                    }
                },
                | 4 => {
                    let ptr = buf.as_mut_ptr() as *mut u32;

                    unsafe {
                        *ptr = *s as u32;
                    }
                },
                | 8 => {
                    let ptr = buf.as_mut_ptr() as *mut u64;

                    unsafe {
                        *ptr = *s as u64;
                    }
                },
                | 16 => {
                    let ptr = buf.as_mut_ptr() as *mut u128;

                    unsafe {
                        *ptr = *s;
                    }
                },
                | _ => unreachable!(),
            },
            | Const::Tuple(cs) => match &lyt.fields {
                | Fields::Primitive => unreachable!(),
                | Fields::Arbitrary { fields } => {
                    for (c, (offset, lyt)) in cs.iter().zip(fields.iter()) {
                        let offset = offset.bytes() as usize;
                        let size = lyt.size.bytes() as usize;

                        c.write(&mut buf[offset..offset + size], lyt.clone());
                    }
                },
                | _ => unimplemented!(),
            },
            | _ => unimplemented!(),
        }
    }
}
