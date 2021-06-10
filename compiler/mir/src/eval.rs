use crate::db::MirDatabase;
use crate::ir::*;
use crate::ty::{Type, TypeKind};
use hir_def::arena::ArenaMap;
use std::sync::Arc;

pub fn eval_query(db: &dyn MirDatabase, def: hir::id::DefWithBodyId) -> EvalResult {
    let body = db.body_mir(def);
    let mut vm = VM::new(db);

    vm.init(&body);
    vm.eval(&body)
}

pub struct VM<'a> {
    db: &'a dyn MirDatabase,
    locals: ArenaMap<LocalId, Const>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum EvalResult {
    Finished(Const),
    Next(BlockId),
    Abort,
}

impl<'a> VM<'a> {
    pub fn new(db: &'a dyn MirDatabase) -> Self {
        VM {
            db,
            locals: ArenaMap::default(),
        }
    }

    pub fn init(&mut self, body: &Body) {
        for (id, local) in body.locals.iter() {
            self.locals.insert(id, Const::undefined(&local.ty));
        }
    }

    pub fn eval(&mut self, body: &Body) -> EvalResult {
        let mut block = body.entry;

        while let Some(id) = block {
            match self.eval_block(body, id) {
                | EvalResult::Finished(r) => return EvalResult::Finished(r),
                | EvalResult::Abort => return EvalResult::Abort,
                | EvalResult::Next(id) => block = Some(id),
            }
        }

        EvalResult::Abort
    }

    fn eval_block(&mut self, body: &Body, block: BlockId) -> EvalResult {
        let Block { stmts, term } = &body.blocks[block];

        for stmt in stmts {
            self.eval_stmt(body, stmt);
        }

        self.eval_term(body, term)
    }

    fn eval_term(&mut self, body: &Body, term: &Term) -> EvalResult {
        match term {
            | Term::Abort => EvalResult::Abort,
            | Term::Return => {
                let ret = body.ret.unwrap();

                EvalResult::Finished(self.locals[ret].clone())
            },
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
        match rvalue {
            | RValue::Use(op) => {
                let val = self.eval_operand(body, op);

                self.store(body, place, val);
            },
            | _ => unimplemented!(),
        }
    }

    fn store(&mut self, body: &Body, place: &Place, value: Const) {
        let mut ty = body.locals[place.local].ty.clone();
        let mut val = &mut self.locals[place.local];

        for elem in &place.elems {
            match elem {
                | PlaceElem::Deref => {
                    if let Const::Ref(to) = val {
                        val = to;
                    }
                },
                | PlaceElem::Field(idx) => {
                    if let Const::Tuple(cs) = val {
                        val = &mut cs[*idx];
                    }
                },
                | _ => unimplemented!(),
            }
        }

        *val = value;
    }

    fn load(&mut self, body: &Body, place: &Place) -> (Const, Arc<Type>) {
        let mut ty = body.locals[place.local].ty.clone();
        let mut val = self.locals[place.local].clone();

        for elem in &place.elems {
            match elem {
                | PlaceElem::Deref => {
                    if let Const::Ref(to) = val {
                        val = *to;
                    }
                },
                | PlaceElem::Downcast(idx) => {
                    // lyt = lyt.variant(*idx);
                    unimplemented!();
                },
                | PlaceElem::Field(idx) => {
                    if let Const::Tuple(mut cs) = val {
                        val = cs.swap_remove(*idx);
                    }
                },
                | PlaceElem::Offset(offset) => {
                    // let offset = self.eval_operand(body, offset);
                    //
                    // if let Const::Scalar(offset) = offset {
                    //     loc += offset as usize;
                    // }
                    unimplemented!();
                },
                | PlaceElem::Index(_) => unimplemented!(),
            }
        }

        (val, ty)
    }

    fn eval_operand(&mut self, body: &Body, op: &Operand) -> Const {
        match op {
            | Operand::Place(place) => self.load(body, place).0,
            | Operand::Const(c, _) => c.clone(),
        }
    }
}

impl Const {
    pub fn undefined(ty: &Type) -> Self {
        match &ty.kind {
            | TypeKind::Ptr(elem) => Const::Ref(Box::new(Const::undefined(elem))),
            | TypeKind::Array(elem, len) => {
                let elem = Const::undefined(elem);
                let cs = vec![elem; *len];

                Const::Tuple(cs)
            },
            | TypeKind::And(fields) => {
                let cs = fields.iter().map(|f| Const::undefined(f)).collect();

                Const::Tuple(cs)
            },
            | _ => Const::Undefined,
        }
    }
}
