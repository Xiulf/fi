use std::sync::Arc;

use arena::{ArenaMap, Idx};

use crate::db::MirDatabase;
use crate::instance::Instance;
use crate::syntax::*;

pub fn eval(db: &dyn MirDatabase, instance: Instance, args: Arc<[Const]>) -> Result<Const, EvalError> {
    EvalCtx::new(db, instance, args).run()
}

pub struct EvalCtx<'db> {
    _db: &'db dyn MirDatabase,
    body: Arc<BodyData>,
    locals: ArenaMap<Idx<LocalData>, Const>,
    block: Block,
    result: Option<Const>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EvalError {
    Abort,
    Unreachable,
    CannotEval,
}

impl<'db> EvalCtx<'db> {
    pub fn new(db: &'db dyn MirDatabase, instance: Instance, args: Arc<[Const]>) -> Self {
        let body = db.lookup_intern_body(instance.body(db));
        let mut locals = ArenaMap::default();

        for (local, _) in body.locals.iter() {
            locals.insert(local, Const::Undefined);
        }

        for (i, param) in body.blocks[Block::ENTRY.0].params.iter().enumerate() {
            locals.insert(param.0, args[i].clone());
        }

        Self {
            _db: db,
            body,
            locals,
            block: Block::ENTRY,
            result: None,
        }
    }

    pub fn run(mut self) -> Result<Const, EvalError> {
        while let None = self.result {
            self.eval_block()?;
        }

        self.result.ok_or(EvalError::CannotEval)
    }

    fn eval_block(&mut self) -> Result<(), EvalError> {
        let body = self.body.clone();
        let block = &body.blocks[self.block.0];

        for stmt in block.stmts.iter() {
            self.eval_stmt(stmt)?;
        }

        self.eval_term(&block.term)
    }

    fn eval_term(&mut self, term: &Term) -> Result<(), EvalError> {
        match term {
            | Term::None => unreachable!(),
            | Term::Abort => Err(EvalError::Abort),
            | Term::Unreachable => Err(EvalError::Unreachable),
            | Term::Return(op) => {
                let val = self.eval_op(op)?;
                self.result = Some(val);
                Ok(())
            },
            | Term::Jump(target) => self.eval_jump_target(target),
            | Term::Switch { .. } => todo!(),
        }
    }

    fn eval_jump_target(&mut self, target: &JumpTarget) -> Result<(), EvalError> {
        let body = self.body.clone();
        let block = &body.blocks[target.block.0];

        for (arg, param) in target.args.iter().zip(block.params.iter()) {
            let val = self.eval_op(arg)?;
            self.locals.insert(param.0, val);
        }

        self.block = target.block;
        Ok(())
    }

    fn eval_stmt(&mut self, stmt: &Stmt) -> Result<(), EvalError> {
        match stmt {
            | Stmt::Assign(place, rvalue) => self.eval_assign(place, rvalue),
            | Stmt::SetDiscriminant(_, _) => todo!(),
            | Stmt::Call { .. } => todo!(),
            | _ => Ok(()),
        }
    }

    fn eval_assign(&mut self, place: &Place, rvalue: &Rvalue) -> Result<(), EvalError> {
        let val = self.eval_rvalue(rvalue)?;
        assert!(place.projection.is_empty());
        self.locals[place.local.0] = val;
        Ok(())
    }

    fn eval_rvalue(&mut self, rvalue: &Rvalue) -> Result<Const, EvalError> {
        match rvalue {
            | Rvalue::Use(op) => self.eval_op(op),
            | _ => todo!(),
        }
    }

    fn eval_op(&mut self, op: &Operand) -> Result<Const, EvalError> {
        match op {
            | Operand::Copy(place) | Operand::Move(place) => self.load_place(place),
            | Operand::Const(val, _) => Ok(val.clone()),
        }
    }

    fn load_place(&mut self, place: &Place) -> Result<Const, EvalError> {
        let val = self.locals[place.local.0].clone();

        for elem in place.projection.iter() {
            match elem {
                | _ => todo!(),
            }
        }

        Ok(val)
    }
}
