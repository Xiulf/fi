#![allow(unused_variables)]

use crate::*;
use check::tcx::Tcx;
use std::collections::HashMap;

pub fn eval<'tcx>(
    tcx: &Tcx<'tcx>,
    body: Body<'tcx>,
    subst: &HashMap<hir::Id, Ty<'tcx>>,
) -> Const<'tcx> {
    let mut ecx = Ecx::new(tcx, body, subst);

    ecx.eval();
    ecx.finish()
}

pub fn eval_expr<'tcx>(
    tcx: &Tcx<'tcx>,
    hir: &hir::Package,
    expr: &hir::Id,
    ty: Ty<'tcx>,
    subst: &HashMap<hir::Id, Ty<'tcx>>,
) -> Const<'tcx> {
    let mut body = Body {
        locals: BTreeMap::new(),
        blocks: BTreeMap::new(),
    };

    body.locals.insert(
        LocalId::RET,
        Local {
            id: LocalId::RET,
            ty,
            kind: LocalKind::Ret,
        },
    );

    let builder = Builder {
        body: &mut body,
        current_block: None,
    };

    let mut converter = crate::convert::BodyConverter::new(tcx, hir, builder);
    let entry = converter.builder.create_block();

    converter.builder.use_block(entry);

    let res = converter.trans_expr(expr);

    converter.builder.use_(Place::local(LocalId::RET), res);
    converter.builder.return_();

    eval(tcx, body, subst)
}

pub struct Ecx<'a, 'tcx> {
    tcx: &'a Tcx<'tcx>,
    body: Body<'tcx>,
    subst: &'a HashMap<hir::Id, Ty<'tcx>>,
    locals: BTreeMap<LocalId, Const<'tcx>>,
    current_block: BlockId,
    status: EvalStatus,
}

#[derive(Debug, Clone, Copy)]
pub enum EvalStatus {
    Busy,
    Done,
    Error,
}

impl<'a, 'tcx> Ecx<'a, 'tcx> {
    pub fn new(
        tcx: &'a Tcx<'tcx>,
        body: Body<'tcx>,
        subst: &'a HashMap<hir::Id, Ty<'tcx>>,
    ) -> Self {
        let locals = body
            .locals
            .keys()
            .map(|id| (*id, Const::Undefined))
            .collect();

        Ecx {
            tcx,
            body,
            subst,
            locals,
            current_block: BlockId(0),
            status: EvalStatus::Busy,
        }
    }

    pub fn finish(self) -> Const<'tcx> {
        self.locals[&LocalId::RET].clone()
    }

    pub fn eval(&mut self) {
        while let EvalStatus::Busy = self.status {
            let block = &self.body.blocks[&self.current_block];
            let stmts = block.stmts.clone();
            let term = block.term.clone();

            for stmt in stmts {
                self.eval_stmt(stmt);
            }

            self.eval_term(term);
        }
    }

    pub fn eval_stmt(&mut self, stmt: Stmt<'tcx>) {
        match stmt {
            Stmt::Nop => {}
            Stmt::Assign(place, rvalue) => {
                let value = self.eval_rvalue(rvalue);

                place.store(self, value);
            }
            Stmt::VarLive(_) => {}
            Stmt::VarDead(_) => {}
        }
    }

    pub fn eval_term(&mut self, term: Term<'tcx>) {
        match term {
            Term::Unset => unreachable!(),
            Term::Abort => {
                self.locals.insert(LocalId::RET, Const::Undefined);
                self.status = EvalStatus::Error;
            }
            Term::Return => {
                self.status = EvalStatus::Done;
            }
            Term::Jump(block) => {
                self.current_block = block;
            }
            Term::Switch(op, vals, targets) => {
                unimplemented!();
            }
            Term::Call(place, func, args, target) => {
                unimplemented!();
            }
        }
    }

    pub fn eval_rvalue(&mut self, rvalue: RValue<'tcx>) -> Const<'tcx> {
        match rvalue {
            RValue::Use(op) => self.eval_op(op),
            RValue::Ref(place) => {
                let val = place.load(self);

                Const::Ref(Box::new(val))
            }
            RValue::Cast(ty, op) => self.eval_op(op),
            RValue::BinOp(op, lhs, rhs) => unimplemented!(),
            RValue::UnOp(op, rhs) => unimplemented!(),
            RValue::Init(ty, _variant, ops) => {
                let vals = ops.into_iter().map(|op| self.eval_op(op)).collect();

                match ty.replace(self.subst, self.tcx) {
                    Type::Array(_, _) => Const::Array(vals),
                    Type::Tuple(_) => Const::Tuple(vals),
                    _ => unimplemented!(),
                }
            }
        }
    }

    pub fn eval_op(&mut self, op: Operand<'tcx>) -> Const<'tcx> {
        match op {
            Operand::Place(place) => place.load(self),
            Operand::Const(c, _) => c,
        }
    }
}

impl<'tcx> Place<'tcx> {
    fn load(&self, ecx: &Ecx<'_, 'tcx>) -> Const<'tcx> {
        let mut val = match &self.base {
            PlaceBase::Local(id) => ecx.locals[id].clone(),
            PlaceBase::Global(_) => unimplemented!(),
        };

        for elem in &self.elems {
            match elem {
                PlaceElem::Deref => match val {
                    Const::Ref(to) => {
                        val = *to;
                    }
                    _ => unreachable!(),
                },
                PlaceElem::Field(idx) => match val {
                    Const::Tuple(vals) => {
                        val = vals.into_iter().nth(*idx).unwrap();
                    }
                    _ => unimplemented!(),
                },
                PlaceElem::Index(idx) => {
                    let idx = match idx {
                        Operand::Place(idx) => idx.load(ecx),
                        Operand::Const(idx, _) => idx.clone(),
                    }
                    .scalar() as usize;

                    match val {
                        Const::Array(vals) => {
                            val = vals.into_iter().nth(idx).unwrap();
                        }
                        Const::Bytes(bytes) => {
                            let byte = bytes[idx];

                            val = Const::Scalar(byte as u128);
                        }
                        _ => unimplemented!(),
                    }
                }
                PlaceElem::Slice(lo, hi) => unimplemented!(),
            }
        }

        val
    }

    fn store(&self, ecx: &mut Ecx<'_, 'tcx>, val: Const<'tcx>) {
        let ecx_clone = unsafe { &*(ecx as *const _) };
        let mut ptr = match &self.base {
            PlaceBase::Local(id) => ecx.locals.get_mut(id).unwrap(),
            PlaceBase::Global(_) => unimplemented!(),
        };

        for elem in &self.elems {
            match elem {
                PlaceElem::Deref => match ptr {
                    Const::Ref(to) => {
                        ptr = &mut **to;
                    }
                    _ => unreachable!(),
                },
                PlaceElem::Field(idx) => match ptr {
                    Const::Tuple(vals) => {
                        ptr = vals.iter_mut().nth(*idx).unwrap();
                    }
                    _ => unimplemented!(),
                },
                PlaceElem::Index(idx) => {
                    let idx = match idx {
                        Operand::Place(idx) => idx.load(ecx_clone),
                        Operand::Const(idx, _) => idx.clone(),
                    }
                    .scalar() as usize;

                    match ptr {
                        Const::Array(vals) => {
                            ptr = vals.iter_mut().nth(idx).unwrap();
                        }
                        _ => unimplemented!(),
                    }
                }
                PlaceElem::Slice(lo, hi) => unimplemented!(),
            }
        }

        *ptr = val;
    }
}

impl<'tcx> Const<'tcx> {
    fn scalar(&self) -> u128 {
        match self {
            Const::Scalar(val) => *val,
            _ => unreachable!(),
        }
    }
}
