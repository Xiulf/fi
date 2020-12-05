use crate::ir::*;
use std::collections::{HashMap, HashSet};
use std::ops::BitOr;

pub fn infer_lifetimes(module: &mut Module) {
    for body in &mut module.bodies {
        body_lifetimes(body);
    }
}

pub struct Lifetimes {
    blocks: HashMap<Block, Lifetime>,
}

#[derive(Debug, Clone)]
pub struct Lifetime {
    pub alive: HashSet<Local>,
}

impl Lifetimes {
    fn new() -> Self {
        Lifetimes {
            blocks: HashMap::new(),
        }
    }

    fn alive(&mut self, block: Block, local: Local) {
        self.blocks.get_mut(&block).unwrap().alive.insert(local);
    }
}

// fn test(a: Vec<u8>) -> Option<u8> {
//     if a.is_empty() {
//         return None;
//     }
//
//     Some(a[0] + 1)
// }
//
//  fn test {
//      ret _0 :: Option<u8>
//      arg _1 :: Vec<u8>
//      tmp _2 :: &Vec<u8>
//      tmp _3 :: bool
//      tmp _4 :: u8
//  %0:
//      VarLive(_2)
//      VarLive(_3)
//      _2 = &_1
//      _3 = call Vec::is_empty(move _2), %1
//  %1:
//      VarDead(_2)
//      switch move _3 [false: %2, otherwise %3]
//  %2:
//      VarDead(_3)
//      VarLive(_4)
//      _4 = add copy _1[0], 1
//      _0 = Option::Some(move _4)
//      VarDead(_4)
//      VarDead(_1)
//      return
//  %3:
//      VarDead(_3)
//      _0 = Option::None
//      VarDead(_1)
//      return
//  }

fn body_lifetimes(body: &mut Body) {
    let root = body.blocks.first().unwrap().id;
    let mut lt = Lifetimes::new();

    block_lifetimes(&mut lt, body, Vec::new(), root);
}

fn block_lifetimes(lt: &mut Lifetimes, body: &mut Body, mut prev: Vec<Block>, block: Block) {
    let data = &mut body.blocks[block];

    lt.blocks.insert(
        block,
        Lifetime {
            alive: HashSet::new(),
        },
    );

    for stmt in &data.stmts {
        match stmt {
            Stmt::Assign(place, rvalue) => {
                place_lifetime(lt, place, block);
                rvalue_lifetime(lt, rvalue, block);
            }
            Stmt::SetDiscr(place, _) => {
                place_lifetime(lt, place, block);
            }
            _ => {}
        }
    }

    let succ = match &data.term {
        Term::Unset => unreachable!(),
        Term::Abort => Vec::new(),
        Term::Return => Vec::new(),
        Term::Jump(n) => vec![*n],
        Term::Switch(op, _, next) => {
            op_lifetime(lt, op, block);
            next.clone()
        }
        Term::Call(place, op, ops, n) => {
            place_lifetime(lt, place, block);
            op_lifetime(lt, op, block);

            for op in ops {
                op_lifetime(lt, op, block);
            }

            vec![*n]
        }
    };

    let prev_alive = prev
        .iter()
        .map(|p| &lt.blocks[p].alive)
        .fold(HashSet::new(), |acc, lt| acc.bitor(lt));

    let born = lt.blocks[&block].alive.difference(&prev_alive);

    for &local in born {
        if let LocalKind::Var | LocalKind::Tmp = body.locals[local].kind {
            data.stmts.insert(0, Stmt::VarLive(local));
        }
    }

    prev.push(block);

    for &next in &succ {
        block_lifetimes(lt, body, prev.clone(), next);
    }

    if !succ.is_empty() {
        let alive_after = succ
            .iter()
            .map(|block| &lt.blocks[block].alive)
            .fold(HashSet::new(), |acc, lt| acc.bitor(lt));

        let dead = lt.blocks[&block].alive.difference(&alive_after);

        for &local in dead {
            if let LocalKind::Var | LocalKind::Tmp = body.locals[local].kind {
                if local_in_term(&body.blocks[block].term, local) {
                    for &next in &succ {
                        body.blocks[next].stmts.insert(0, Stmt::VarDead(local));
                    }
                } else {
                    body.blocks[block].stmts.push(Stmt::VarDead(local));
                }
            }
        }
    } else {
        for &local in &lt.blocks[&block].alive {
            if let LocalKind::Var | LocalKind::Tmp = body.locals[local].kind {
                body.blocks[block].stmts.push(Stmt::VarDead(local));
            }
        }
    }
}

fn rvalue_lifetime(lt: &mut Lifetimes, rvalue: &RValue, block: Block) {
    match rvalue {
        RValue::Use(op) => op_lifetime(lt, op, block),
        RValue::AddrOf(place) => place_lifetime(lt, place, block),
        RValue::Discr(place) => place_lifetime(lt, place, block),
        RValue::Init(_, ops) => {
            for op in ops {
                op_lifetime(lt, op, block);
            }
        }
    }
}

fn place_lifetime(lt: &mut Lifetimes, place: &Place, block: Block) {
    if let PlaceBase::Local(local) = place.base {
        lt.alive(block, local);
    }

    for elem in &place.elems {
        if let PlaceElem::Index(op) = elem {
            op_lifetime(lt, op, block);
        }
    }
}

fn op_lifetime(lt: &mut Lifetimes, op: &Operand, block: Block) {
    match op {
        Operand::Move(place) => place_lifetime(lt, place, block),
        Operand::Copy(place) => place_lifetime(lt, place, block),
        _ => {}
    }
}

fn local_in_term(term: &Term, local: Local) -> bool {
    match term {
        Term::Switch(op, _, _) => local_in_op(op, local),
        Term::Call(place, op, ops, _) => {
            local_in_place(place, local)
                || local_in_op(op, local)
                || ops.iter().any(|op| local_in_op(op, local))
        }
        _ => false,
    }
}

fn local_in_op(op: &Operand, local: Local) -> bool {
    match op {
        Operand::Move(place) => local_in_place(place, local),
        Operand::Copy(place) => local_in_place(place, local),
        _ => false,
    }
}

fn local_in_place(place: &Place, local: Local) -> bool {
    if let PlaceBase::Local(l) = place.base {
        if l == local {
            return true;
        }
    }

    place.elems.iter().any(|e| match e {
        PlaceElem::Index(i) => local_in_op(i, local),
        _ => false,
    })
}
