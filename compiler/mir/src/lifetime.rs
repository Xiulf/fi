use crate::ir::*;
use std::collections::HashMap;

pub fn infer_lifetimes(module: &mut Module) {
    for body in &mut module.bodies {
        body_lifetimes(body);
    }
}

pub struct Lifetimes<'a> {
    body: &'a Body,
    lt: HashMap<Local, Lifetime>,
}

#[derive(Debug, Clone, Copy)]
pub struct Lifetime {
    pub start: Location,
    pub end: Location,
}

impl<'a> Lifetimes<'a> {
    fn new(body: &'a Body) -> Self {
        Lifetimes {
            body,
            lt: HashMap::new(),
        }
    }

    fn extend(&mut self, local: Local, to: Location) {
        if let LocalKind::Var | LocalKind::Tmp = self.body.locals[local].kind {
            let entry = self
                .lt
                .entry(local)
                .or_insert(Lifetime { start: to, end: to });

            entry.end = to;
        }
    }

    fn end(&mut self, loc: Location) {
        for lt in self.lt.values_mut() {
            if lt.start == lt.end {
                lt.end = loc;
            }
        }
    }
}

fn test(a: Vec<u8>) -> Option<u8> {
    if a.is_empty() {
        return None;
    }

    Some(a[0] + 1)
}

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
//      _3 = call Vec::is_empty(_2), %1
//  %1:
//      VarDead(_2)
//      switch _3 [true: %2, otherwise %3]
//  %2:
//      VarDead(_3)
//      _0 = Option::None
//      return
//  %3:
//      VarDead(_3)
//      VarLive(_4)
//      _4 = add _1[0], 1
//      _0 = Option::Some(_4)
//      VarDead(_4)
//      return
//  }

fn body_lifetimes(body: &mut Body) {
    let mut lt = Lifetimes::new(body);
    let mut blocks = vec![body.blocks.first().unwrap().id];

    while let Some(block) = blocks.pop() {
        let data = &body.blocks[block];

        for (i, stmt) in data.stmts.iter().enumerate() {
            let loc = Location { block, stmt: i };

            match stmt {
                Stmt::Assign(place, rvalue) => {
                    place_lifetime(&mut lt, place, loc);
                    rvalue_lifetime(&mut lt, rvalue, loc);
                }
                Stmt::SetDiscr(place, _) => {
                    place_lifetime(&mut lt, place, loc);
                }
                _ => {}
            }
        }

        let loc = Location {
            block,
            stmt: data.stmts.len(),
        };

        match &data.term {
            Term::Abort => lt.end(loc),
            Term::Return => lt.end(loc),
            Term::Jump(to) => blocks.push(*to),
            Term::Switch(op, _, tos) => {
                blocks.extend(tos);
                op_lifetime(&mut lt, op, loc);
            }
            Term::Call(place, op, ops, to) => {
                blocks.push(*to);
                place_lifetime(&mut lt, place, loc);
                op_lifetime(&mut lt, op, loc);

                for op in ops {
                    op_lifetime(&mut lt, op, loc);
                }
            }
            _ => {}
        }
    }

    println!("{:#?}", lt.lt);
}

fn rvalue_lifetime(lt: &mut Lifetimes, rvalue: &RValue, loc: Location) {
    match rvalue {
        RValue::Use(op) => op_lifetime(lt, op, loc),
        RValue::AddrOf(place) => place_lifetime(lt, place, loc),
        RValue::Discr(place) => place_lifetime(lt, place, loc),
        RValue::Init(_, ops) => {
            for op in ops {
                op_lifetime(lt, op, loc);
            }
        }
    }
}

fn place_lifetime(lt: &mut Lifetimes, place: &Place, loc: Location) {
    if let PlaceBase::Local(local) = place.base {
        lt.extend(local, loc);
    }

    for elem in &place.elems {
        if let PlaceElem::Index(op) = elem {
            op_lifetime(lt, op, loc);
        }
    }
}

fn op_lifetime(lt: &mut Lifetimes, op: &Operand, loc: Location) {
    match op {
        Operand::Move(place) => place_lifetime(lt, place, loc),
        Operand::Copy(place) => place_lifetime(lt, place, loc),
        _ => {}
    }
}
