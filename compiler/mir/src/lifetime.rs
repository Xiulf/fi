use crate::ir::*;
use std::collections::HashMap;

pub fn infer_lifetimes(module: &mut Module) {
    for body in &mut module.bodies {
        body_lifetimes(body);
    }
}

pub struct Lifetimes {
    lt: HashMap<Local, Lifetime>,
}

#[derive(Debug, Clone, Copy)]
pub struct Lifetime {
    pub start: Location,
    pub end: Location,
}

impl Lifetimes {
    fn new() -> Self {
        Lifetimes { lt: HashMap::new() }
    }

    fn extend(&mut self, local: Local, to: Location) {
        let entry = self
            .lt
            .entry(local)
            .or_insert(Lifetime { start: to, end: to });

        entry.end = to;
    }
}

fn body_lifetimes(body: &mut Body) {
    let mut lt = Lifetimes::new();
    let graph = body.graph();

    for block in &body.blocks {
        for (i, stmt) in block.stmts.iter().enumerate() {
            let loc = Location {
                block: block.id,
                stmt: i,
            };

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
            block: block.id,
            stmt: block.stmts.len(),
        };

        match &block.term {
            Term::Switch(op, _, _) => {
                op_lifetime(&mut lt, op, loc);
            }
            Term::Call(place, op, ops, _) => {
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
