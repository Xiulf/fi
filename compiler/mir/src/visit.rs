use crate::ir::*;

macro_rules! make_visitor {
    ($name:ident, $iter:ident $($mut:ident)?) => {
        pub trait $name {
            fn visit_bodies(&mut self, bodies: &$($mut)? Bodies) {
                self.super_bodies(bodies);
            }

            fn visit_body(&mut self, body: &$($mut)? Body) {
                self.super_body(body);
            }

            fn visit_local(&mut self, local: &$($mut)? Local) {
            }

            fn visit_block(&mut self, block: &$($mut)? Block) {
                self.super_block(block);
            }

            fn visit_stmt(&mut self, stmt: &$($mut)? Stmt) {
                self.super_stmt(stmt);
            }

            fn visit_term(&mut self, term: &$($mut)? Term) {
                self.super_term(term);
            }

            fn visit_rvalue(&mut self, rvalue: &$($mut)? RValue) {
                self.super_rvalue(rvalue);
            }

            fn visit_operand(&mut self, operand: &$($mut)? Operand) {
                self.super_operand(operand);
            }

            fn visit_place(&mut self, place: &$($mut)? Place) {
                self.super_place(place);
            }

            fn visit_const(&mut self, const_: &$($mut)? Const) {
            }

            fn super_bodies(&mut self, bodies: &$($mut)? Bodies) {
                for (_, body) in bodies.bodies.$iter() {
                    self.visit_body(body);
                }
            }

            fn super_body(&mut self, body: &$($mut)? Body) {
                for (_, local) in body.locals.$iter() {
                    self.visit_local(local);
                }

                for (_, block) in body.blocks.$iter() {
                    self.visit_block(block);
                }
            }

            fn super_block(&mut self, block: &$($mut)? Block) {
                for stmt in block.stmts.$iter() {
                    self.visit_stmt(stmt);
                }

                self.visit_term(&$($mut)? block.term);
            }

            fn super_stmt(&mut self, stmt: &$($mut)? Stmt) {
                match stmt {
                    | Stmt::Assign(place, rvalue) => {
                        self.visit_place(place);
                        self.visit_rvalue(rvalue);
                    },
                    | Stmt::SetDiscr(place, _) => {
                        self.visit_place(place);
                    },
                    | Stmt::Call(ret, func, args) => {
                        self.visit_place(ret);
                        self.visit_operand(func);

                        for arg in args.$iter() {
                            self.visit_operand(arg);
                        }
                    },
                }
            }

            fn super_term(&mut self, term: &$($mut)? Term) {
                match term {
                    | Term::Switch(op, _, _) => {
                        self.visit_operand(op);
                    },
                    | _ => {},
                }
            }

            fn super_rvalue(&mut self, rvalue: &$($mut)? RValue) {
                match rvalue {
                    | RValue::Use(op) => {
                        self.visit_operand(op);
                    },
                    | RValue::AddrOf(place) => {
                        self.visit_place(place);
                    },
                    | RValue::GetDiscr(place) => {
                        self.visit_place(place);
                    },
                    | RValue::Intrinsic(_, args) => {
                        for arg in args.$iter() {
                            self.visit_operand(arg);
                        }
                    },
                }
            }

            fn super_operand(&mut self, operand: &$($mut)? Operand) {
                match operand {
                    | Operand::Place(place) => {
                        self.visit_place(place);
                    },
                    | Operand::Const(const_, _) => {
                        self.visit_const(const_);
                    },
                    | _ => {},
                }
            }

            fn super_place(&mut self, place: &$($mut)? Place) {
                for elem in place.elems.$iter() {
                    match elem {
                        | PlaceElem::Index(op) => {
                            self.visit_operand(op);
                        },
                        | PlaceElem::Offset(op) => {
                            self.visit_operand(op);
                        },
                        | _ => {},
                    }
                }
            }
        }
    };
}

make_visitor!(Visitor, iter);
make_visitor!(VisitorMut, iter_mut mut);
