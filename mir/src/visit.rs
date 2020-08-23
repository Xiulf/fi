use crate::*;

#[macro_export]
macro_rules! make_visitor{
    ($name:ident, $($mut:ident)?) => {
        #[allow(unused_variables)]
        pub trait $name<'tcx> {
            fn visit_package(&mut self, package: & $($mut)? Package<'tcx>) {
                self.super_package(package)
            }

            fn visit_item(&mut self, item: & $($mut)? Item<'tcx>) {
                self.super_item(item)
            }

            fn visit_body(&mut self, body: & $($mut)? Body<'tcx>) {
                self.super_body(body)
            }

            fn visit_local(&mut self, local: & $($mut)? Local<'tcx>) {
            }

            fn visit_block(&mut self, block: & $($mut)? Block<'tcx>) {
                self.super_block(block)
            }

            fn visit_stmt(&mut self, stmt: & $($mut)? Stmt<'tcx>) {
                self.super_stmt(stmt)
            }

            fn visit_assign(&mut self, place: & $($mut)? Place, rvalue: & $($mut)? RValue<'tcx>) {
                self.super_assign(place, rvalue)
            }

            fn visit_term(&mut self, term: & $($mut)? Term<'tcx>) {
                self.super_term(term)
            }

            fn visit_place(&mut self, place: & $($mut)? Place) {
                self.super_place(place)
            }

            fn visit_rvalue(&mut self, rvalue: & $($mut)? RValue<'tcx>) {
                self.super_rvalue(rvalue)
            }

            fn visit_op(&mut self, op: & $($mut)? Operand<'tcx>) {
                self.super_op(op)
            }

            fn visit_const(&mut self, const_: & $($mut)? Const<'tcx>) {
            }

            fn super_package(&mut self, package: & $($mut)? Package<'tcx>) {
                let Package { items } = package;

                for (_, item) in items {
                    self.visit_item(item);
                }
            }

            fn super_item(&mut self, item: & $($mut)? Item<'tcx>) {
                let Item { id: _, name: _, kind } = item;

                match kind {
                    ItemKind::Extern(_) => {},
                    ItemKind::Global(_, _) => {},
                    ItemKind::Body(body) => self.visit_body(body),
                }
            }

            fn super_body(&mut self, body: & $($mut)? Body<'tcx>) {
                let Body { locals, blocks } = body;

                for (_, local) in locals {
                    self.visit_local(local);
                }

                for (_, block) in blocks {
                    self.visit_block(block);
                }
            }

            fn super_block(&mut self, block: & $($mut)? Block<'tcx>) {
                let Block { id: _, stmts, term } = block;

                for stmt in stmts {
                    self.visit_stmt(stmt);
                }

                self.visit_term(term);
            }

            fn super_stmt(&mut self, stmt: & $($mut)? Stmt<'tcx>) {
                match stmt {
                    Stmt::Assign(place, rvalue) => self.visit_assign(place, rvalue),
                    Stmt::Nop => {},
                }
            }

            fn super_assign(&mut self, place: & $($mut)? Place, rvalue: & $($mut)? RValue<'tcx>) {
                self.visit_place(place);
                self.visit_rvalue(rvalue);
            }

            fn super_term(&mut self, term: & $($mut)? Term<'tcx>) {
                match term {
                    Term::Unset => {},
                    Term::Abort => {},
                    Term::Return => {},
                    Term::Jump(_) => {},
                    Term::Switch(pred, _, _) => self.visit_op(pred),
                    Term::Call(place, func, args, _) => {
                        self.visit_place(place);
                        self.visit_op(func);

                        for arg in args {
                            self.visit_op(arg);
                        }
                    },
                }
            }

            fn super_place(&mut self, place: & $($mut)? Place) {
                let Place { base, elems } = place;

                match base {
                    PlaceBase::Local(_) => {},
                    PlaceBase::Global(_) => {},
                }

                for elem in elems {
                    match elem {
                        PlaceElem::Deref => {},
                        PlaceElem::Field(_) => {},
                        PlaceElem::Index(place) => self.visit_place(place),
                    }
                }
            }

            fn super_rvalue(&mut self, rvalue: & $($mut)? RValue<'tcx>) {
                match rvalue {
                    RValue::Use(op) => self.visit_op(op),
                    RValue::Ref(place) => self.visit_place(place),
                    RValue::Cast(_, op) => {
                        self.visit_op(op);
                    },
                    // RValue::Slice(place, lo, hi) => {
                    //     self.visit_place(place);
                    //     self.visit_op(lo);
                    //     self.visit_op(hi);
                    // },
                    RValue::Init(_, ops) => {
                        for op in ops {
                            self.visit_op(op);
                        }
                    },
                    RValue::BinOp(_, lhs, rhs) => {
                        self.visit_op(lhs);
                        self.visit_op(rhs);
                    },
                    RValue::UnOp(_, op) => self.visit_op(op),
                }
            }

            fn super_op(&mut self, op: & $($mut)? Operand<'tcx>) {
                match op {
                    Operand::Place(place) => self.visit_place(place),
                    Operand::Const(const_) => self.visit_const(const_),
                }
            }
        }
    };
}

make_visitor!(Visitor,);
make_visitor!(VisitorMut, mut);
