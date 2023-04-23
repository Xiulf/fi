use crate::ir::*;

macro_rules! make_visitor {
    ($name:ident, $($mut:ident)?) => {
        pub trait $name {
            // fn visit_body(&mut self, body: Body, data: &$($mut)? BodyData) {
            //     self.super_body(body, data);
            // }

            fn visit_local_decl(&mut self, local: Local, data: &$($mut)? LocalData) {
                self.super_local_decl(local, data);
            }

            fn visit_block(&mut self, block: Block, data: &$($mut)? BlockData) {
                self.super_block(block, data);
            }

            fn visit_stmt(&mut self, stmt: &$($mut)? Statement, loc: Location) {
                self.super_stmt(stmt, loc);
            }

            fn visit_assign(&mut self, place: &$($mut)? Place, rvalue: &$($mut)? RValue, loc: Location) {
                self.super_assign(place, rvalue, loc);
            }

            fn visit_terminator(&mut self, terminator: &$($mut)? Terminator, loc: Location) {
                self.super_terminator(terminator, loc);
            }

            fn visit_jump_target(&mut self, target: &$($mut)? JumpTarget, loc: Location) {
                self.super_jump_target(target, loc);
            }

            fn visit_rvalue(&mut self, rvalue: &$($mut)? RValue, loc: Location) {
                self.super_rvalue(rvalue, loc);
            }

            fn visit_operand(&mut self, op: &$($mut)? Operand, loc: Location) {
                self.super_operand(op, loc);
            }

            fn visit_place(&mut self, place: &$($mut)? Place, ctx: PlaceContext, loc: Location) {
                self.super_place(place, ctx, loc);
            }

            fn visit_projection(&mut self, place: &$($mut)? Place, ctx: PlaceContext, loc: Location) {
                self.super_projection(place, ctx, loc);
            }

            fn visit_projection_elem(&mut self, elem: &$($mut)? Projection, ctx: PlaceContext, loc: Location) {
                self.super_projection_elem(elem, ctx, loc);
            }

            fn visit_const(&mut self, _const: &$($mut)? Const, _loc: Location) {
            }

            fn visit_local(&mut self, _local: &$($mut)? Local, _ctx: PlaceContext, _loc: Location) {
            }

            // fn super_body(&mut self, _body: Body, data: &$($mut)? BodyData) {
            //     for (block, data) in blocks_iter!(data.blocks, $($mut)?) {
            //         self.visit_block(Block(block), data);
            //     }
            // }

            fn super_local_decl(&mut self, _local: Local, _data: &$($mut)? LocalData) {
            }

            fn super_block(&mut self, block: Block, data: &$($mut)? BlockData) {
                let BlockData { statements, terminator, .. } = data;

                for (i, stmt) in statements.into_iter().enumerate() {
                    self.visit_stmt(stmt, Location { block, statement: i });
                }

                self.visit_terminator(terminator, Location { block, statement: statements.len() });
            }

            fn super_stmt(&mut self, stmt: &$($mut)? Statement, loc: Location) {
                match stmt {
                    | Statement::Init(local) => self.visit_local(local, PlaceContext::MutUse(MutUseContext::Init), loc),
                    | Statement::Drop(place) => self.visit_place(place, PlaceContext::MutUse(MutUseContext::Drop), loc),
                    | Statement::Assign(place, rvalue) => self.visit_assign(place, rvalue, loc),
                    | Statement::SetDiscriminant(place, _) => self.visit_place(place, PlaceContext::MutUse(MutUseContext::SetDiscriminant), loc),
                    | Statement::Intrinsic { place, args, .. } => {
                        self.visit_place(place, PlaceContext::MutUse(MutUseContext::Call), loc);

                        for arg in args {
                            self.visit_operand(arg, loc);
                        }
                    },
                    | Statement::Call { place, func, args } => {
                        self.visit_place(place, PlaceContext::MutUse(MutUseContext::Call), loc);
                        self.visit_operand(func, loc);

                        for arg in args {
                            self.visit_operand(arg, loc);
                        }
                    },
                }
            }

            fn super_assign(&mut self, place: &$($mut)? Place, rvalue: &$($mut)? RValue, loc: Location) {
                self.visit_place(place, PlaceContext::MutUse(MutUseContext::Store), loc);
                self.visit_rvalue(rvalue, loc);
            }

            fn super_terminator(&mut self, terminator: &$($mut)? Terminator, loc: Location) {
                match terminator {
                    | Terminator::None | Terminator::Unreachable | Terminator::Abort => {},
                    | Terminator::Return(op) => self.visit_operand(op, loc),
                    | Terminator::Jump(target) => self.visit_jump_target(target, loc),
                    | Terminator::Switch { discr, targets, .. } => {
                        self.visit_operand(discr, loc);

                        for target in targets {
                            self.visit_jump_target(target, loc);
                        }
                    },
                }
            }

            fn super_jump_target(&mut self, target: &$($mut)? JumpTarget, loc: Location) {
                let JumpTarget { args, .. } = target;

                for arg in args {
                    self.visit_operand(arg, loc);
                }
            }

            fn super_rvalue(&mut self, rvalue: &$($mut)? RValue, loc: Location) {
                match rvalue {
                    | RValue::Use(op) => self.visit_operand(op, loc),
                    | RValue::AddrOf(place) => self.visit_place(place, PlaceContext::Use(UseContext::AddrOf), loc),
                    | RValue::Cast(_, op) => self.visit_operand(op, loc),
                    | RValue::BinOp(_, lhs, rhs) => {
                        self.visit_operand(lhs, loc);
                        self.visit_operand(rhs, loc);
                    },
                    | RValue::NullOp(_, _) => {},
                    | RValue::Discriminant(place) => self.visit_place(place, PlaceContext::Use(UseContext::Inspect), loc),
                }
            }

            fn super_operand(&mut self, op: &$($mut)? Operand, loc: Location) {
                match op {
                    | Operand::Move(place) => self.visit_place(place, PlaceContext::Use(UseContext::Move), loc),
                    | Operand::Copy(place) => self.visit_place(place, PlaceContext::Use(UseContext::Copy), loc),
                    | Operand::Const(const_, _) => self.visit_const(const_, loc),
                }
            }

            fn super_place(&mut self, place: &$($mut)? Place, mut ctx: PlaceContext, loc: Location) {
                if !place.projection.is_empty() {
                    if !matches!(ctx, PlaceContext::NonUse(_)) {
                        ctx = if matches!(ctx, PlaceContext::MutUse(_)) {
                            PlaceContext::MutUse(MutUseContext::Projection)
                        } else {
                            PlaceContext::Use(UseContext::Projection)
                        };
                    }
                }

                self.visit_local(&$($mut)? place.local, ctx, loc);
                self.visit_projection(place, ctx, loc);
            }

            fn super_projection(&mut self, place: &$($mut)? Place, ctx: PlaceContext, loc: Location) {
                let Place { projection, .. } = place;

                for elem in projection.into_iter().rev() {
                    self.visit_projection_elem(elem, ctx, loc);
                }
            }

            fn super_projection_elem(&mut self, elem: &$($mut)? Projection, _ctx: PlaceContext, loc: Location) {
                match elem {
                    | Projection::Index(index) => self.visit_operand(index, loc),
                    | Projection::Slice(lo, hi) => {
                        self.visit_operand(lo, loc);
                        self.visit_operand(hi, loc);
                    },
                    | _ => {},
                }
            }
        }
    };
}

// macro_rules! blocks_iter {
//     ($blocks:expr,mut) => {
//         $blocks.iter_mut()
//     };
//     ($blocks:expr,) => {
//         $blocks.iter()
//     };
// }

make_visitor!(Visitor,);
make_visitor!(MutVisitor, mut);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PlaceContext {
    NonUse(NonUseContext),
    MutUse(MutUseContext),
    Use(UseContext),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NonUseContext {
    Init,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MutUseContext {
    Init,
    Drop,
    Store,
    Call,
    Projection,
    SetDiscriminant,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UseContext {
    Inspect,
    Copy,
    Move,
    AddrOf,
    Projection,
}
