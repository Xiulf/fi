use crate::place::Place;
use crate::value::Value;
use crate::FunctionCtx;
use check::ty::{Layout, Type};
use cranelift::codegen::ir::{self, InstBuilder};
use cranelift_module::Backend;

impl<'a, 'tcx, B: Backend> FunctionCtx<'a, 'tcx, B> {
    pub fn trans_rvalue(&mut self, place: Place<'tcx>, rvalue: &mir::RValue<'tcx>) {
        match rvalue {
            mir::RValue::Use(op) => {
                let value = self.trans_operand(op);

                place.store(self, value);
            }
            mir::RValue::Ref(val) => {
                let val = self.trans_place(val);

                val.write_place_ref(self, place);
            }
            mir::RValue::Cast(ty, op) => {
                let op = self.trans_operand(op);

                match (
                    self.clif_type(op.layout),
                    self.clif_type(self.tcx.layout(ty)),
                ) {
                    (Some(_), Some(to_ty)) => {
                        let from = op.load_scalar(self);
                        let value = self.trans_cast(
                            from,
                            op.layout.abi.is_signed(),
                            to_ty,
                            self.tcx.layout(ty).abi.is_signed(),
                        );

                        place.store(self, Value::new_val(value, place.layout));

                        return;
                    }
                    _ => {}
                }

                place.store(self, op.cast(self.tcx.layout(ty)));
            }
            mir::RValue::BinOp(op, lhs, rhs) => {
                let lhs = self.trans_operand(lhs);
                let rhs = self.trans_operand(rhs);
                let layout = lhs.layout;
                let lhs = lhs.load_scalar(self);
                let rhs = rhs.load_scalar(self);
                let val = self.trans_binop(op, lhs, rhs, layout);
                let val = Value::new_val(val, place.layout);

                place.store(self, val);
            }
            mir::RValue::UnOp(op, rhs) => {
                let rhs = self.trans_operand(rhs);
                let layout = rhs.layout;
                let rhs = rhs.load_scalar(self);
                let val = self.trans_unop(op, rhs, layout);
                let val = Value::new_val(val, place.layout);

                place.store(self, val);
            }
            mir::RValue::Init(ty, ops) => match ty {
                Type::Array(_, _) => {
                    for (i, op) in ops.iter().enumerate() {
                        let val = self.trans_operand(op);
                        let idx = self.builder.ins().iconst(self.pointer_type, i as i64);

                        place.index(self, idx).store(self, val);
                    }
                }
                Type::Tuple(_) => {
                    for (i, op) in ops.iter().enumerate() {
                        let val = self.trans_operand(op);

                        place.field(self, i).store(self, val);
                    }
                }
                Type::Struct(_, _) => {
                    for (i, op) in ops.iter().enumerate() {
                        let val = self.trans_operand(op);

                        place.field(self, i).store(self, val);
                    }
                }
                _ => unimplemented!(),
            },
        }
    }

    fn trans_binop(
        &mut self,
        op: &mir::BinOp,
        lhs: ir::Value,
        rhs: ir::Value,
        layout: Layout<'tcx>,
    ) -> ir::Value {
        use ir::condcodes::{FloatCC, IntCC};

        match op {
            mir::BinOp::Add => {
                if layout.abi.is_float() {
                    self.builder.ins().fadd(lhs, rhs)
                } else {
                    self.builder.ins().iadd(lhs, rhs)
                }
            }
            mir::BinOp::Sub => {
                if layout.abi.is_float() {
                    self.builder.ins().fsub(lhs, rhs)
                } else {
                    self.builder.ins().isub(lhs, rhs)
                }
            }
            mir::BinOp::Mul => {
                if layout.abi.is_float() {
                    self.builder.ins().fmul(lhs, rhs)
                } else {
                    self.builder.ins().imul(lhs, rhs)
                }
            }
            mir::BinOp::Div => {
                if layout.abi.is_float() {
                    self.builder.ins().fdiv(lhs, rhs)
                } else if layout.abi.is_signed() {
                    self.builder.ins().sdiv(lhs, rhs)
                } else {
                    self.builder.ins().udiv(lhs, rhs)
                }
            }
            mir::BinOp::Rem => {
                if layout.abi.is_signed() {
                    self.builder.ins().srem(lhs, rhs)
                } else {
                    self.builder.ins().urem(lhs, rhs)
                }
            }
            mir::BinOp::BitAnd => self.builder.ins().band(lhs, rhs),
            mir::BinOp::BitOr => self.builder.ins().bor(lhs, rhs),
            mir::BinOp::BitXOr => self.builder.ins().bxor(lhs, rhs),
            mir::BinOp::Shl => self.builder.ins().ishl(lhs, rhs),
            mir::BinOp::Shr => {
                if layout.abi.is_signed() {
                    self.builder.ins().sshr(lhs, rhs)
                } else {
                    self.builder.ins().ushr(lhs, rhs)
                }
            }
            mir::BinOp::Lt => {
                let val = if layout.abi.is_float() {
                    self.builder.ins().fcmp(FloatCC::LessThan, lhs, rhs)
                } else if layout.abi.is_signed() {
                    self.builder.ins().icmp(IntCC::SignedLessThan, lhs, rhs)
                } else {
                    self.builder.ins().icmp(IntCC::UnsignedLessThan, lhs, rhs)
                };

                self.builder.ins().bint(ir::types::I8, val)
            }
            mir::BinOp::Le => {
                let val = if layout.abi.is_float() {
                    self.builder.ins().fcmp(FloatCC::LessThanOrEqual, lhs, rhs)
                } else if layout.abi.is_signed() {
                    self.builder
                        .ins()
                        .icmp(IntCC::SignedLessThanOrEqual, lhs, rhs)
                } else {
                    self.builder
                        .ins()
                        .icmp(IntCC::UnsignedLessThanOrEqual, lhs, rhs)
                };

                self.builder.ins().bint(ir::types::I8, val)
            }
            mir::BinOp::Gt => {
                let val = if layout.abi.is_float() {
                    self.builder.ins().fcmp(FloatCC::GreaterThan, lhs, rhs)
                } else if layout.abi.is_signed() {
                    self.builder.ins().icmp(IntCC::SignedGreaterThan, lhs, rhs)
                } else {
                    self.builder
                        .ins()
                        .icmp(IntCC::UnsignedGreaterThan, lhs, rhs)
                };

                self.builder.ins().bint(ir::types::I8, val)
            }
            mir::BinOp::Ge => {
                let val = if layout.abi.is_float() {
                    self.builder
                        .ins()
                        .fcmp(FloatCC::GreaterThanOrEqual, lhs, rhs)
                } else if layout.abi.is_signed() {
                    self.builder
                        .ins()
                        .icmp(IntCC::SignedGreaterThanOrEqual, lhs, rhs)
                } else {
                    self.builder
                        .ins()
                        .icmp(IntCC::UnsignedGreaterThanOrEqual, lhs, rhs)
                };

                self.builder.ins().bint(ir::types::I8, val)
            }
            mir::BinOp::Eq => {
                let val = if layout.abi.is_float() {
                    self.builder.ins().fcmp(FloatCC::Equal, lhs, rhs)
                } else {
                    self.builder.ins().icmp(IntCC::Equal, lhs, rhs)
                };

                self.builder.ins().bint(ir::types::I8, val)
            }
            mir::BinOp::Ne => {
                let val = if layout.abi.is_float() {
                    self.builder.ins().fcmp(FloatCC::NotEqual, lhs, rhs)
                } else {
                    self.builder.ins().icmp(IntCC::NotEqual, lhs, rhs)
                };

                self.builder.ins().bint(ir::types::I8, val)
            }
        }
    }

    fn trans_unop(&mut self, op: &mir::UnOp, rhs: ir::Value, layout: Layout<'tcx>) -> ir::Value {
        match op {
            mir::UnOp::Not => self.builder.ins().bnot(rhs),
            mir::UnOp::Neg => {
                if layout.abi.is_float() {
                    self.builder.ins().fneg(rhs)
                } else {
                    self.builder.ins().ineg(rhs)
                }
            }
        }
    }
}
