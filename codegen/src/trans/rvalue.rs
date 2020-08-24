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
                _ => unimplemented!(),
            },
        }
    }

    fn trans_binop(
        &mut self,
        op: &mir::BinOp,
        lhs: ir::Value,
        rhs: ir::Value,
        _layout: Layout<'tcx>,
    ) -> ir::Value {
        match op {
            mir::BinOp::Add => self.builder.ins().iadd(lhs, rhs),
            mir::BinOp::Sub => self.builder.ins().isub(lhs, rhs),
            mir::BinOp::Div => self.builder.ins().udiv(lhs, rhs),
            mir::BinOp::Rem => self.builder.ins().urem(lhs, rhs),
            mir::BinOp::Lt => {
                let val = self
                    .builder
                    .ins()
                    .icmp(ir::condcodes::IntCC::SignedLessThan, lhs, rhs);

                self.builder.ins().bint(ir::types::I8, val)
            }
            mir::BinOp::Gt => {
                let val =
                    self.builder
                        .ins()
                        .icmp(ir::condcodes::IntCC::SignedGreaterThan, lhs, rhs);

                self.builder.ins().bint(ir::types::I8, val)
            }
            mir::BinOp::Eq => {
                let val = self
                    .builder
                    .ins()
                    .icmp(ir::condcodes::IntCC::Equal, lhs, rhs);

                self.builder.ins().bint(ir::types::I8, val)
            }
            _ => unimplemented!("{:?}", op),
        }
    }

    fn trans_unop(&mut self, op: &mir::UnOp, rhs: ir::Value, _layout: Layout<'tcx>) -> ir::Value {
        match op {
            mir::UnOp::Not => self.builder.ins().bnot(rhs),
            _ => unimplemented!(),
        }
    }
}
