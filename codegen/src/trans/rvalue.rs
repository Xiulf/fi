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
            mir::RValue::Ref(val) => {}
            mir::RValue::Cast(ty, op) => {
                let op = self.trans_operand(op);

                place.store(self, op.cast(self.tcx.layout(ty)));
            }
            mir::RValue::Call(func, args) => {
                let args = args
                    .iter()
                    .map(|a| self.trans_operand(a))
                    .collect::<Vec<_>>();
                let ret_mode = crate::pass::pass_mode(self.module, place.layout);
                let ret_ptr = match &ret_mode {
                    crate::pass::PassMode::ByRef { .. } => Some(place.as_ptr().get_addr(self)),
                    _ => None,
                };

                let args = ret_ptr
                    .into_iter()
                    .chain(
                        args.into_iter()
                            .map(|a| crate::pass::value_for_arg(self, a))
                            .flatten(),
                    )
                    .collect::<Vec<_>>();

                let inst = if let mir::Operand::Const(mir::Const::FuncAddr(id)) = func {
                    let func = self.func_ids[id].0;
                    let func = self.module.declare_func_in_func(func, self.builder.func);

                    self.builder.ins().call(func, &args)
                } else {
                    let _func = self.trans_operand(func).load_scalar(self);

                    // self.builder.ins().call_indirect(sig, func, &args)
                    unimplemented!()
                };

                match ret_mode {
                    crate::pass::PassMode::NoPass => {}
                    crate::pass::PassMode::ByRef { .. } => {}
                    crate::pass::PassMode::ByVal(_) => {
                        let ret_val = self.builder.inst_results(inst)[0];
                        let ret_val = Value::new_val(ret_val, place.layout);

                        place.store(self, ret_val);
                    }
                    crate::pass::PassMode::ByPair(_, _) => {
                        let ret_val = self.builder.inst_results(inst);
                        let ret_val = Value::new_pair(ret_val[0], ret_val[1], place.layout);

                        place.store(self, ret_val);
                    }
                }
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
            mir::RValue::Init(ty, ops) => match ty {
                Type::Array(_, _) => {
                    for (i, op) in ops.iter().enumerate() {
                        let val = self.trans_operand(op);
                        let idx = self.builder.ins().iconst(self.pointer_type, i as i64);

                        place.index(self, idx).store(self, val);
                    }
                }
                _ => unimplemented!(),
            },
            _ => unimplemented!("{}", rvalue),
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
            mir::BinOp::Lt => {
                let val = self
                    .builder
                    .ins()
                    .icmp(ir::condcodes::IntCC::SignedLessThan, lhs, rhs);

                self.builder.ins().bint(ir::types::I8, val)
            }
            _ => unimplemented!("{:?}", op),
        }
    }
}
