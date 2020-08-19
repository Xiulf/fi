use crate::FunctionCtx;
use cranelift::codegen::ir::{self, InstBuilder};
use cranelift_module::Backend;

impl<'a, 'tcx, B: Backend> FunctionCtx<'a, 'tcx, B> {
    pub fn trans_term(&mut self, term: &mir::Term<'tcx>) {
        match term {
            mir::Term::Unset => unreachable!(),
            mir::Term::Abort => {
                self.builder.ins().trap(ir::TrapCode::User(0));
            }
            mir::Term::Return => {
                let rets = match crate::pass::pass_mode(
                    self.module,
                    self.tcx.layout(self.body.locals[&mir::LocalId::RET].ty),
                ) {
                    crate::pass::PassMode::ByVal(_) => {
                        let place = self.locals[&mir::LocalId::RET];
                        let ret_val = place.to_value(self).load_scalar(self);

                        vec![ret_val]
                    }
                    _ => Vec::new(),
                };

                self.builder.ins().return_(&rets);
            }
            mir::Term::Jump(to) => {
                self.builder.ins().jump(self.blocks[to], &[]);
            }
            mir::Term::Switch(val, vals, targets) => {
                let mut switch = cranelift::frontend::Switch::new();
                let otherwise = self.blocks[targets.last().unwrap()];
                let val = self.trans_operand(val).load_scalar(self);

                for (val, target) in vals.iter().zip(targets.iter()) {
                    switch.set_entry(*val, self.blocks[target]);
                }

                switch.emit(&mut self.builder, val, otherwise);
            }
        }
    }
}
