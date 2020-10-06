use crate::value::Value;
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
                    crate::pass::PassMode::ByPair(_, _) => {
                        let place = self.locals[&mir::LocalId::RET];
                        let (a, b) = place.to_value(self).load_scalar_pair(self);

                        vec![a, b]
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
            mir::Term::Call(place, func, args, target) => {
                let place = self.trans_place(place);
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

                let inst = if let mir::Operand::Const(mir::Const::FuncAddr(id), _) = func {
                    if self.package.items[id].is_intrinsic() {
                        call_intrinsic_match! {
                            self, &**self.package.items[id].name.symbol, &args[..], place, target,
                            sqrtf32(f) -> sqrt,
                            sqrtf64(f) -> sqrt,
                        }

                        self.tcx
                            .bug(None, "unknown intrinsic", self.package.items[id].name.span);
                        self.tcx.reporter.report(true);
                        unreachable!();
                    } else {
                        let func = self.func_ids[id].0;
                        let func = self.module.declare_func_in_func(func, self.builder.func);

                        self.builder.ins().call(func, &args)
                    }
                } else {
                    let (_, params, ret) = self.type_of_op(func).func().unwrap();
                    let func = self.trans_operand(func).load_scalar(self);
                    let sig = crate::pass::call_sig(self, params, ret);
                    let sig = self.builder.import_signature(sig);

                    self.builder.ins().call_indirect(sig, func, &args)
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

                self.builder.ins().jump(self.blocks[target], &[]);
            }
        }
    }
}

macro call_intrinsic_match {
    ($fx:expr, $intrinsic:expr, $args:expr, $ret:expr, $next:expr, $(
        $name:ident($($arg:ident),*) -> $func:ident,
    )*) => {
        match $intrinsic {
            $(
                stringify!($name) => {
                    if let [$($arg),*] = $args {
                        let val = Value::new_val($fx.builder.ins().$func($($arg.clone()),*), $ret.layout);

                        $ret.store($fx, val);
                        $fx.builder.ins().jump($fx.blocks[$next], &[]);
                        return;
                    }
                }
            )*
            _ => {},
        }
    }
}
