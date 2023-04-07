use mir::ir::Local;
use rustc_hash::FxHashSet;
use triomphe::Arc;

use crate::abi::PassMode;
use crate::ctx::{BodyCtx, CodegenCtx};
use crate::layout::{repr_and_layout, ReprAndLayout};
use crate::operand::{OperandRef, OperandValue};
use crate::place::PlaceRef;

#[derive(Debug, Clone)]
pub enum LocalRef<'ctx> {
    Place(PlaceRef<'ctx>),
    Operand(Option<OperandRef<'ctx>>),
}

impl<'ctx> LocalRef<'ctx> {
    pub fn new_operand(ctx: &mut CodegenCtx<'_, 'ctx>, layout: Arc<ReprAndLayout>) -> Self {
        if layout.is_zst() {
            Self::Operand(Some(OperandRef::new_zst(ctx, layout)))
        } else {
            Self::Operand(None)
        }
    }
}

impl<'ctx> BodyCtx<'_, '_, 'ctx> {
    pub fn arg_local_refs(&mut self, by_ref_locals: &FxHashSet<Local>) -> Vec<LocalRef<'ctx>> {
        let block = self.body.blocks.iter().next().unwrap().1;
        let mut index = self.fn_abi.ret.is_indirect() as u32;

        block
            .params
            .iter()
            .map(|arg| {
                let repr = self.instance.subst_repr(self.db, &self.body.locals[arg.0].repr);
                let layout = repr_and_layout(self.db, repr);
                let pass_mode = self.pass_mode(&layout);

                if !by_ref_locals.contains(arg) {
                    let local = |op| LocalRef::Operand(Some(op));

                    match pass_mode {
                        | PassMode::NoPass => {
                            return local(OperandRef::new_zst(self.cx, layout));
                        },
                        | PassMode::ByVal(_) => {
                            let arg = self.func.get_nth_param(index).unwrap();
                            index += 1;
                            return local(OperandRef::new_imm(layout, arg));
                        },
                        | PassMode::ByValPair(..) => {
                            let a = self.func.get_nth_param(index).unwrap();
                            let b = self.func.get_nth_param(index + 1).unwrap();
                            index += 2;
                            return local(OperandRef::new_pair(layout, a, b));
                        },
                        | _ => {},
                    }
                }

                if let PassMode::ByRef { .. } = pass_mode {
                    if !layout.abi.is_unsized() {
                        let arg = self.func.get_nth_param(index).unwrap().into_pointer_value();
                        index += 1;
                        LocalRef::Place(PlaceRef::new(layout, arg, None))
                    } else {
                        let a = self.func.get_nth_param(index).unwrap().into_pointer_value();
                        let b = self.func.get_nth_param(index + 1).unwrap();
                        index += 2;
                        LocalRef::Place(PlaceRef::new(layout, a, Some(b)))
                    }
                } else {
                    let tmp = PlaceRef::new_alloca(self.cx, layout);

                    match pass_mode {
                        | PassMode::NoPass => {},
                        | PassMode::ByVal(_) | PassMode::ByRef { size: Some(_) } => {
                            let arg = self.func.get_nth_param(index).unwrap();
                            index += 1;
                            OperandValue::Imm(arg).store(self.cx, &tmp);
                        },
                        | PassMode::ByValPair(_, _) => {
                            let a = self.func.get_nth_param(index).unwrap();
                            let b = self.func.get_nth_param(index + 1).unwrap();
                            index += 2;
                            OperandValue::Pair(a, b).store(self.cx, &tmp);
                        },
                        | PassMode::ByRef { size: None } => {
                            let a = self.func.get_nth_param(index).unwrap().into_pointer_value();
                            let b = self.func.get_nth_param(index + 1).unwrap();
                            index += 2;
                            OperandValue::Ref(a, Some(b)).store(self.cx, &tmp);
                        },
                    };

                    LocalRef::Place(tmp)
                }
            })
            .collect()
    }
}
