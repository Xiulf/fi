use std::sync::Arc;

use inkwell::types;
use mir::layout::{Abi, ReprAndLayout, Size};
use mir::repr::Signature;

use crate::ctx::CodegenCtx;

#[derive(Debug, Clone, Copy)]
pub enum PassMode<'ctx> {
    NoPass,
    ByVal(types::BasicTypeEnum<'ctx>),
    ByValPair(types::BasicTypeEnum<'ctx>, types::BasicTypeEnum<'ctx>),
    ByRef { size: Option<Size> },
}

#[derive(Clone, Copy)]
pub enum EmptySinglePair<T> {
    Empty,
    Single(T),
    Pair(T, T),
}

#[derive(Debug, Clone)]
pub struct FnAbi<'ctx> {
    pub args: Box<[ArgAbi<'ctx>]>,
    pub ret: ArgAbi<'ctx>,
}

#[derive(Debug, Clone)]
pub struct ArgAbi<'ctx> {
    pub layout: Arc<ReprAndLayout>,
    pub mode: PassMode<'ctx>,
}

impl ArgAbi<'_> {
    pub fn is_indirect(&self) -> bool {
        matches!(self.mode, PassMode::ByRef { .. })
    }
}

impl<T> Iterator for EmptySinglePair<T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        match std::mem::replace(self, EmptySinglePair::Empty) {
            | EmptySinglePair::Empty => None,
            | EmptySinglePair::Single(v) => Some(v),
            | EmptySinglePair::Pair(a, b) => {
                *self = EmptySinglePair::Single(b);
                Some(a)
            },
        }
    }
}

impl<'ctx> CodegenCtx<'_, 'ctx> {
    pub fn compute_fn_abi(&self, sig: &Signature) -> FnAbi<'ctx> {
        let ret_layout = self.db.layout_of(sig.ret.clone());
        let ret = self.compute_layout_abi(ret_layout);
        let args = sig
            .params
            .iter()
            .map(|a| {
                let layout = self.db.layout_of(a.clone());
                self.compute_layout_abi(layout)
            })
            .collect();

        FnAbi { args, ret }
    }

    pub fn compute_layout_abi(&self, layout: Arc<ReprAndLayout>) -> ArgAbi<'ctx> {
        let pass_mode = self.pass_mode(&layout);

        ArgAbi {
            layout,
            mode: pass_mode,
        }
    }

    pub fn pass_mode(&self, layout: &ReprAndLayout) -> PassMode<'ctx> {
        if layout.is_zst() {
            PassMode::NoPass
        } else {
            match &layout.abi {
                | Abi::Uninhabited => PassMode::NoPass,
                | Abi::Scalar(s) => PassMode::ByVal(self.basic_type_for_scalar(s)),
                | Abi::ScalarPair(a, b) => {
                    let a_ty = self.basic_type_for_ral(&layout.field(self.db, 0).unwrap());
                    let b_ty = self.basic_type_for_ral(&layout.field(self.db, 1).unwrap());

                    if a.value.size(self.triple).bits() == 128 && b.value.size(self.triple).bits() == 128 {
                        PassMode::ByRef {
                            size: Some(layout.size),
                        }
                    } else {
                        PassMode::ByValPair(a_ty, b_ty)
                    }
                },
                | Abi::Aggregate { sized: true } => PassMode::ByRef {
                    size: Some(layout.size),
                },
                | Abi::Aggregate { sized: false } => PassMode::ByRef { size: None },
            }
        }
    }
}

// impl<'ctx> CodegenCtx<'_, 'ctx> {
//     pub fn value_for_arg(&mut self, arg: ValueRef) -> EmptySinglePair<types::BasicTypeEnum<'ctx>> {
//         match self.pass_mode(&arg.layout) {
//             | PassMode::NoPass => EmptySinglePair::Empty,
//             | PassMode::ByVal(_) => EmptySinglePair::Single(arg.load_scalar(self)),
//             | PassMode::ByValPair(_, _) => {
//                 let (a, b) = arg.load_scalar_pair(self);

//                 EmptySinglePair::Pair(a, b)
//             },
//             | PassMode::ByRef { size: _ } => match arg.on_stack(self) {
//                 | (ptr, None) => EmptySinglePair::Single(ptr.get_addr(self)),
//                 | (ptr, Some(meta)) => EmptySinglePair::Pair(ptr.get_addr(self), meta),
//             },
//         }
//     }

//     pub fn value_for_ret(&mut self, ret: ir::LocalId) -> EmptySinglePair<clif::Value> {
//         let place = self.locals[ret].clone();

//         match self.pass_mode(&place.layout) {
//             | PassMode::NoPass => EmptySinglePair::Empty,
//             | PassMode::ByVal(_) => EmptySinglePair::Single(place.to_value(self).load_scalar(self)),
//             | PassMode::ByValPair(_, _) => {
//                 let (a, b) = place.to_value(self).load_scalar_pair(self);

//                 EmptySinglePair::Pair(a, b)
//             },
//             | PassMode::ByRef { size: _ } => EmptySinglePair::Empty,
//         }
//     }
// }
