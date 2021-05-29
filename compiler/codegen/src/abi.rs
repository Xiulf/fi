use crate::*;
use mir::layout::{Abi, Layout, Size};

#[derive(Clone, Copy)]
pub enum PassMode {
    NoPass,
    ByVal(clif::Type),
    ByValPair(clif::Type, clif::Type),
    ByRef { size: Option<Size> },
}

#[derive(Clone, Copy)]
pub enum EmptySinglePair<T> {
    Empty,
    Single(T),
    Pair(T, T),
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

impl ModuleCtx<'_> {
    pub fn pass_mode(&self, layout: &Layout) -> PassMode {
        if layout.is_zst() {
            PassMode::NoPass
        } else {
            match &layout.abi {
                | Abi::Uninhabited => PassMode::NoPass,
                | Abi::Scalar(s) => PassMode::ByVal(self.scalar_type(s)),
                | Abi::ScalarPair(a, b) => {
                    let a_ty = self.scalar_type(a);
                    let b_ty = self.scalar_type(b);

                    if a.value.size(&self.triple).bits() == 128 && b.value.size(&self.triple).bits() == 128 {
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

impl FunctionCtx<'_, '_> {
    pub fn value_for_arg(&mut self, arg: ValueRef) -> EmptySinglePair<clif::Value> {
        match self.pass_mode(&arg.layout) {
            | PassMode::NoPass => EmptySinglePair::Empty,
            | PassMode::ByVal(_) => EmptySinglePair::Single(arg.load_scalar(self)),
            | PassMode::ByValPair(_, _) => {
                let (a, b) = arg.load_scalar_pair(self);

                EmptySinglePair::Pair(a, b)
            },
            | PassMode::ByRef { size: _ } => match arg.on_stack(self) {
                | (ptr, None) => EmptySinglePair::Single(ptr.get_addr(self)),
                | (ptr, Some(meta)) => EmptySinglePair::Pair(ptr.get_addr(self), meta),
            },
        }
    }

    pub fn value_for_ret(&mut self, ret: ir::LocalId) -> EmptySinglePair<clif::Value> {
        let place = self.locals[ret].clone();

        match self.pass_mode(&place.layout) {
            | PassMode::NoPass => EmptySinglePair::Empty,
            | PassMode::ByVal(_) => EmptySinglePair::Single(place.to_value(self).load_scalar(self)),
            | PassMode::ByValPair(_, _) => {
                let (a, b) = place.to_value(self).load_scalar_pair(self);

                EmptySinglePair::Pair(a, b)
            },
            | PassMode::ByRef { size: _ } => EmptySinglePair::Empty,
        }
    }
}
