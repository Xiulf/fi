use crate::*;
use check::ty::Ty;
use layout::{Abi, Size, TyLayout};

#[derive(Clone, Copy)]
pub enum PassMode<B: Backend> {
    NoPass,
    ByVal(<B::Type as Type>::Raw),
    ByValPair(<B::Type as Type>::Raw, <B::Type as Type>::Raw),
    ByRef { size: Option<Size> },
}

#[derive(Clone, Copy)]
pub enum EmptySinglePair<T> {
    Empty,
    Single(T),
    Pair(T, T),
}

impl<T> EmptySinglePair<T> {
    pub fn map<U>(self, mut f: impl FnMut(T) -> U) -> EmptySinglePair<U> {
        match self {
            EmptySinglePair::Empty => EmptySinglePair::Empty,
            EmptySinglePair::Single(v) => EmptySinglePair::Single(f(v)),
            EmptySinglePair::Pair(a, b) => EmptySinglePair::Pair(f(a), f(b)),
        }
    }
}

impl<T: std::fmt::Debug> EmptySinglePair<T> {
    pub fn assert_single(self) -> T {
        match self {
            EmptySinglePair::Single(v) => v,
            _ => unreachable!(),
        }
    }

    pub fn assert_pair(self) -> (T, T) {
        match self {
            EmptySinglePair::Pair(a, b) => (a, b),
            _ => unreachable!(),
        }
    }
}

impl<T> Iterator for EmptySinglePair<T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        match std::mem::replace(self, EmptySinglePair::Empty) {
            EmptySinglePair::Empty => None,
            EmptySinglePair::Single(v) => Some(v),
            EmptySinglePair::Pair(a, b) => {
                *self = EmptySinglePair::Single(b);
                Some(a)
            }
        }
    }
}

pub fn get_pass_mode<B: Backend>(fx: &FunctionCtx<B>, layout: &TyLayout<Ty>) -> PassMode<B> {
    if layout.is_zst() {
        PassMode::NoPass
    } else {
        match &layout.abi {
            Abi::Uninhabited => PassMode::NoPass,
            Abi::Scalar(scalar) => PassMode::ByVal(fx.scalar_ty(scalar)),
            Abi::ScalarPair(a, b) => {
                let a_ty = fx.scalar_ty(a);
                let b_ty = fx.scalar_ty(b);
                let target = fx.db.target(fx.lib);

                if a.value.size(&target).bits() == 128 && b.value.size(&target).bits() == 128 {
                    PassMode::ByRef {
                        size: Some(layout.size),
                    }
                } else {
                    PassMode::ByValPair(a_ty, b_ty)
                }
            }
            Abi::Aggregate { sized: true } => PassMode::ByRef {
                size: Some(layout.size),
            },
            Abi::Aggregate { sized: false } => PassMode::ByRef { size: None },
        }
    }
}
