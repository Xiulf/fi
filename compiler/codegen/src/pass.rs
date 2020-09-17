use crate::ptr::Pointer;
use crate::value::Value;
use crate::FunctionCtx;
use check::layout::Abi;
use check::ty::{Layout, Param, Ty};
use cranelift::codegen::ir as cir;
use cranelift_module::{Backend, Module};

#[derive(Debug)]
pub enum PassMode {
    ByRef { sized: bool },
    ByVal(cir::Type),
    ByPair(cir::Type, cir::Type),
    NoPass,
}

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

impl<T> Iterator for EmptySinglePair<T> {
    type Item = T;

    fn next(&mut self) -> Option<T> {
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

impl PassMode {
    pub fn get_param_ty(self, module: &Module<impl Backend>) -> EmptySinglePair<cir::Type> {
        match self {
            PassMode::NoPass => EmptySinglePair::Empty,
            PassMode::ByVal(ty) => EmptySinglePair::Single(ty),
            PassMode::ByPair(a, b) => EmptySinglePair::Pair(a, b),
            PassMode::ByRef { sized: true } => {
                EmptySinglePair::Single(module.target_config().pointer_type())
            }
            PassMode::ByRef { sized: false } => EmptySinglePair::Pair(
                module.target_config().pointer_type(),
                module.target_config().pointer_type(),
            ),
        }
    }
}

pub fn pass_mode<'tcx>(module: &Module<impl Backend>, layout: Layout<'tcx>) -> PassMode {
    if layout.is_zst() {
        PassMode::NoPass
    } else {
        match &layout.abi {
            Abi::Uninhabited => PassMode::NoPass,
            Abi::Scalar(scalar) => PassMode::ByVal(super::scalar_clif_type(module, scalar)),
            Abi::ScalarPair(a, b) => {
                let a = super::scalar_clif_type(module, a);
                let b = super::scalar_clif_type(module, b);

                if a == cir::types::I128 && b == cir::types::I128 {
                    PassMode::ByRef { sized: true }
                } else {
                    PassMode::ByPair(a, b)
                }
            }
            Abi::Aggregate { sized } => PassMode::ByRef { sized: *sized },
        }
    }
}

pub fn value_for_param<'a, 'tcx>(
    fx: &mut FunctionCtx<'a, 'tcx, impl Backend>,
    start_ebb: cir::Block,
    layout: Layout<'tcx>,
) -> Option<Value<'tcx>> {
    match pass_mode(fx.module, layout) {
        PassMode::NoPass => None,
        PassMode::ByVal(clif_type) => {
            let ebb_param = fx.builder.append_block_param(start_ebb, clif_type);

            Some(Value::new_val(ebb_param, layout))
        }
        PassMode::ByPair(a, b) => {
            let a = fx.builder.append_block_param(start_ebb, a);
            let b = fx.builder.append_block_param(start_ebb, b);

            Some(Value::new_pair(a, b, layout))
        }
        PassMode::ByRef { sized: true } => {
            let ebb_param = fx.builder.append_block_param(start_ebb, fx.pointer_type);

            Some(Value::new_ref(Pointer::addr(ebb_param), layout))
        }
        PassMode::ByRef { sized: false } => {
            let ptr = fx.builder.append_block_param(start_ebb, fx.pointer_type);
            let meta = fx.builder.append_block_param(start_ebb, fx.pointer_type);

            Some(Value::new_ref_meta(Pointer::addr(ptr), meta, layout))
        }
    }
}

pub fn value_for_arg<'a, 'tcx>(
    fx: &mut FunctionCtx<'a, 'tcx, impl Backend>,
    arg: Value<'tcx>,
) -> EmptySinglePair<cir::Value> {
    match pass_mode(fx.module, arg.layout) {
        PassMode::NoPass => EmptySinglePair::Empty,
        PassMode::ByVal(_) => EmptySinglePair::Single(arg.load_scalar(fx)),
        PassMode::ByPair(_, _) => {
            let (a, b) = arg.load_scalar_pair(fx);

            EmptySinglePair::Pair(a, b)
        }
        PassMode::ByRef { sized: _ } => match arg.on_stack(fx) {
            (ptr, None) => EmptySinglePair::Single(ptr.get_addr(fx)),
            (ptr, Some(meta)) => EmptySinglePair::Pair(ptr.get_addr(fx), meta),
        },
    }
}

pub fn call_sig<'tcx>(
    fx: &FunctionCtx<'_, 'tcx, impl Backend>,
    params: &'tcx [Param<'tcx>],
    ret: Ty<'tcx>,
) -> cir::Signature {
    let params = params
        .iter()
        .map(|param| pass_mode(fx.module, fx.tcx.layout(param.ty)).get_param_ty(fx.module))
        .flatten();
    let (params, returns) = match pass_mode(fx.module, fx.tcx.layout(ret)) {
        PassMode::NoPass => (params.map(cir::AbiParam::new).collect(), vec![]),
        PassMode::ByVal(ret_ty) => (
            params.map(cir::AbiParam::new).collect(),
            vec![cir::AbiParam::new(ret_ty)],
        ),
        PassMode::ByPair(a, b) => (
            params.map(cir::AbiParam::new).collect(),
            vec![cir::AbiParam::new(a), cir::AbiParam::new(b)],
        ),
        PassMode::ByRef { sized: true } => (
            Some(fx.pointer_type)
                .into_iter()
                .chain(params)
                .map(cir::AbiParam::new)
                .collect(),
            vec![],
        ),
        PassMode::ByRef { sized: false } => unimplemented!(),
    };

    cir::Signature {
        params,
        returns,
        call_conv: fx.module.isa().default_call_conv(),
    }
}
