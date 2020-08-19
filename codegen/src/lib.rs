pub mod analyze;
pub mod pass;
pub mod place;
pub mod ptr;
pub mod trans;
pub mod value;

use check::layout::{Integer, Primitive, Scalar};
use check::tcx::Tcx;
use check::ty::{Layout, Type};
use cranelift::codegen::ir as cir;
use cranelift::prelude::*;
use cranelift_module::{Backend, DataId, FuncId, Module};
use std::collections::BTreeMap;

pub struct FunctionCtx<'a, 'tcx, B: Backend> {
    pub module: &'a mut Module<B>,
    pub builder: FunctionBuilder<'a>,
    pub pointer_type: types::Type,
    pub tcx: &'a Tcx<'tcx>,
    pub package: &'a mir::Package<'tcx>,
    pub body: &'a mir::Body<'tcx>,
    pub func_ids: &'a BTreeMap<mir::Id, (FuncId, cir::Signature, Layout<'tcx>)>,
    pub data_ids: &'a BTreeMap<mir::Id, (DataId, Layout<'tcx>)>,
    pub blocks: BTreeMap<mir::BlockId, cir::Block>,
    pub locals: BTreeMap<mir::LocalId, place::Place<'tcx>>,
    bytes_count: &'a mut usize,
}

impl<'a, 'tcx, B: Backend> FunctionCtx<'a, 'tcx, B> {
    pub fn clif_type(&self, layout: Layout<'tcx>) -> Option<types::Type> {
        self::clif_type(self.module, layout)
    }
}

pub fn clif_type<'tcx>(module: &Module<impl Backend>, layout: Layout<'tcx>) -> Option<types::Type> {
    match layout.ty {
        Type::Bool => Some(types::I8),
        Type::TypeId => Some(module.target_config().pointer_type()),
        Type::Int(0) | Type::UInt(0) => match module.target_config().pointer_width {
            target_lexicon::PointerWidth::U16 => Some(types::I16),
            target_lexicon::PointerWidth::U32 => Some(types::I32),
            target_lexicon::PointerWidth::U64 => Some(types::I64),
        },
        Type::Float(0) => match module.target_config().pointer_width {
            target_lexicon::PointerWidth::U16 => Some(types::F32),
            target_lexicon::PointerWidth::U32 => Some(types::F32),
            target_lexicon::PointerWidth::U64 => Some(types::F64),
        },
        Type::Int(size) | Type::UInt(size) => match size {
            8 => Some(types::I8),
            16 => Some(types::I16),
            32 => Some(types::I32),
            64 => Some(types::I64),
            128 => Some(types::I128),
            _ => unreachable!(),
        },
        Type::Float(size) => match size {
            32 => Some(types::F32),
            64 => Some(types::F64),
            _ => unreachable!(),
        },
        Type::Ref(_, _) => Some(module.target_config().pointer_type()),
        Type::Func(_, _) => Some(module.target_config().pointer_type()),
        _ => None,
    }
}

pub fn scalar_clif_type(module: &Module<impl Backend>, scalar: &Scalar) -> types::Type {
    match &scalar.value {
        Primitive::Int(int, _) => match int {
            Integer::I8 => types::I8,
            Integer::I16 => types::I16,
            Integer::I32 => types::I32,
            Integer::I64 => types::I64,
            Integer::I128 => types::I128,
        },
        Primitive::F32 => types::F32,
        Primitive::F64 => types::F64,
        Primitive::Pointer => module.target_config().pointer_type(),
    }
}
