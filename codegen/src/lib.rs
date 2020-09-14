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

pub fn compile<'tcx>(
    tcx: &Tcx<'tcx>,
    package: &mir::Package<'tcx>,
    out_file: impl AsRef<std::path::Path>,
) {
    let out_file = out_file.as_ref();
    let product = trans::translate(tcx, package);
    let mut tmp_name = out_file.to_owned();
    tmp_name.set_extension("o");

    assemble(product, tmp_name.as_ref());
    link(tmp_name.as_ref(), out_file.as_ref());
}

pub fn assemble(product: cranelift_object::ObjectProduct, out_file: &std::path::Path) {
    use std::io::Write;
    let bytes = product.emit().unwrap();

    std::fs::File::create(out_file)
        .unwrap()
        .write_all(&bytes)
        .unwrap();
}

pub fn link(obj_file: &std::path::Path, out_file: &std::path::Path) {
    let _status = std::process::Command::new("cc")
        .arg(obj_file)
        .arg("-o")
        .arg(out_file)
        // .arg("-lc")
        .status()
        .unwrap();
}

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
    ssa_vars: u32,
}

impl<'a, 'tcx, B: Backend> FunctionCtx<'a, 'tcx, B> {
    pub fn next_ssa_var(&mut self) -> u32 {
        self.ssa_vars += 1;
        self.ssa_vars
    }

    pub fn clif_type(&self, layout: Layout<'tcx>) -> Option<types::Type> {
        self::clif_type(self.module, layout)
    }

    pub fn clif_pair_type(&self, layout: Layout<'tcx>) -> Option<(types::Type, types::Type)> {
        self::clif_pair_type(self.module, layout)
    }
}

pub fn clif_type<'tcx>(module: &Module<impl Backend>, layout: Layout<'tcx>) -> Option<types::Type> {
    match layout.ty {
        Type::Bool => Some(types::I8),
        Type::TypeId => Some(module.target_config().pointer_type()),
        Type::VInt(_) | Type::VUInt(_) | Type::Int(0) | Type::UInt(0) => {
            match module.target_config().pointer_width {
                target_lexicon::PointerWidth::U16 => Some(types::I16),
                target_lexicon::PointerWidth::U32 => Some(types::I32),
                target_lexicon::PointerWidth::U64 => Some(types::I64),
            }
        }
        Type::VFloat(_) | Type::Float(0) => match module.target_config().pointer_width {
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
        Type::Func(_, _, _) => Some(module.target_config().pointer_type()),
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

pub fn clif_pair_type(
    module: &Module<impl Backend>,
    layout: Layout,
) -> Option<(types::Type, types::Type)> {
    let ptr_ty = module.target_config().pointer_type();

    match layout.ty {
        Type::Str => Some((ptr_ty, ptr_ty)),
        Type::Slice(_) => Some((ptr_ty, ptr_ty)),
        _ => None,
    }
}
