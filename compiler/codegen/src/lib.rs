#![feature(decl_macro)]

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

#[derive(Debug, Clone, Copy)]
pub enum OutputType {
    Bin,
    Lib,
    DyLib,
}

pub fn compile<'a, 'tcx>(
    tcx: &Tcx<'tcx>,
    package: &mir::Package<'tcx>,
    target: &target_lexicon::Triple,
    out_type: OutputType,
    out_file: impl AsRef<std::path::Path>,
    libs: impl Iterator<Item = &'a std::path::Path>,
) {
    let mut out_file = out_file.as_ref().to_owned();
    let product = trans::translate(target, tcx, package);
    let mut tmp_name = out_file.to_owned();

    out_file.set_extension(out_type.extension(target));
    tmp_name.set_extension("o");

    std::fs::create_dir_all(out_file.parent().unwrap()).unwrap();
    assemble(product, tmp_name.as_ref());
    link(tmp_name.as_ref(), out_type, out_file.as_ref(), libs);
}

pub fn assemble(product: cranelift_object::ObjectProduct, out_file: &std::path::Path) {
    use std::io::Write;
    let bytes = product.emit().unwrap();

    std::fs::File::create(out_file)
        .unwrap()
        .write_all(&bytes)
        .unwrap();
}

pub fn link<'a>(
    obj_file: &std::path::Path,
    out_type: OutputType,
    out_file: &std::path::Path,
    libs: impl Iterator<Item = &'a std::path::Path>,
) {
    let mut cmd = std::process::Command::new("cc");

    cmd.arg(obj_file).arg("-o").arg(out_file);
    cmd.arg("-Wl,-R");

    for lib in libs {
        cmd.arg(format!(
            "-Wl,{}",
            lib.parent().unwrap().canonicalize().unwrap().display()
        ));
        cmd.arg(lib);
    }

    if let OutputType::DyLib = out_type {
        cmd.arg("-shared");
    }

    let _status = cmd.status().unwrap();
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

    pub fn type_of_op(&self, op: &mir::Operand<'tcx>) -> mir::Ty<'tcx> {
        match op {
            mir::Operand::Const(_, ty) => ty,
            mir::Operand::Move(place) | mir::Operand::Copy(place) => self.type_of_place(place),
        }
    }

    pub fn type_of_place(&self, place: &mir::Place<'tcx>) -> mir::Ty<'tcx> {
        let mut ty = match &place.base {
            mir::PlaceBase::Local(id) => self.locals[id].layout.ty,
            mir::PlaceBase::Global(id) => self.tcx.type_of(id),
        };

        for elem in &place.elems {
            ty = match elem {
                mir::PlaceElem::Deref => ty.pointee(self.tcx),
                mir::PlaceElem::Field(idx) => ty.fields(self.tcx)[*idx].1,
                mir::PlaceElem::Index(_) => ty.idx(self.tcx),
                mir::PlaceElem::Slice(..) => self.tcx.intern_ty(Type::Slice(ty.idx(self.tcx))),
            }
        }

        ty
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
        Type::Ptr(_, _) => Some(module.target_config().pointer_type()),
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

impl OutputType {
    pub fn extension(&self, target: &target_lexicon::Triple) -> &'static str {
        match self {
            OutputType::Bin => match target.operating_system {
                target_lexicon::OperatingSystem::Windows => "exe",
                target_lexicon::OperatingSystem::Wasi => "wasm",
                _ => "",
            },
            OutputType::Lib => match target.operating_system {
                target_lexicon::OperatingSystem::Windows => "lib",
                target_lexicon::OperatingSystem::Wasi => "wasm",
                _ => "a",
            },
            OutputType::DyLib => match target.operating_system {
                target_lexicon::OperatingSystem::Darwin => "dylib",
                target_lexicon::OperatingSystem::Ios => "dylib",
                target_lexicon::OperatingSystem::MacOSX { .. } => "dylib",
                target_lexicon::OperatingSystem::Windows => "dll",
                target_lexicon::OperatingSystem::Wasi => "wasm",
                _ => "so",
            },
        }
    }
}
