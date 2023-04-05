use std::collections::HashMap;
use std::ops::{Deref, DerefMut};

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::passes::{PassManager, PassManagerBuilder};
use inkwell::targets::{CodeModel, InitializationConfig, RelocMode, Target, TargetData, TargetMachine, TargetTriple};
use inkwell::{values, OptimizationLevel};
use target_lexicon::Architecture;

use crate::Db;

pub struct CodegenCtx<'a, 'ctx> {
    pub db: &'a dyn Db,
    pub target: &'a crate::target::Target,
    pub target_machine: TargetMachine,
    pub target_data: TargetData,
    pub context: &'ctx Context,
    pub module: &'a Module<'ctx>,
    pub builder: &'a Builder<'ctx>,
    pub fpm: &'a PassManager<values::FunctionValue<'ctx>>,
    data: CodegenCtxData<'ctx>,
}

#[derive(Default)]
pub struct CodegenCtxData<'ctx> {
    pub intrinsics: HashMap<&'static str, values::FunctionValue<'ctx>>,
}

pub fn with_codegen_ctx<T>(db: &dyn Db, f: impl FnOnce(CodegenCtx) -> T) -> T {
    init_backend(db);
    let target = db.target();
    let target_triple = TargetTriple::create(&target.triple.to_string());
    let host_cpu = target.cpu;
    let host_features = target.features;
    let target = Target::from_triple(&target_triple).unwrap();
    let target_machine = target
        .create_target_machine(
            &target_triple,
            host_cpu,
            host_features,
            OptimizationLevel::Default,
            RelocMode::PIC,
            CodeModel::Default,
        )
        .unwrap();

    let target_data = target_machine.get_target_data();
    let context = Context::create();
    let module = context.create_module("test");
    let builder = context.create_builder();
    let pmb = PassManagerBuilder::create();
    let fpm = PassManager::create(&module);

    pmb.set_optimization_level(OptimizationLevel::None);
    pmb.populate_function_pass_manager(&fpm);
    fpm.initialize();

    let ctx = CodegenCtx {
        db,
        target: db.target(),
        target_machine,
        target_data,
        context: &context,
        module: &module,
        builder: &builder,
        fpm: &fpm,
        data: CodegenCtxData::default(),
    };

    f(ctx)
}

fn init_backend(db: &dyn Db) {
    let initialize_config = InitializationConfig::default();
    match db.target().triple.architecture {
        | Architecture::Aarch64(_) => Target::initialize_aarch64(&initialize_config),
        | Architecture::AmdGcn => Target::initialize_amd_gpu(&initialize_config),
        | Architecture::Arm(_) => Target::initialize_arm(&initialize_config),
        | Architecture::Bpfeb => Target::initialize_bpf(&initialize_config),
        | Architecture::Bpfel => Target::initialize_bpf(&initialize_config),
        | Architecture::Hexagon => Target::initialize_hexagon(&initialize_config),
        | Architecture::Mips32(_) => Target::initialize_mips(&initialize_config),
        | Architecture::Mips64(_) => Target::initialize_mips(&initialize_config),
        | Architecture::Msp430 => Target::initialize_msp430(&initialize_config),
        | Architecture::Nvptx64 => Target::initialize_nvptx(&initialize_config),
        | Architecture::Powerpc => Target::initialize_power_pc(&initialize_config),
        | Architecture::Riscv32(_) => Target::initialize_riscv(&initialize_config),
        | Architecture::Riscv64(_) => Target::initialize_riscv(&initialize_config),
        | Architecture::Sparc => Target::initialize_sparc(&initialize_config),
        | Architecture::Wasm32 => Target::initialize_webassembly(&initialize_config),
        | Architecture::X86_32(_) => Target::initialize_x86(&initialize_config),
        | Architecture::X86_64 => Target::initialize_x86(&initialize_config),
        | _ => Target::initialize_all(&initialize_config),
    }
}

impl<'ctx> Deref for CodegenCtx<'_, 'ctx> {
    type Target = CodegenCtxData<'ctx>;

    fn deref(&self) -> &Self::Target {
        &self.data
    }
}

impl DerefMut for CodegenCtx<'_, '_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.data
    }
}
