use std::cell::RefCell;
use std::ops::{Deref, DerefMut};

use arena::{ArenaMap, Idx};
use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::{Linkage, Module};
use inkwell::passes::{PassManager, PassManagerBuilder};
use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetData, TargetMachine, TargetTriple,
};
use inkwell::{types, values, OptimizationLevel};
use mir::instance::Instance;
use mir::ir::{self, MirValueId};
use rustc_hash::FxHashMap;
use target_lexicon::Architecture;
use triomphe::Arc;

use crate::abi::FnAbi;
use crate::layout::ReprAndLayout;
use crate::local::LocalRef;
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
    pub intrinsics: FxHashMap<&'static str, values::FunctionValue<'ctx>>,
    pub funcs: FxHashMap<Instance, (values::FunctionValue<'ctx>, FnAbi<'ctx>)>,
    pub types: RefCell<FxHashMap<Arc<ReprAndLayout>, types::BasicTypeEnum<'ctx>>>,
}

pub struct BodyCtx<'a, 'b, 'ctx> {
    pub cx: &'b mut CodegenCtx<'a, 'ctx>,
    pub func: values::FunctionValue<'ctx>,
    pub fn_abi: FnAbi<'ctx>,
    pub instance: Instance,
    pub body: ir::Body,
    pub blocks: ArenaMap<Idx<ir::BlockData>, BasicBlock<'ctx>>,
    pub locals: ArenaMap<Idx<ir::LocalData>, LocalRef<'ctx>>,
}

impl<'ctx> CodegenCtx<'_, 'ctx> {
    pub fn write(&mut self, file: &tempfile::TempPath) {
        tracing::debug!("{}", self.module.to_string());

        self.target_machine
            .write_to_file(&self.module, FileType::Object, file.as_ref())
            .unwrap();
    }

    pub fn codegen(&mut self, module: hir::Module) {
        self.codegen_module(module);

        let mpm = PassManager::create(());

        mpm.add_always_inliner_pass();
        mpm.run_on(self.module);
    }

    pub fn codegen_module(&mut self, module: hir::Module) {
        for item in module.items(self.db) {
            match item {
                | hir::Item::Value(id) => self.codegen_value(id),
                | _ => {},
            }
        }
    }

    fn codegen_value(&mut self, value: hir::Value) {
        tracing::debug!("codegen_value({})", value.name(self.db).display(self.db));

        if value.is_intrinsic(self.db) {
            return;
        }

        if !value.type_vars(self.db).is_empty() {
            return;
        }

        if !value.has_body(self.db) {
            return;
        }

        let instance = Instance::new(self.db, MirValueId::ValueId(value.id()).into(), None);

        if instance.is_func(self.db) {
            let (func, fn_abi) = self.declare_func(instance);
            let body = mir::lower::value_mir(self.db, value.id()).body(self.db).unwrap();
            let mut bcx = BodyCtx {
                body,
                func,
                fn_abi,
                instance,
                blocks: ArenaMap::default(),
                locals: ArenaMap::default(),
                cx: self,
            };
        } else {
            todo!("statics");
        }
    }

    fn declare_func(&mut self, instance: Instance) -> (values::FunctionValue<'ctx>, FnAbi<'ctx>) {
        if let Some(value) = self.funcs.get(&instance) {
            return value.clone();
        }

        let linkage = if instance.is_foreign(self.db) || instance.is_exported(self.db) {
            Linkage::External
        } else {
            Linkage::Internal
        };

        let name = instance.link_name(self.db);
        let repr = instance.repr(self.db);
        let signature = match &*repr {
            | mir::repr::Repr::Func(s, _) => s,
            | _ => unreachable!(),
        };

        let abi = self.compute_fn_abi(signature);
        let ty = self.fn_type_for_abi(&abi);
        let value = self.module.add_function(&name, ty, Some(linkage));

        self.funcs.insert(instance, (value, abi.clone()));
        (value, abi)
    }
}

pub fn with_codegen_ctx<T>(db: &dyn Db, module_name: &str, f: impl FnOnce(CodegenCtx) -> T) -> T {
    init_backend(db);
    let target = db.target();
    let target_triple = TargetTriple::create(&target.triple.to_string());
    let cpu = target.cpu;
    let features = target.features;
    let target = Target::from_triple(&target_triple).unwrap();
    let target_machine = target
        .create_target_machine(
            &target_triple,
            cpu,
            features,
            OptimizationLevel::Default,
            RelocMode::PIC,
            CodeModel::Default,
        )
        .unwrap();

    let target_data = target_machine.get_target_data();
    let context = Context::create();
    let module = context.create_module(module_name);
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

impl<'a, 'ctx> Deref for BodyCtx<'a, '_, 'ctx> {
    type Target = CodegenCtx<'a, 'ctx>;

    fn deref(&self) -> &Self::Target {
        self.cx
    }
}

impl DerefMut for BodyCtx<'_, '_, '_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.cx
    }
}
