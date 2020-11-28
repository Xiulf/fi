pub mod assembly;
pub mod place;
mod trans;
pub mod type_;

use by_address::ByAddress;
use inkwell::targets::{
    CodeModel, InitializationConfig, RelocMode, Target, TargetMachine, TargetTriple,
};
use mir::ir as mir;
use std::collections::HashMap;
use std::sync::Arc;

pub mod ir {
    pub use inkwell::basic_block::BasicBlock;
    pub use inkwell::types::*;
    pub use inkwell::values::*;
}

#[salsa::query_group(CodegenDatabaseStorage)]
pub trait CodegenDatabase: ::mir::MirDatabase {
    #[salsa::input]
    fn optimization_level(&self, lib: source::LibId) -> inkwell::OptimizationLevel;

    fn target_machine(&self, lib: source::LibId) -> ByAddress<Arc<TargetMachine>>;

    #[salsa::invoke(assembly::build_assembly)]
    fn assembly(&self, lib: source::LibId, module: mir::ModuleId) -> Arc<assembly::Assembly>;
}

fn target_machine(db: &dyn CodegenDatabase, lib: source::LibId) -> ByAddress<Arc<TargetMachine>> {
    let triple = db.target(lib);

    Target::initialize_x86(&InitializationConfig::default());

    let target_triple = TargetTriple::create(&triple.to_string());
    let llvm_target = Target::from_triple(&target_triple).expect("could not find llvm target");
    let target_machine = llvm_target
        .create_target_machine(
            &target_triple,
            "",
            "",
            db.optimization_level(lib),
            RelocMode::PIC,
            CodeModel::Default,
        )
        .expect("could not create llvm target machine");

    ByAddress(Arc::new(target_machine))
}

pub struct ModuleCtx<'db, 'ctx> {
    pub db: &'db dyn CodegenDatabase,
    pub ctx: &'ctx inkwell::context::Context,
    pub module: inkwell::module::Module<'ctx>,
    pub lib: source::LibId,
    pub mir: Arc<mir::Module>,
}

pub struct FunctionCtx<'db, 'ctx, 'mcx> {
    pub mcx: &'mcx ModuleCtx<'db, 'ctx>,
    pub bcx: inkwell::builder::Builder<'ctx>,
    pub body: &'db mir::Body,
    pub blocks: HashMap<mir::Block, ir::BasicBlock<'ctx>>,
    pub locals: HashMap<mir::Local, ir::AnyValueEnum<'ctx>>,
}

impl<'db, 'ctx, 'mcx> std::ops::Deref for FunctionCtx<'db, 'ctx, 'mcx> {
    type Target = ModuleCtx<'db, 'ctx>;

    fn deref(&self) -> &Self::Target {
        self.mcx
    }
}
