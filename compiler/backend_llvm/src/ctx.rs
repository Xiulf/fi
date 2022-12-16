use std::cell::Cell;
use std::sync::Arc;

use arena::{ArenaMap, Idx};
use base_db::target::CompilerTarget;
use hir::attrs::HasAttrs;
use hir::id::DefWithBodyId;
use inkwell::attributes::{Attribute, AttributeLoc};
use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::{Linkage, Module};
use inkwell::passes::PassManager;
use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetData, TargetMachine, TargetTriple,
};
use inkwell::{values, OptimizationLevel};
use mir::db::MirDatabase;
use mir::repr::{Repr, Signature};
use mir::syntax::{BlockData, LocalData};
use rustc_hash::FxHashMap;

use crate::abi::FnAbi;
use crate::local::LocalRef;
use crate::place::PlaceRef;

pub struct CodegenCtx<'a, 'ctx> {
    pub db: &'a dyn MirDatabase,
    pub context: &'ctx Context,
    pub module: &'a Module<'ctx>,
    pub builder: &'a Builder<'ctx>,
    pub target_machine: TargetMachine,
    pub target_data: TargetData,
    pub fpm: &'a PassManager<values::FunctionValue<'ctx>>,
    pub hir: hir::Module,
    pub funcs: FxHashMap<hir::Func, (values::FunctionValue<'ctx>, FnAbi<'ctx>)>,
    pub consts: Cell<usize>,
}

pub struct BodyCtx<'a, 'b, 'ctx> {
    pub cx: &'a mut CodegenCtx<'b, 'ctx>,
    pub body: Arc<mir::syntax::BodyData>,
    pub func: values::FunctionValue<'ctx>,
    pub fn_abi: &'a FnAbi<'ctx>,
    pub ret_ptr: Option<PlaceRef<'ctx>>,
    pub blocks: ArenaMap<Idx<BlockData>, BasicBlock<'ctx>>,
    pub locals: ArenaMap<Idx<LocalData>, LocalRef<'ctx>>,
}

impl<'a, 'b, 'ctx> std::ops::Deref for BodyCtx<'a, 'b, 'ctx> {
    type Target = CodegenCtx<'b, 'ctx>;

    fn deref(&self) -> &Self::Target {
        self.cx
    }
}

pub fn with_codegen_ctx<T>(db: &dyn MirDatabase, hir: hir::Module, f: impl FnOnce(CodegenCtx) -> T) -> T {
    Target::initialize_native(&InitializationConfig::default()).unwrap();

    let target_triple = match db.target() {
        | CompilerTarget::Native(triple) => triple,
        | _ => unreachable!(),
    };

    let target_triple = TargetTriple::create(&target_triple.to_string());
    let host_cpu = TargetMachine::get_host_cpu_name();
    let host_features = TargetMachine::get_host_cpu_features();
    let target = Target::from_triple(&target_triple).unwrap();
    let target_machine = target
        .create_target_machine(
            &target_triple,
            host_cpu.to_str().unwrap(),
            host_features.to_str().unwrap(),
            OptimizationLevel::Default,
            RelocMode::PIC,
            CodeModel::Default,
        )
        .unwrap();

    let target_data = target_machine.get_target_data();
    let context = Context::create();
    let module = context.create_module(hir.name(db.upcast()).as_ref());
    let builder = context.create_builder();
    let fpm = PassManager::create(&module);

    fpm.add_instruction_combining_pass();
    fpm.add_reassociate_pass();
    fpm.add_gvn_pass();
    fpm.add_cfg_simplification_pass();
    fpm.add_basic_alias_analysis_pass();
    fpm.add_promote_memory_to_register_pass();
    fpm.add_instruction_combining_pass();
    fpm.add_reassociate_pass();
    fpm.initialize();

    let ctx = CodegenCtx {
        db,
        hir,
        target_data,
        target_machine,
        module: &module,
        builder: &builder,
        context: &context,
        fpm: &fpm,
        funcs: FxHashMap::default(),
        consts: Cell::new(0),
    };

    f(ctx)
}

impl<'ctx> CodegenCtx<'_, 'ctx> {
    pub fn write(&mut self, file: &mut dyn std::io::Write) {
        self.module.print_to_stderr();

        let buffer = self
            .target_machine
            .write_to_memory_buffer(self.module, FileType::Object)
            .unwrap();

        file.write_all(buffer.as_slice()).unwrap();
    }

    pub fn codegen(&mut self) {
        for def in self.hir.declarations(self.db.upcast()) {
            match def {
                | hir::ModuleDef::Func(func) => self.codegen_func(func),
                | _ => {},
            }
        }

        for member in self.hir.members(self.db.upcast()) {
            for item in member.items(self.db.upcast()) {
                match item {
                    | hir::AssocItem::Func(func) => self.codegen_func(func),
                    | _ => {},
                }
            }
        }

        let mpm = PassManager::create(());

        mpm.add_always_inliner_pass();
        // mpm.add_function_inlining_pass();
        mpm.run_on(self.module);
    }

    pub fn declare_func(&mut self, func: hir::Func) -> (values::FunctionValue<'ctx>, FnAbi<'ctx>) {
        if let Some(value) = self.funcs.get(&func) {
            return value.clone();
        }

        let linkage = if func.is_foreign(self.db.upcast()) || func.is_exported(self.db.upcast()) {
            Linkage::External
        } else {
            Linkage::Internal
        };

        let (name, mangle) = func.link_name(self.db.upcast());
        let name = if mangle {
            mangling::mangle(name.as_ref().bytes())
        } else {
            name.to_string()
        };

        let sig = self.db.func_signature(func);
        let abi = self.compute_fn_abi(&sig);
        let ty = self.fn_type_for_abi(&abi);
        let value = self.module.add_function(&name, ty, Some(linkage));

        self.funcs.insert(func, (value, abi.clone()));

        (value, abi)
    }

    pub fn codegen_func(&mut self, func: hir::Func) {
        if func.is_intrinsic(self.db.upcast()) {
            return;
        }

        if func.has_body(self.db.upcast()) {
            let (value, abi) = self.declare_func(func);
            let body = self.db.body_mir(DefWithBodyId::FuncId(func.into()));
            let mut ctx = BodyCtx {
                body: self.db.lookup_intern_body(body),
                func: value,
                fn_abi: &abi,
                ret_ptr: None,
                blocks: ArenaMap::default(),
                locals: ArenaMap::default(),
                cx: self,
            };

            ctx.codegen();

            if func.attrs(self.db.upcast()).by_key("main").exists() {
                self.codegen_main_shim(func);
            }
        }
    }

    pub fn codegen_main_shim(&mut self, main_func: hir::Func) {
        let sig = Signature {
            params: vec![Repr::i32(), Repr::i32()].into_boxed_slice(),
            ret: Repr::i32(),
        };

        let abi = self.compute_fn_abi(&sig);
        let main_type = self.fn_type_for_abi(&abi);
        let value = self.module.add_function("main", main_type, None);
        let main_shim = self.db.mir_main_shim(main_func);
        let mut ctx = BodyCtx {
            body: self.db.lookup_intern_body(main_shim),
            func: value,
            fn_abi: &abi,
            ret_ptr: None,
            blocks: ArenaMap::default(),
            locals: ArenaMap::default(),
            cx: self,
        };

        ctx.codegen();
    }
}
