use std::cell::Cell;
use std::sync::Arc;

use arena::{ArenaMap, Idx};
use codegen::db::CodegenDatabase;
use codegen::CompilerTarget;
use hir::attrs::HasAttrs;
use hir::id::{AssocItemId, DefWithBodyId};
use hir::AsName;
use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::{Linkage, Module};
use inkwell::passes::PassManager;
use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetData, TargetMachine, TargetTriple,
};
use inkwell::values::BasicValue;
use inkwell::{values, OptimizationLevel};
use mir::repr::{Integer, Primitive, Repr, Scalar};
use mir::syntax::{BlockData, LocalData};
use rustc_hash::FxHashMap;

use crate::abi::FnAbi;
use crate::local::LocalRef;
use crate::place::PlaceRef;

pub struct CodegenCtx<'a, 'ctx> {
    pub db: &'a dyn CodegenDatabase,
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

pub fn with_codegen_ctx<T>(db: &dyn CodegenDatabase, hir: hir::Module, f: impl FnOnce(CodegenCtx) -> T) -> T {
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
                self.codegen_main_shim(func, &abi, value);
            }
        }
    }

    pub fn codegen_main_shim(
        &mut self,
        main_func: hir::Func,
        main_abi: &FnAbi<'ctx>,
        main_value: values::FunctionValue<'ctx>,
    ) {
        let int_type = self.context.i32_type();
        let main_type = int_type.fn_type(&[int_type.into(), int_type.into()], false);
        let value = self.module.add_function("main", main_type, None);
        let entry = self.context.append_basic_block(value, "entry");
        let infer = self.db.infer(DefWithBodyId::FuncId(main_func.into()));
        let body = self.db.body(DefWithBodyId::FuncId(main_func.into()));
        let mut methods = infer.methods[&(body.body_expr(), 0)].iter().copied();
        let member = match methods.next().unwrap() {
            | hir::MethodSource::Member(id) => id,
            | hir::MethodSource::Record(..) => unreachable!(),
        };

        let report = match self.db.member_data(member).item(&"report".as_name()).unwrap() {
            | AssocItemId::FuncId(f) => f,
            | _ => unreachable!(),
        };

        let (report, report_abi) = self.declare_func(report.into());

        self.builder.position_at_end(entry);

        // TODO: correctly pass/return the main value
        let main_ret = PlaceRef::new_alloca(self, main_abi.ret.layout.clone());
        let report_ret = PlaceRef::new_alloca(self, report_abi.ret.layout.clone());

        self.builder
            .build_call(main_value, &[main_ret.ptr.as_basic_value_enum().into()], "");

        self.builder.build_call(
            report,
            &[
                report_ret.ptr.as_basic_value_enum().into(),
                main_ret.ptr.as_basic_value_enum().into(),
            ],
            "",
        );

        let exit_code = report_ret.field(self, 0).load_operand(self).load(self);

        self.builder.build_return(Some(&exit_code));
        value.verify(true);
    }
}
