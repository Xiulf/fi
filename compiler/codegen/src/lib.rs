mod abi;
pub mod assembly;
pub mod db;
mod intrinsic;
pub mod linker;
mod lower;
mod place;
mod ptr;
mod ssa;
mod value;

use clif::InstBuilder as _;
use clif::Module as _;
use hir::arena::ArenaMap;
use hir::display::HirDisplay;
use hir::AsName as _;
use mir::ir;
use place::PlaceRef;
use ptr::Pointer;
use rustc_hash::FxHashMap;
use std::sync::Arc;
use value::ValueRef;

mod clif {
    pub use cranelift::codegen::binemit::{NullRelocSink, NullStackMapSink, NullTrapSink};
    pub use cranelift::codegen::ir;
    pub use cranelift::codegen::ir::LibCall;
    pub use cranelift::codegen::ir::{Heap, HeapData, HeapStyle};
    pub use cranelift::codegen::Context;
    pub use cranelift::frontend::*;
    pub use cranelift::prelude::*;
    pub use cranelift_module::{default_libcall_names, DataContext, DataId, FuncId, Linkage, Module};
}

struct ModuleCtx<'a> {
    db: &'a dyn db::CodegenDatabase,
    triple: Arc<target_lexicon::Triple>,
    module: cranelift_object::ObjectModule,
    ctx: &'a mut clif::Context,
    fcx: &'a mut clif::FunctionBuilderContext,
    func_ids: FxHashMap<ir::BodyId, (clif::FuncId, clif::Signature)>,
    static_ids: FxHashMap<hir::Static, clif::DataId>,
}

struct FunctionCtx<'a, 'mcx> {
    mcx: &'mcx mut ModuleCtx<'a>,
    bcx: clif::FunctionBuilder<'a>,
    body: &'mcx ir::Body,
    blocks: ArenaMap<ir::BlockId, clif::Block>,
    locals: ArenaMap<ir::LocalId, PlaceRef>,
    ssa_vars: u32,
}

impl<'a> ModuleCtx<'a> {
    pub fn with_mcx<T>(db: &dyn db::CodegenDatabase, f: impl FnOnce(ModuleCtx) -> T) -> T {
        let mut ctx = clif::Context::new();
        let mut fcx = clif::FunctionBuilderContext::new();
        let triple = db.target_triple();
        let flags_builder = clif::settings::builder();
        let flags = clif::settings::Flags::new(flags_builder);
        let isa = clif::isa::lookup((*triple).clone()).unwrap().finish(flags);
        let builder = cranelift_object::ObjectBuilder::new(isa, "test", clif::default_libcall_names()).unwrap();
        let module = cranelift_object::ObjectModule::new(builder);
        let mcx = ModuleCtx {
            db,
            module,
            triple: db.target_triple(),
            ctx: &mut ctx,
            fcx: &mut fcx,
            func_ids: FxHashMap::default(),
            static_ids: FxHashMap::default(),
        };

        f(mcx)
    }

    pub fn build(mut self, lib: hir::Lib) -> cranelift_object::ObjectProduct {
        for module in lib.modules(self.db.upcast()) {
            if module.is_virtual(self.db.upcast()) {
                continue;
            }

            for def in module.declarations(self.db.upcast()) {
                match def {
                    | hir::ModuleDef::Func(f) => self.register_func(f),
                    | hir::ModuleDef::Static(s) => self.register_static(s),
                    | _ => {},
                }
            }

            for inst in module.instances(self.db.upcast()) {
                for def in inst.items(self.db.upcast()) {
                    match def {
                        | hir::AssocItem::Func(f) => self.register_func(f),
                        | hir::AssocItem::Static(s) => self.register_static(s),
                    }
                }
            }

            for def in module.declarations(self.db.upcast()) {
                match def {
                    | hir::ModuleDef::Func(f) => self.lower_func(f),
                    | hir::ModuleDef::Static(s) => self.lower_static(s),
                    | _ => {},
                }
            }

            for inst in module.instances(self.db.upcast()) {
                for def in inst.items(self.db.upcast()) {
                    match def {
                        | hir::AssocItem::Func(f) => self.lower_func(f),
                        | hir::AssocItem::Static(s) => self.lower_static(s),
                    }
                }
            }
        }

        if let base_db::libs::LibKind::Executable = self.db.libs()[lib.into()].kind {
            self.generate_main(lib);
        }

        self.module.finish()
    }

    pub fn register_func(&mut self, func: hir::Func) {
        if let Some(it) = func.as_assoc_item(self.db.upcast()) {
            if let hir::AssocItemContainer::Class(_) = it.container(self.db.upcast()) {
                return;
            }
        } else if func.is_foreign(self.db.upcast()) {
            let attrs = self.db.attrs(hir::id::AttrDefId::FuncId(func.into()));

            if attrs.by_key("intrinsic").exists() {
                return;
            }
        }

        let def: hir::id::FuncId = func.into();
        let def: hir::id::DefWithBodyId = def.into();
        let bodies = self.db.body_mir(def);
        let linkage = if func.is_foreign(self.db.upcast()) {
            clif::Linkage::Import
        } else if func.is_exported(self.db.upcast()) {
            clif::Linkage::Export
        } else {
            clif::Linkage::Local
        };

        let name = func.link_name(self.db.upcast()).to_string();
        let mut first = true;

        for body in bodies.ids(def) {
            let name = if first {
                first = false;
                name.clone()
            } else {
                let local_id: u32 = body.local_id.into_raw().into();

                format!("{}^{}", name, local_id)
            };

            let sig = self.func_signature(body);
            // eprintln!("{}: {}", name, sig);
            let id = self.module.declare_function(&name, linkage, &sig).unwrap();

            self.func_ids.insert(body, (id, sig));
        }
    }

    pub fn register_static(&mut self, static_: hir::Static) {
        if let Some(it) = static_.as_assoc_item(self.db.upcast()) {
            if let hir::AssocItemContainer::Class(_) = it.container(self.db.upcast()) {
                return;
            }
        }

        let is_foreign = static_.is_foreign(self.db.upcast());
        let linkage = if is_foreign {
            clif::Linkage::Import
        } else if static_.is_exported(self.db.upcast()) {
            clif::Linkage::Export
        } else {
            clif::Linkage::Local
        };

        let name = static_.link_name(self.db.upcast()).to_string();
        let id = self.module.declare_data(&name, linkage, false, false).unwrap();

        self.static_ids.insert(static_, id);
    }

    pub fn lower_func(&mut self, func: hir::Func) {
        if func.is_foreign(self.db.upcast()) {
            return;
        }

        let def: hir::id::FuncId = func.into();
        let def: hir::id::DefWithBodyId = def.into();
        let bodies = self.db.body_mir(def);

        for id in bodies.ids(def) {
            if let Some((func, sig)) = self.func_ids.get(&id).cloned() {
                self.lower_body(def, &bodies[id.local_id], func, sig);
            }
        }
    }

    pub fn lower_body(&mut self, def: hir::id::DefWithBodyId, body: &ir::Body, id: clif::FuncId, sig: clif::Signature) {
        let mut fx = self.function(def, body);
        let start_block = fx.bcx.create_block();

        fx.bcx.func.signature = sig;
        fx.bcx.switch_to_block(start_block);

        let ptr_type = fx.module.target_config().pointer_type();
        let ret = fx.body.ret.unwrap();
        let ret_lyt = fx.db.layout_of(fx.body.locals[ret].ty.clone());
        let ssa = ssa::analyze(&fx);

        match fx.pass_mode(&ret_lyt) {
            | abi::PassMode::NoPass => {
                fx.locals.insert(ret, PlaceRef::no_place(ret_lyt));
            },
            | abi::PassMode::ByVal(_) | abi::PassMode::ByValPair(_, _) => {
                local_place(&mut fx, &ssa, ret, ret_lyt);
            },
            | abi::PassMode::ByRef { size: _ } => {
                let val = fx.bcx.append_block_param(start_block, ptr_type);

                fx.locals.insert(ret, PlaceRef::new_ref(Pointer::addr(val), ret_lyt));
            },
        }

        let vals = fx
            .body
            .args()
            .into_iter()
            .map(|arg| {
                let arg = &fx.body.locals[arg];
                let layout = fx.db.layout_of(arg.ty.clone());
                let value = match fx.mcx.pass_mode(&layout) {
                    | abi::PassMode::NoPass => return None,
                    | abi::PassMode::ByVal(ty) => {
                        let param = fx.bcx.append_block_param(start_block, ty);

                        ValueRef::new_val(param, layout)
                    },
                    | abi::PassMode::ByValPair(a, b) => {
                        let a = fx.bcx.append_block_param(start_block, a);
                        let b = fx.bcx.append_block_param(start_block, b);

                        ValueRef::new_val_pair(a, b, layout)
                    },
                    | abi::PassMode::ByRef { size: Some(_) } => {
                        let param = fx.bcx.append_block_param(start_block, ptr_type);

                        ValueRef::new_ref(Pointer::addr(param), layout)
                    },
                    | abi::PassMode::ByRef { size: None } => {
                        let ptr = fx.bcx.append_block_param(start_block, ptr_type);
                        let meta = fx.bcx.append_block_param(start_block, ptr_type);

                        ValueRef::new_ref_meta(Pointer::addr(ptr), meta, layout)
                    },
                };

                Some(value)
            })
            .collect::<Vec<_>>();

        for (arg, value) in fx.body.args().into_iter().zip(vals) {
            let place = if let Some(value) = value {
                let layout = value.layout.clone();

                if let ssa::SsaKind::Ssa = ssa[arg] {
                    let place = if let mir::layout::Abi::ScalarPair(_, _) = layout.abi {
                        PlaceRef::new_var_pair(&mut fx, layout)
                    } else {
                        PlaceRef::new_var(&mut fx, layout)
                    };

                    place.clone().store(&mut fx, value);
                    place
                } else {
                    PlaceRef::new_ref(value.on_stack(&mut fx).0, layout)
                }
            } else {
                PlaceRef::no_place(fx.db.layout_of(fx.body.locals[arg].ty.clone()))
            };

            fx.locals.insert(arg, place);
        }

        for (id, local) in fx.body.locals.clone().iter() {
            if let ir::LocalKind::Var = local.kind {
                let layout = fx.db.layout_of(local.ty.clone());

                local_place(&mut fx, &ssa, id, layout);
            }
        }

        for (id, _) in fx.body.blocks.iter() {
            let block = fx.bcx.create_block();

            fx.blocks.insert(id, block);
        }

        fx.bcx.ins().jump(fx.blocks[fx.body.entry.unwrap()], &[]);
        fx.bcx.seal_block(start_block);
        fx.lower();
        fx.bcx.finalize();
        fx.ctx.compute_cfg();
        fx.ctx.compute_domtree();

        // let name = &fx.module.declarations().get_function_decl(id).name;
        // eprintln!("{}: {}", name, fx.ctx.func);

        fx.mcx.ctx.eliminate_unreachable_code(fx.mcx.module.isa()).unwrap();
        fx.mcx
            .module
            .define_function(
                id,
                &mut fx.mcx.ctx,
                &mut clif::NullTrapSink {},
                &mut clif::NullStackMapSink {},
            )
            .unwrap();

        fx.ctx.clear();

        fn local_place(
            fx: &mut FunctionCtx,
            ssa: &ArenaMap<ir::LocalId, ssa::SsaKind>,
            local: ir::LocalId,
            layout: Arc<mir::layout::Layout>,
        ) -> PlaceRef {
            let place = if let ssa::SsaKind::Ssa = ssa[local] {
                if let mir::layout::Abi::ScalarPair(_, _) = layout.abi {
                    PlaceRef::new_var_pair(fx, layout)
                } else {
                    PlaceRef::new_var(fx, layout)
                }
            } else {
                PlaceRef::new_stack(fx, layout)
            };

            fx.locals.insert(local, place.clone());
            place
        }
    }

    pub fn lower_static(&mut self, static_: hir::Static) {
        if static_.is_foreign(self.db.upcast()) {
            return;
        }

        if let Some(id) = self.static_ids.get(&static_) {
            unimplemented!();
        }
    }

    fn function<'mcx>(&'mcx mut self, def: hir::id::DefWithBodyId, body: &'mcx ir::Body) -> FunctionCtx<'a, 'mcx> {
        // unsafe is used here to fix lifetimes, this is indeed very unsafe :/
        let func = unsafe { &mut *(&mut self.ctx.func as *mut _) };
        let fcx = unsafe { &mut *(self.fcx as *mut _) };

        FunctionCtx {
            bcx: clif::FunctionBuilder::new(func, fcx),
            body,
            blocks: ArenaMap::default(),
            locals: ArenaMap::default(),
            ssa_vars: 0,
            mcx: self,
        }
    }

    fn generate_main(&mut self, lib: hir::Lib) {
        let main = (|| {
            for module in lib.modules(self.db.upcast()) {
                for def in module.declarations(self.db.upcast()) {
                    if let hir::ModuleDef::Func(f) = def {
                        if f.name(self.db.upcast()).to_string() == "main" {
                            return Some(f);
                        } else {
                            let attrs = self.db.attrs(hir::id::AttrDefId::FuncId(f.into()));

                            if attrs.by_key("main").exists() {
                                return Some(f);
                            }
                        }
                    }
                }
            }

            None
        })();

        let main = main.expect("executable contains no main function");
        let main_ret_ty = main.ty(self.db.upcast()).ret_type(self.db.upcast(), lib.into());
        let main: hir::id::FuncId = main.into();
        let main_def = main.into();
        let main = self.db.body_mir(main_def);
        let main = main.main_id(main_def);
        let main = self.func_ids[&main].0;
        let mut sig = self.module.make_signature();
        let ptr_type = self.module.target_config().pointer_type();

        sig.params.push(clif::AbiParam::new(ptr_type));
        sig.params.push(clif::AbiParam::new(ptr_type));
        sig.returns.push(clif::AbiParam::new(ptr_type));

        let id = self
            .module
            .declare_function("main", clif::Linkage::Export, &sig)
            .unwrap();

        let termination = self.db.lang_item(lib.into(), "termination-class".into()).unwrap();
        let termination = termination.as_class().unwrap();
        let ctnt = hir::ty::Constraint::new(termination, [main_ret_ty]);
        let solved = self.db.solve_constraint(ctnt).unwrap();
        let inst = self.db.instance_data(solved.instance);
        let method = inst.item(&"report".as_name()).unwrap();
        let method = method.as_func_id().unwrap();
        let method_bodies = self.db.body_mir(method.into());
        let method_body_id = method_bodies.main_id(method.into());
        let method_sig = self.func_signature(method_body_id);
        let method_name = hir::Func::from(method).link_name(self.db.upcast()).to_string();
        let method = self
            .module
            .declare_function(&method_name, clif::Linkage::Import, &method_sig)
            .unwrap();

        let mut bcx = clif::FunctionBuilder::new(&mut self.ctx.func, &mut self.fcx);
        let block = bcx.create_block();

        bcx.func.signature = sig;
        bcx.switch_to_block(block);
        bcx.append_block_params_for_function_params(block);

        let main = self.module.declare_func_in_func(main, &mut bcx.func);
        let main_inst = bcx.ins().call(main, &[]);
        let main_res = bcx.inst_results(main_inst).to_vec();
        let method = self.module.declare_func_in_func(method, &mut bcx.func);
        let method_inst = bcx.ins().call(method, &main_res);
        let ret = bcx.inst_results(method_inst)[0];
        let ret = match self.module.target_config().pointer_width {
            | target_lexicon::PointerWidth::U32 => ret,
            | target_lexicon::PointerWidth::U16 => bcx.ins().ireduce(clif::types::I16, ret),
            | target_lexicon::PointerWidth::U64 => bcx.ins().sextend(clif::types::I64, ret),
        };

        bcx.ins().return_(&[ret]);
        bcx.seal_block(block);
        bcx.finalize();

        self.module
            .define_function(
                id,
                &mut self.ctx,
                &mut clif::NullTrapSink {},
                &mut clif::NullStackMapSink {},
            )
            .unwrap();
    }

    pub fn ir_type(&self, layout: &mir::layout::Layout) -> Option<clif::Type> {
        if let mir::layout::Abi::Scalar(s) = &layout.abi {
            Some(self.scalar_type(s))
        } else {
            None
        }
    }

    pub fn ir_pair_type(&self, layout: &mir::layout::Layout) -> Option<(clif::Type, clif::Type)> {
        if let mir::layout::Abi::ScalarPair(a, b) = &layout.abi {
            Some((self.scalar_type(a), self.scalar_type(b)))
        } else {
            None
        }
    }

    pub fn scalar_type(&self, scalar: &mir::layout::Scalar) -> clif::Type {
        use clif::{types, Module};
        use mir::layout::{Integer, Primitive};

        match &scalar.value {
            | Primitive::Int(Integer::I8, _) => types::I8,
            | Primitive::Int(Integer::I16, _) => types::I16,
            | Primitive::Int(Integer::I32, _) => types::I32,
            | Primitive::Int(Integer::I64, _) => types::I64,
            | Primitive::Int(Integer::I128, _) => types::I128,
            | Primitive::F32 => types::F32,
            | Primitive::F64 => types::F64,
            | Primitive::Pointer => self.module.target_config().pointer_type(),
        }
    }

    pub fn func_signature(&self, id: ir::BodyId) -> clif::Signature {
        let bodies = self.db.body_mir(id.def);
        let body = &bodies[id.local_id];
        let args = body
            .args()
            .into_iter()
            .map(|a| self.db.layout_of(body.locals[a].ty.clone()))
            .collect::<Vec<_>>();

        let ret = self.db.layout_of(body.locals[body.ret.unwrap()].ty.clone());

        self.mk_signature(&ret, &args)
    }

    pub fn mk_signature(&self, ret: &mir::layout::Layout, args: &[Arc<mir::layout::Layout>]) -> clif::Signature {
        let mut sig = self.module.make_signature();
        let ptr_type = self.module.target_config().pointer_type();

        match self.pass_mode(ret) {
            | abi::PassMode::NoPass => {},
            | abi::PassMode::ByVal(t) => sig.returns.push(clif::AbiParam::new(t)),
            | abi::PassMode::ByValPair(a, b) => {
                sig.returns.push(clif::AbiParam::new(a));
                sig.returns.push(clif::AbiParam::new(b));
            },
            | abi::PassMode::ByRef { size: _ } => sig.params.push(clif::AbiParam::new(ptr_type)),
        }

        for arg in args {
            match self.pass_mode(&arg) {
                | abi::PassMode::NoPass => {},
                | abi::PassMode::ByVal(t) => sig.params.push(clif::AbiParam::new(t)),
                | abi::PassMode::ByValPair(a, b) => {
                    sig.params.push(clif::AbiParam::new(a));
                    sig.params.push(clif::AbiParam::new(b));
                },
                | abi::PassMode::ByRef { size: _ } => sig.params.push(clif::AbiParam::new(ptr_type)),
            }
        }

        sig
    }

    pub fn call_malloc(&mut self, bcx: &mut clif::FunctionBuilder, size: clif::Value) -> clif::Value {
        let ptr_type = self.module.target_config().pointer_type();
        let mut sig = self.module.make_signature();

        sig.params.push(clif::AbiParam::new(ptr_type));
        sig.returns.push(clif::AbiParam::new(ptr_type));

        let libc_malloc = self
            .module
            .declare_function("malloc", clif::Linkage::Import, &sig)
            .unwrap();

        let libc_malloc = self.module.declare_func_in_func(libc_malloc, &mut self.ctx.func);
        let inst = bcx.ins().call(libc_malloc, &[size]);

        bcx.inst_results(inst)[0]
    }

    pub fn call_free(&mut self, bcx: &mut clif::FunctionBuilder, ptr: clif::Value) {
        let ptr_type = self.module.target_config().pointer_type();
        let mut sig = self.module.make_signature();

        sig.params.push(clif::AbiParam::new(ptr_type));

        let libc_free = self
            .module
            .declare_function("free", clif::Linkage::Import, &sig)
            .unwrap();

        let libc_free = self.module.declare_func_in_func(libc_free, &mut self.ctx.func);

        bcx.ins().call(libc_free, &[ptr]);
    }
}

impl<'a, 'mcx> FunctionCtx<'a, 'mcx> {
    pub fn next_ssa_var(&mut self) -> u32 {
        self.ssa_vars += 1;
        self.ssa_vars
    }

    pub fn call_malloc(&mut self, size: clif::Value) -> clif::Value {
        self.mcx.call_malloc(&mut self.bcx, size)
    }

    pub fn call_free(&mut self, ptr: clif::Value) {
        self.mcx.call_free(&mut self.bcx, ptr)
    }
}

impl<'a, 'mcx> std::ops::Deref for FunctionCtx<'a, 'mcx> {
    type Target = ModuleCtx<'a>;

    fn deref(&self) -> &Self::Target {
        self.mcx
    }
}

impl<'a, 'mcx> std::ops::DerefMut for FunctionCtx<'a, 'mcx> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.mcx
    }
}
