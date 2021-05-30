mod abi;
pub mod assembly;
pub mod db;
mod intrinsic;
pub mod linker;
mod lower;
mod place;
mod ptr;
mod value;

use clif::Module as _;
use hir::arena::ArenaMap;
use mir::ir;
use place::PlaceRef;
use ptr::Pointer;
use rustc_hash::FxHashMap;
use std::sync::Arc;
use value::ValueRef;

mod clif {
    pub use cranelift::codegen::binemit::{NullRelocSink, NullStackMapSink, NullTrapSink};
    pub use cranelift::codegen::ir;
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
    func_ids: FxHashMap<hir::Func, (clif::FuncId, clif::Signature)>,
    static_ids: FxHashMap<hir::Static, clif::DataId>,
}

struct FunctionCtx<'a, 'mcx> {
    mcx: &'mcx mut ModuleCtx<'a>,
    bcx: clif::FunctionBuilder<'a>,
    body: Arc<ir::Body>,
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

        self.module.finish()
    }

    pub fn register_func(&mut self, func: hir::Func) {
        if let Some(it) = func.as_assoc_item(self.db.upcast()) {
            if let hir::AssocItemContainer::Class(_) = it.container(self.db.upcast()) {
                return;
            }
        }

        let sig = self.func_signature(func);
        let linkage = if func.is_foreign(self.db.upcast()) {
            clif::Linkage::Import
        } else if func.is_exported(self.db.upcast()) {
            clif::Linkage::Export
        } else {
            clif::Linkage::Local
        };

        let name = func.link_name(self.db.upcast());
        let id = self.module.declare_function(&name, linkage, &sig).unwrap();

        self.func_ids.insert(func, (id, sig));
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

        let name = if is_foreign {
            static_.name(self.db.upcast()).to_string()
        } else {
            static_.path(self.db.upcast()).to_string()
        };

        let id = self.module.declare_data(&name, linkage, false, false).unwrap();

        self.static_ids.insert(static_, id);
    }

    pub fn lower_func(&mut self, func: hir::Func) {
        if func.is_foreign(self.db.upcast()) {
            return;
        }

        if let Some(&(id, ref sig)) = self.func_ids.get(&func) {
            use clif::InstBuilder;
            let sig = sig.clone();
            let def = hir::id::DefWithBodyId::FuncId(func.into());
            let mut fx = self.function(def);
            let start_block = fx.bcx.create_block();

            fx.bcx.func.signature = sig;
            fx.bcx.switch_to_block(start_block);

            let ptr_type = fx.module.target_config().pointer_type();
            let ret = fx.body.ret.unwrap();
            let ret_lyt = fx.body.locals[ret].layout.clone();

            match fx.pass_mode(&ret_lyt) {
                | abi::PassMode::NoPass => {
                    fx.locals.insert(ret, PlaceRef::no_place(ret_lyt));
                },
                | abi::PassMode::ByVal(_) | abi::PassMode::ByValPair(_, _) => {
                    local_place(&mut fx, ret, ret_lyt);
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
                .filter_map(|arg| {
                    let arg = &fx.body.locals[arg];
                    let value = match fx.mcx.pass_mode(&arg.layout) {
                        | abi::PassMode::NoPass => return None,
                        | abi::PassMode::ByVal(ty) => {
                            let param = fx.bcx.append_block_param(start_block, ty);

                            ValueRef::new_val(param, arg.layout.clone())
                        },
                        | abi::PassMode::ByValPair(a, b) => {
                            let a = fx.bcx.append_block_param(start_block, a);
                            let b = fx.bcx.append_block_param(start_block, b);

                            ValueRef::new_val_pair(a, b, arg.layout.clone())
                        },
                        | abi::PassMode::ByRef { size: Some(_) } => {
                            let param = fx.bcx.append_block_param(start_block, ptr_type);

                            ValueRef::new_ref(Pointer::addr(param), arg.layout.clone())
                        },
                        | abi::PassMode::ByRef { size: None } => {
                            let ptr = fx.bcx.append_block_param(start_block, ptr_type);
                            let meta = fx.bcx.append_block_param(start_block, ptr_type);

                            ValueRef::new_ref_meta(Pointer::addr(ptr), meta, arg.layout.clone())
                        },
                    };

                    Some(value)
                })
                .collect::<Vec<_>>();

            for (arg, value) in fx.body.args().into_iter().zip(vals) {
                let layout = value.layout.clone();
                let place = if fx.body.locals[arg].is_ssa {
                    let place = if let mir::layout::Abi::ScalarPair(_, _) = layout.abi {
                        PlaceRef::new_var_pair(&mut fx, layout)
                    } else {
                        PlaceRef::new_var(&mut fx, layout)
                    };

                    place.clone().store(&mut fx, value);
                    place
                } else {
                    PlaceRef::new_ref(value.on_stack(&mut fx).0, layout)
                };

                fx.locals.insert(arg, place);
            }

            for (id, local) in fx.body.locals.clone().iter() {
                if let ir::LocalKind::Var = local.kind {
                    local_place(&mut fx, id, local.layout.clone());
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
            fx.mcx.ctx.eliminate_unreachable_code(fx.mcx.module.isa()).unwrap();

            eprintln!("{}: {}", func.link_name(fx.db.upcast()), fx.ctx.func);

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

            fn local_place(fx: &mut FunctionCtx, local: ir::LocalId, layout: Arc<mir::layout::Layout>) -> PlaceRef {
                let place = if fx.body.locals[local].is_ssa {
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
    }

    pub fn lower_static(&mut self, static_: hir::Static) {
        if static_.is_foreign(self.db.upcast()) {
            return;
        }

        if let Some(id) = self.static_ids.get(&static_) {
            unimplemented!();
        }
    }

    fn function<'mcx>(&'mcx mut self, def: hir::id::DefWithBodyId) -> FunctionCtx<'a, 'mcx> {
        // unsafe is used here to fix lifetimes, this is indeed very unsafe :/
        let func = unsafe { &mut *(&mut self.ctx.func as *mut _) };
        let fcx = unsafe { &mut *(self.fcx as *mut _) };

        FunctionCtx {
            bcx: clif::FunctionBuilder::new(func, fcx),
            body: self.db.body_mir(def),
            blocks: ArenaMap::default(),
            locals: ArenaMap::default(),
            ssa_vars: 0,
            mcx: self,
        }
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

    pub fn func_signature(&self, func: hir::Func) -> clif::Signature {
        let lib = func.lib(self.db.upcast()).into();
        let func_id = self.db.lang_item(lib, "fn-type".into()).unwrap();
        let func_id = func_id.as_type_ctor().unwrap();
        let mut args = Vec::new();
        let mut ty = self.db.value_ty(hir::id::ValueTyDefId::FuncId(func.into()));

        while let hir::ty::TyKind::ForAll(var, ret) = ty.lookup(self.db.upcast()) {
            if let Some(layout) = mir::layout::type_var(self.db.upcast(), lib, var) {
                args.push(layout);
            }

            ty = ret;
        }

        while let hir::ty::TyKind::Ctnt(ctnt, ret) = ty.lookup(self.db.upcast()) {
            ty = ret;
        }

        while let Some([arg, ret]) = ty.match_ctor(self.db.upcast(), func_id) {
            args.push(self.db.layout_of(arg));
            ty = ret;
        }

        let ret = if let hir::ty::TyKind::TypeVar(_) = ty.lookup(self.db.upcast()) {
            args.insert(0, self.db.layout_of(ty));
            Arc::new(mir::layout::Layout::UNIT)
        } else {
            self.db.layout_of(ty)
        };

        self.mk_signature(&ret, args)
    }

    pub fn mk_signature(&self, ret: &mir::layout::Layout, args: Vec<Arc<mir::layout::Layout>>) -> clif::Signature {
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
}

impl<'a, 'mcx> FunctionCtx<'a, 'mcx> {
    pub fn next_ssa_var(&mut self) -> u32 {
        self.ssa_vars += 1;
        self.ssa_vars
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
