pub mod assembly;
pub mod db;
mod lower;
mod place;
mod ptr;
mod value;

use hir::arena::ArenaMap;
use mir::ir;
use std::sync::Arc;

mod clif {
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
    anon_count: usize,
}

struct FunctionCtx<'a, 'mcx> {
    mcx: &'mcx mut ModuleCtx<'a>,
    bcx: clif::FunctionBuilder<'a>,
    body: Arc<ir::Body>,
    blocks: ArenaMap<ir::BlockId, clif::Block>,
    locals: ArenaMap<ir::LocalId, place::Place>,
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
            anon_count: 0,
        };

        f(mcx)
    }

    pub fn function<'mcx>(&'mcx mut self, def: hir::id::DefWithBodyId) -> FunctionCtx<'a, 'mcx> {
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
