#![feature(decl_macro)]

mod alloc;
pub mod place;
pub mod ptr;
pub mod value;

use codegen::*;
use cranelift::codegen::ir;
use cranelift::prelude::InstBuilder;
use cranelift_module::Linkage;
use cranelift_module::Module;
use mir::ir as mir;
use std::collections::HashMap;
use std::marker::PhantomData;

pub fn create_backend(
    db: &dyn ::mir::MirDatabase,
    lib: source::LibId,
    mir: std::sync::Arc<mir::Module>,
) -> codegen::obj_file::ObjectFile {
    use ::mir::ir::display::MirDisplay;
    println!("{}", mir.display(db));
    let backend = ClifBackend::new();
    let mcx = codegen::ModuleCtx::new(db, lib, mir, backend);

    mcx.build()
}

pub struct ClifBackend<'ctx> {
    ptr_type: cranelift::prelude::Type,
    ssa_vars: u32,
    anon_count: usize,
    func_ids: HashMap<
        mir::DefId,
        (
            cranelift_module::FuncId,
            ir::Signature,
            layout::TyLayout<mir::Ty>,
        ),
    >,
    data_ids: HashMap<mir::DefId, (cranelift_module::DataId, layout::TyLayout<mir::Ty>)>,
    func_ctx: Option<*mut cranelift::frontend::FunctionBuilderContext>,
    _ctx: PhantomData<&'ctx cranelift::codegen::Context>,
}

impl<'ctx> Backend for ClifBackend<'ctx> {
    type Module = cranelift_object::ObjectModule;
    type Context = cranelift::codegen::Context;
    type Builder = cranelift::frontend::FunctionBuilder<'ctx>;
    type Func = cranelift_module::FuncId;
    type Static = cranelift_module::DataId;
    type Block = cranelift::prelude::Block;
    type Place = place::Place<'ctx>;
    type Value = value::Value<'ctx>;
    type Type = IrType<'ctx>;

    fn create_module(&mut self, lib: source::LibId, db: &dyn ::mir::MirDatabase) -> Self::Module {
        let triple = db.target(lib);
        let manifest = db.manifest(lib);
        let flags_builder = cranelift::codegen::settings::builder();
        let flags = cranelift::codegen::settings::Flags::new(flags_builder);
        let isa = cranelift::codegen::isa::lookup((*triple).clone())
            .unwrap()
            .finish(flags);

        let builder = cranelift_object::ObjectBuilder::new(
            isa,
            manifest.package.name.as_str(),
            cranelift_module::default_libcall_names(),
        )
        .unwrap();

        let module = cranelift_object::ObjectModule::new(builder);

        self.ptr_type = module.target_config().pointer_type();

        module
    }

    fn create_ctx(&mut self, module: &mut Self::Module) -> Self::Context {
        module.make_context()
    }

    fn create_builder(&mut self, ctx: &mut Self::Context) -> Self::Builder {
        self.func_ctx = Some(Box::leak(Box::new(
            cranelift::frontend::FunctionBuilderContext::new(),
        )));

        let func: *mut cranelift::codegen::ir::Function = &mut ctx.func;

        cranelift::frontend::FunctionBuilder::new(unsafe { &mut *func }, unsafe {
            &mut *self.func_ctx.unwrap()
        })
    }

    fn declare_static(mcx: &mut ModuleCtx<Self>, body: &mir::Body) -> Self::Static {
        let file = mcx.db.module_tree(body.def.lib).file(body.def.module);
        let module = mcx.db.module_hir(file);
        let def = module.def(body.def);
        let name = def.name();
        let name = format!("{}.{}", module.name, name);
        let mut name = mangling::mangle(name.bytes());

        if let hir::ir::Def::Item(item) = def {
            if item.is_no_mangle() {
                name = def.name().to_string();
            }
        }

        let data = mcx
            .module
            .declare_data(&name, Linkage::Export, true, false)
            .unwrap();

        data
    }

    fn declare_func(mcx: &mut ModuleCtx<Self>, body: &mir::Body) -> Self::Func {
        let file = mcx.db.module_tree(body.def.lib).file(body.def.module);
        let module = mcx.db.module_hir(file);
        let def = module.def(body.def);
        let name = def.name();
        let name = format!("{}.{}", module.name, name);
        let mut name = mangling::mangle(name.bytes());

        if let hir::ir::Def::Item(item) = def {
            if item.is_main() {
                name = "main".to_string();
            } else if item.is_no_mangle() {
                name = def.name().to_string();
            }
        }

        let mut sig = mcx.module.make_signature();
        let ret = mcx
            .db
            .layout_of(mcx.lib, body.locals[mir::Local::RET].ty.clone());
        use codegen::abi::{get_pass_mode, PassMode};
        use cranelift::prelude::AbiParam;

        match get_pass_mode(mcx, &ret) {
            PassMode::NoPass => {}
            PassMode::ByVal(ty) => sig.returns.push(AbiParam::new(ty)),
            PassMode::ByValPair(a, b) => {
                sig.returns.push(AbiParam::new(a));
                sig.returns.push(AbiParam::new(b));
            }
            PassMode::ByRef { size: _ } => {
                sig.params.push(AbiParam::new(mcx.ptr_type));
            }
        }

        for arg in body.args() {
            match get_pass_mode(mcx, &mcx.db.layout_of(mcx.lib, arg.ty.clone())) {
                PassMode::NoPass => {}
                PassMode::ByVal(ty) => sig.params.push(AbiParam::new(ty)),
                PassMode::ByValPair(a, b) => {
                    sig.params.push(AbiParam::new(a));
                    sig.params.push(AbiParam::new(b));
                }
                PassMode::ByRef { size: _ } => {
                    sig.params.push(AbiParam::new(mcx.ptr_type));
                }
            }
        }

        let func = mcx
            .module
            .declare_function(&name, Linkage::Export, &sig)
            .unwrap();

        mcx.func_ids.insert(body.def, (func, sig, ret));

        func
    }

    fn declare_foreign_static(mcx: &mut ModuleCtx<Self>, id: mir::DefId) -> Self::Static {
        let file = mcx.db.module_tree(id.lib).file(id.module);
        let module = mcx.db.module_hir(file);
        let def = module.def(id);
        let name = def.name();
        let data = mcx
            .module
            .declare_data(&**name.symbol, Linkage::Import, true, false)
            .unwrap();

        data
    }

    fn declare_foreign_func(mcx: &mut ModuleCtx<Self>, id: mir::DefId) -> Self::Func {
        let file = mcx.db.module_tree(id.lib).file(id.module);
        let module = mcx.db.module_hir(file);
        let def = module.def(id);
        let name = def.name();
        let mut sig = mcx.module.make_signature();
        let ty = mcx.db.typecheck(id).ty.clone();
        let mut vargs = None;
        let ty = if let check::ty::Type::ForAll(vargs2, ty2) = &*ty {
            vargs = Some(vargs2.clone());
            ty2.clone()
        } else {
            ty
        };

        let ret = if let check::ty::Type::Func(args, ret) = &*ty {
            use codegen::abi::{get_pass_mode, PassMode};
            use cranelift::prelude::AbiParam;
            let ret = mcx.db.layout_of(mcx.lib, ret.clone());

            match get_pass_mode(mcx, &ret) {
                PassMode::NoPass => {}
                PassMode::ByVal(ty) => sig.returns.push(AbiParam::new(ty)),
                PassMode::ByValPair(a, b) => {
                    sig.returns.push(AbiParam::new(a));
                    sig.returns.push(AbiParam::new(b));
                }
                PassMode::ByRef { size: _ } => {
                    sig.params.push(AbiParam::new(mcx.ptr_type));
                }
            }

            for arg in args {
                match get_pass_mode(mcx, &mcx.db.layout_of(mcx.lib, arg)) {
                    PassMode::NoPass => {}
                    PassMode::ByVal(ty) => sig.params.push(AbiParam::new(ty)),
                    PassMode::ByValPair(a, b) => {
                        sig.params.push(AbiParam::new(a));
                        sig.params.push(AbiParam::new(b));
                    }
                    PassMode::ByRef { size: _ } => {
                        sig.params.push(AbiParam::new(mcx.ptr_type));
                    }
                }
            }

            if let Some(vargs) = vargs {
                for _ in &vargs {
                    sig.params.push(AbiParam::new(mcx.ptr_type));
                }
            }

            ret
        } else {
            unreachable!();
        };

        let func = mcx
            .module
            .declare_function(&**name.symbol, Linkage::Import, &sig)
            .unwrap();

        mcx.func_ids.insert(id, (func, sig, ret));

        func
    }

    fn func_prologue(fx: &mut FunctionCtx<Self>) {
        let sig = fx.func_ids[&fx.body.def].1.clone();
        let start_block = fx.bcx.create_block();

        fx.bcx.func.signature = sig;
        fx.bcx.switch_to_block(start_block);

        for block in &fx.body.blocks {
            fx.blocks.insert(block.id, fx.bcx.create_block());
        }

        use codegen::abi::{get_pass_mode, PassMode};
        let ssa_map = codegen::analyze::analyze(fx);

        {
            let layout = fx
                .db
                .layout_of(fx.lib, fx.body.locals[mir::Local::RET].ty.clone());

            match get_pass_mode(fx.mcx, &layout) {
                PassMode::NoPass => {
                    fx.locals
                        .insert(mir::Local::RET, place::Place::no_place(layout));
                }
                PassMode::ByVal(_) | PassMode::ByValPair(_, _) => {
                    let ssa = ssa_map[&mir::Local::RET] == codegen::analyze::SsaKind::Ssa;

                    local_place(fx, mir::Local::RET, layout, ssa);
                }
                PassMode::ByRef { size: _ } => {
                    let ret = fx.bcx.append_block_param(start_block, fx.ptr_type);

                    fx.locals.insert(
                        mir::Local::RET,
                        place::Place::new_ref(ptr::Pointer::addr(ret), layout),
                    );
                }
            }
        }

        let vals = fx
            .body
            .args()
            .filter_map(|arg| {
                let layout = fx.db.layout_of(fx.lib, arg.ty.clone());
                let value = match get_pass_mode(fx.mcx, &layout) {
                    PassMode::NoPass => return None,
                    PassMode::ByVal(ty) => {
                        let param = fx.bcx.append_block_param(start_block, ty);

                        value::Value::new_val(param, layout.clone())
                    }
                    PassMode::ByValPair(a, b) => {
                        let a = fx.bcx.append_block_param(start_block, a);
                        let b = fx.bcx.append_block_param(start_block, b);

                        value::Value::new_val_pair(a, b, layout.clone())
                    }
                    PassMode::ByRef { size: Some(_) } => {
                        let param = fx.bcx.append_block_param(start_block, fx.ptr_type);

                        value::Value::new_ref(ptr::Pointer::addr(param), layout.clone())
                    }
                    PassMode::ByRef { size: None } => {
                        let ptr = fx.bcx.append_block_param(start_block, fx.ptr_type);
                        // let meta = fx.bcx.append_block_param(start_block, fx.ptr_type);

                        // value::Value::new_ref_meta(ptr::Pointer::addr(ptr), meta, layout.clone())
                        value::Value::new_ref(ptr::Pointer::addr(ptr), layout.clone())
                    }
                };

                Some(value)
            })
            .collect::<Vec<_>>();

        for (arg, value) in fx.body.args().zip(vals) {
            let ssa = ssa_map[&arg.id] == codegen::analyze::SsaKind::Ssa;
            let place = if ssa {
                let place = if let layout::Abi::ScalarPair(_, _) = value.layout.abi {
                    place::Place::new_var_pair(fx, value.layout.clone())
                } else {
                    place::Place::new_var(fx, value.layout.clone())
                };

                place.clone().store(fx, value);
                place
            } else {
                place::Place::new_ref(value.clone().on_stack(fx).0, value.layout)
            };

            fx.locals.insert(arg.id, place);
        }

        for local in &fx.body.locals {
            if let mir::LocalKind::Var | mir::LocalKind::Tmp = local.kind {
                let ssa = ssa_map[&local.id] == codegen::analyze::SsaKind::Ssa;
                let layout = fx.db.layout_of(fx.lib, local.ty.clone());

                local_place(fx, local.id, layout, ssa);
            }
        }

        fx.bcx
            .ins()
            .jump(fx.blocks[&fx.body.blocks.first().unwrap().id], &[]);

        fn local_place<'ctx>(
            fx: &mut FunctionCtx<ClifBackend<'ctx>>,
            local: mir::Local,
            layout: layout::TyLayout<mir::Ty>,
            ssa: bool,
        ) -> place::Place<'ctx> {
            let place = if ssa {
                if let layout::Abi::ScalarPair(_, _) = layout.abi {
                    place::Place::new_var_pair(fx, layout)
                } else {
                    place::Place::new_var(fx, layout)
                }
            } else if let check::ty::Type::Var(_) = &*layout.ty {
                let var = cranelift::frontend::Variable::with_u32(fx.next_ssa_var());

                fx.bcx.declare_var(var, fx.ptr_type);
                place::Place::new_ref(ptr::Pointer::var(var), layout)
            } else {
                place::Place::new_stack(fx, layout)
            };

            fx.locals.insert(local, place.clone());
            place
        }
    }

    fn define_func(fx: &mut FunctionCtx<Self>, func: Self::Func) {
        fx.bcx.seal_all_blocks();
        fx.bcx.finalize();
        fx.ctx.compute_cfg();
        fx.ctx.compute_domtree();

        println!("{}", fx.module.declarations().get_function_decl(func).name);
        println!("{}", fx.ctx.func);

        fx.mcx
            .ctx
            .eliminate_unreachable_code(fx.mcx.module.isa())
            .unwrap();

        fx.mcx.ctx.dce(fx.mcx.module.isa()).unwrap();

        fx.mcx
            .module
            .define_function(
                func,
                &mut fx.mcx.ctx,
                &mut cranelift::codegen::binemit::NullTrapSink {},
            )
            .unwrap();

        fx.ctx.clear();
    }

    fn finish(mcx: ModuleCtx<Self>) -> codegen::obj_file::ObjectFile {
        let mut obj_file = codegen::obj_file::ObjectFile::new();
        let product = mcx.module.finish();
        let bytes = product.emit().unwrap();

        obj_file.write(&bytes);
        obj_file
    }

    fn switch_to_block(fx: &mut FunctionCtx<Self>, block: Self::Block) {
        fx.bcx.switch_to_block(block);
    }

    fn var_live(fx: &mut FunctionCtx<Self>, local: mir::Local) {
        if let check::ty::Type::Var(var) = &*fx.body.locals[local].ty {
            let __alloc = fx.db.lang_items().__alloc().owner;
            let __alloc = fx.func_ids[&__alloc].0;
            let __alloc = fx
                .mcx
                .module
                .declare_func_in_func(__alloc, &mut fx.mcx.ctx.func);

            let var_info = fx.body.tvar_local(fx.db, *var).unwrap();
            let var_info = fx.locals[&var_info].clone().deref(fx).field(fx, 0);
            let size = var_info.field(fx, 0).to_value(fx).load_scalar(fx);
            let call = fx.bcx.ins().call(__alloc, &[size]);
            let val = fx.bcx.inst_results(call)[0];
            let place = fx.locals[&local].clone();
            let val = value::Value::new_val(val, place.layout.clone());

            place.store(fx, val);
        }
    }

    fn var_dead(fx: &mut FunctionCtx<Self>, local: mir::Local) {
        if let check::ty::Type::Var(_) = &*fx.body.locals[local].ty {
            let __free = fx.db.lang_items().__free().owner;
            let __free = fx.func_ids[&__free].0;
            let __free = fx
                .mcx
                .module
                .declare_func_in_func(__free, &mut fx.mcx.ctx.func);

            let ptr = fx.locals[&local]
                .as_ptr()
                .load(fx, fx.ptr_type, ir::MemFlags::new());

            fx.bcx.ins().call(__free, &[ptr]);
        }
    }

    fn trans_copy(
        fx: &mut FunctionCtx<Self>,
        place: Self::Place,
        into: Option<Self::Place>,
    ) -> Self::Value {
        if let check::ty::Type::Var(var) = &*place.layout.ty {
            let into = if let Some(into) = into {
                into
            } else {
                let var = cranelift::frontend::Variable::with_u32(fx.next_ssa_var());

                fx.bcx.declare_var(var, fx.ptr_type);
                place::Place::new_ref(ptr::Pointer::var(var), place.layout.clone())
            };

            let var_info = fx.body.tvar_local(fx.db, *var).unwrap();
            let var_info = fx.locals[&var_info].clone();
            let copy = var_info
                .clone()
                .deref(fx)
                .field(fx, 0)
                .field(fx, 3)
                .deref(fx)
                .field(fx, 0)
                .field(fx, 0)
                .to_value(fx)
                .load_scalar(fx);

            let mut sig = fx.module.make_signature();

            sig.params.push(ir::AbiParam::new(fx.ptr_type));
            sig.params.push(ir::AbiParam::new(fx.ptr_type));
            sig.params.push(ir::AbiParam::new(fx.ptr_type));

            let sig = fx.bcx.import_signature(sig);
            let arg0 = into.as_ptr().get_addr(fx);
            let arg1 = place.as_ptr().get_addr(fx);
            let arg2 = var_info.to_value(fx).load_scalar(fx);

            fx.bcx.ins().call_indirect(sig, copy, &[arg0, arg1, arg2]);

            into.to_value(fx)
        } else {
            if let Some(into) = into {
                let val = place.to_value(fx);

                into.clone().store(fx, val);
                into.to_value(fx)
            } else {
                place.to_value(fx)
            }
        }
    }

    fn trans_move(
        fx: &mut FunctionCtx<Self>,
        place: Self::Place,
        into: Option<Self::Place>,
    ) -> Self::Value {
        if let check::ty::Type::Var(var) = &*place.layout.ty {
            let into = if let Some(into) = into {
                into
            } else {
                let var = cranelift::frontend::Variable::with_u32(fx.next_ssa_var());

                fx.bcx.declare_var(var, fx.ptr_type);
                place::Place::new_ref(ptr::Pointer::var(var), place.layout.clone())
            };

            let var_info = fx.body.tvar_local(fx.db, *var).unwrap();
            let var_info = fx.locals[&var_info].clone();
            let move_ = var_info
                .clone()
                .deref(fx)
                .field(fx, 0)
                .field(fx, 3)
                .deref(fx)
                .field(fx, 0)
                .field(fx, 1)
                .to_value(fx)
                .load_scalar(fx);

            let mut sig = fx.module.make_signature();

            sig.params.push(ir::AbiParam::new(fx.ptr_type));
            sig.params.push(ir::AbiParam::new(fx.ptr_type));
            sig.params.push(ir::AbiParam::new(fx.ptr_type));

            let sig = fx.bcx.import_signature(sig);
            let arg0 = into.as_ptr().get_addr(fx);
            let arg1 = place.as_ptr().get_addr(fx);
            let arg2 = var_info.to_value(fx).load_scalar(fx);

            fx.bcx.ins().call_indirect(sig, move_, &[arg0, arg1, arg2]);

            into.to_value(fx)
        } else {
            if let Some(into) = into {
                let val = place.to_value(fx);

                into.clone().store(fx, val);
                into.to_value(fx)
            } else {
                place.to_value(fx)
            }
        }
    }

    fn trans_place(fx: &mut FunctionCtx<Self>, place: &mir::Place) -> Self::Place {
        let mut res = match &place.base {
            mir::PlaceBase::Local(id) => fx.locals[id].clone(),
            mir::PlaceBase::Static(id) => {
                let (data_id, layout) = fx.data_ids[id].clone();
                let local_data_id = fx
                    .mcx
                    .module
                    .declare_data_in_func(data_id, &mut fx.bcx.func);

                let ptr_type = fx.ptr_type;
                let global_ptr = fx.bcx.ins().global_value(ptr_type, local_data_id);

                place::Place::new_ref(ptr::Pointer::addr(global_ptr), layout)
            }
        };

        for elem in &place.elems {
            match elem {
                mir::PlaceElem::Deref => res = res.deref(fx),
                mir::PlaceElem::Field(idx) => res = res.field(fx, *idx),
                mir::PlaceElem::Index(idx) => {
                    let idx = Self::trans_op(fx, idx, None);

                    res = res.index(fx, idx);
                }
                mir::PlaceElem::Downcast(idx) => res = res.downcast_variant(fx, *idx),
            }
        }

        res
    }

    fn trans_const(
        fx: &mut FunctionCtx<Self>,
        const_: &mir::Const,
        ty: &mir::Ty,
        into: Option<Self::Place>,
    ) -> Self::Value {
        let layout = fx.db.layout_of(fx.lib, ty.clone());

        if let Some(into) = into {
            match const_ {
                mir::Const::Undefined => into.to_value(fx),
                mir::Const::Scalar(val) => {
                    let val = value::Value::new_const(*val, fx, layout);

                    into.store(fx, val.clone());
                    val
                }
                mir::Const::Tuple(vals) if vals.is_empty() => value::Value::new_unit(layout),
                _ => unimplemented!(),
            }
        } else {
            match const_ {
                mir::Const::Undefined => {
                    let slot = fx.bcx.create_stack_slot(ir::StackSlotData::new(
                        ir::StackSlotKind::ExplicitSlot,
                        layout.size.bytes() as u32,
                    ));

                    value::Value::new_ref(ptr::Pointer::stack(slot), layout)
                }
                mir::Const::Scalar(val) => value::Value::new_const(*val, fx, layout),
                mir::Const::Tuple(vals) if vals.is_empty() => value::Value::new_unit(layout),
                mir::Const::FuncAddr(id) => {
                    let ptr_type = fx.ptr_type;
                    let func = fx.func_ids[id].0;
                    let func = fx.mcx.module.declare_func_in_func(func, &mut fx.bcx.func);
                    let func = fx.bcx.ins().func_addr(ptr_type, func);

                    value::Value::new_val(func, layout)
                }
                mir::Const::Bytes(bytes) => Self::trans_bytes(fx, bytes, layout),
                mir::Const::Ref(const_) => {
                    let ptr_type = fx.ptr_type;
                    let data_id = Self::trans_const_alloc(
                        fx,
                        const_,
                        layout.pointee(fx.lib, fx.db.to_layout_db()),
                    );

                    let global = fx
                        .mcx
                        .module
                        .declare_data_in_func(data_id, &mut fx.mcx.ctx.func);

                    let global = fx.bcx.ins().global_value(ptr_type, global);

                    value::Value::new_val(global, layout)
                }
                _ => unimplemented!(),
            }
        }
    }

    fn trans_rvalue(fx: &mut FunctionCtx<Self>, place: Self::Place, rvalue: &mir::RValue) {
        match rvalue {
            mir::RValue::Use(op) => {
                Self::trans_op(fx, op, Some(place));
            }
            mir::RValue::AddrOf(val) => {
                let val = Self::trans_place(fx, val);

                val.write_place_ref(fx, place);
            }
            mir::RValue::Discr(val) => {
                let val = Self::trans_place(fx, val);
                let val = val.to_value(fx);

                unimplemented!();
            }
            mir::RValue::Init(_, ops) => {
                unimplemented!();
            }
        }
    }

    fn trans_term(fx: &mut FunctionCtx<Self>, term: &mir::Term) {
        match term {
            mir::Term::Unset => unreachable!(),
            mir::Term::Abort => {
                fx.bcx.ins().trap(ir::TrapCode::User(0));
            }
            mir::Term::Return => {
                use codegen::abi::{get_pass_mode, PassMode};
                let ret_layout = fx
                    .db
                    .layout_of(fx.lib, fx.body.locals[mir::Local::RET].ty.clone());
                let rets = match get_pass_mode(fx.mcx, &ret_layout) {
                    PassMode::ByVal(_) => {
                        let place = fx.locals[&mir::Local::RET].clone();
                        let ret_val = place.to_value(fx).load_scalar(fx);

                        vec![ret_val]
                    }
                    PassMode::ByValPair(_, _) => {
                        let place = fx.locals[&mir::Local::RET].clone();
                        let (a, b) = place.to_value(fx).load_scalar_pair(fx);

                        vec![a, b]
                    }
                    _ => Vec::new(),
                };

                fx.bcx.ins().return_(&rets);
            }
            mir::Term::Jump(to) => {
                fx.bcx.ins().jump(fx.blocks[to], &[]);
            }
            mir::Term::Switch(op, vals, blocks) => {
                let mut switch = cranelift::frontend::Switch::new();
                let otherwise = fx.blocks[blocks.last().unwrap()];
                let val = Self::trans_op(fx, op, None).load_scalar(fx);

                for (val, block) in vals.iter().zip(blocks) {
                    switch.set_entry(*val, fx.blocks[block]);
                }

                switch.emit(&mut fx.bcx, val, otherwise);
            }
            mir::Term::Call(place, op, args, block) => {
                use codegen::abi::{get_pass_mode, value_for_arg, EmptySinglePair, PassMode};
                let place = Self::trans_place(fx, place);
                let args = args
                    .iter()
                    .map(|a| Self::trans_op(fx, a, None))
                    .collect::<Vec<_>>();

                let ret_mode = get_pass_mode(fx.mcx, &place.layout);
                let ret_ptr = match &ret_mode {
                    PassMode::ByRef { size: _ } => Some(place.as_ptr().get_addr(fx)),
                    _ => None,
                };

                let args = ret_ptr
                    .into_iter()
                    .chain(
                        args.into_iter()
                            .map(|a| {
                                value_for_arg!(
                                    fx,
                                    a,
                                    match a.on_stack(fx) {
                                        (ptr, None) => EmptySinglePair::Single(ptr.get_addr(fx)),
                                        (ptr, Some(meta)) => {
                                            EmptySinglePair::Pair(ptr.get_addr(fx), meta)
                                        }
                                    }
                                )
                            })
                            .flatten(),
                    )
                    .collect::<Vec<_>>();

                let inst = if let mir::Operand::Const(mir::Const::FuncAddr(id), _) = op {
                    let file = fx.db.module_tree(id.lib).file(id.module);
                    let hir = fx.db.module_hir(file);
                    let def = hir.def(*id);

                    match def {
                        hir::ir::Def::Item(item) if item.is_intrinsic() => {
                            call_intrinsic_match! {
                                fx, &**item.name.symbol, &args[..], place, block,
                                sqrtf32(f) -> sqrt,
                                sqrtf64(f) -> sqrt,
                            }

                            unreachable!();
                        }
                        _ => {
                            let func = fx.func_ids[id].0;
                            let func = fx.mcx.module.declare_func_in_func(func, &mut fx.bcx.func);

                            fx.bcx.ins().call(func, &args)
                        }
                    }
                } else {
                    unimplemented!();
                };

                match ret_mode {
                    PassMode::NoPass => {}
                    PassMode::ByRef { .. } => {}
                    PassMode::ByVal(_) => {
                        let ret_val = fx.bcx.inst_results(inst);
                        let ret_val = value::Value::new_val(ret_val[0], place.layout.clone());

                        place.store(fx, ret_val);
                    }
                    PassMode::ByValPair(_, _) => {
                        let ret_val = fx.bcx.inst_results(inst);
                        let ret_val = value::Value::new_val_pair(
                            ret_val[0],
                            ret_val[1],
                            place.layout.clone(),
                        );

                        place.store(fx, ret_val);
                    }
                }

                fx.bcx.ins().jump(fx.blocks[block], &[]);
            }
        }
    }
}

macro call_intrinsic_match {
    ($fx:expr, $intrinsic:expr, $args:expr, $ret:expr, $next:expr, $(
        $name:ident($($arg:ident),*) -> $func:ident,
    )*) => {
        match $intrinsic {
            $(
                stringify!($name) => {
                    if let [$($arg),*] = $args {
                        let val = value::Value::new_val($fx.bcx.ins().$func($($arg.clone()),*), $ret.layout.clone());

                        $ret.store($fx, val);
                        $fx.bcx.ins().jump($fx.blocks[$next], &[]);
                        return;
                    }
                }
            )*
            _ => {},
        }
    }
}

pub struct IrType<'ctx>(PhantomData<&'ctx cranelift::codegen::Context>);

impl<'ctx> ClifBackend<'ctx> {
    pub fn new() -> Self {
        ClifBackend {
            ptr_type: cranelift::prelude::Type::default(),
            ssa_vars: 0,
            anon_count: 0,
            func_ids: HashMap::new(),
            data_ids: HashMap::new(),
            func_ctx: None,
            _ctx: PhantomData,
        }
    }

    fn next_ssa_var(&mut self) -> u32 {
        self.ssa_vars += 1;
        self.ssa_vars
    }

    fn trans_bytes(
        fx: &mut FunctionCtx<'_, '_, Self>,
        bytes: &Box<[u8]>,
        layout: layout::TyLayout<mir::Ty>,
    ) -> value::Value<'ctx> {
        use cranelift_module::DataContext;
        let anon_count = fx.anon_count;
        let data_id = fx
            .module
            .declare_data(
                &format!("__data_{}", anon_count),
                Linkage::Local,
                false,
                false,
            )
            .unwrap();

        let mut data_ctx = DataContext::new();

        fx.anon_count += 1;
        data_ctx.define(bytes.clone());
        fx.module.define_data(data_id, &data_ctx).unwrap();

        let global = fx
            .mcx
            .module
            .declare_data_in_func(data_id, &mut fx.bcx.func);

        let ptr_type = fx.ptr_type;
        let value = fx.bcx.ins().global_value(ptr_type, global);
        let len = fx.bcx.ins().iconst(ptr_type, bytes.len() as i64);

        value::Value::new_val_pair(value, len, layout)
    }
}

impl<'ctx> codegen::Type for IrType<'ctx> {
    type Backend = ClifBackend<'ctx>;
    type Raw = cranelift::prelude::Type;

    fn ir_type(
        layout: &layout::TyLayout<check::ty::Ty>,
        mcx: &ModuleCtx<Self::Backend>,
    ) -> Option<Self::Raw> {
        if let layout::Abi::Scalar(s) = &layout.abi {
            Some(Self::scalar_ty(s, mcx))
        } else {
            None
        }
    }

    fn ir_pair_type(
        layout: &layout::TyLayout<check::ty::Ty>,
        mcx: &ModuleCtx<Self::Backend>,
    ) -> Option<(Self::Raw, Self::Raw)> {
        if let layout::Abi::ScalarPair(a, b) = &layout.abi {
            Some((Self::scalar_ty(a, mcx), Self::scalar_ty(b, mcx)))
        } else {
            None
        }
    }

    fn scalar_ty(scalar: &layout::Scalar, mcx: &ModuleCtx<Self::Backend>) -> Self::Raw {
        use cranelift::prelude::types;
        use layout::{Integer, Primitive};
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
            Primitive::Pointer => mcx.module.target_config().pointer_type(),
        }
    }
}
