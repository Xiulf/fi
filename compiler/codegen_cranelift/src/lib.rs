#![feature(decl_macro)]

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

codegen::define_codegen_backend!(ClifBackend);

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

    fn create_module(&mut self, lib: source::LibId, db: &dyn CodegenDatabase) -> Self::Module {
        let triple = db.target(lib);
        let manifest = db.manifest(lib);
        let flags_builder = cranelift::codegen::settings::builder();
        let flags = cranelift::codegen::settings::Flags::new(flags_builder);
        let isa = cranelift::codegen::isa::lookup(triple)
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
        let name = mangling::mangle(name.bytes());
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
        let name = mangling::mangle(name.bytes());
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

        for arg in fx.body.args() {
            let layout = fx.db.layout_of(fx.lib, arg.ty.clone());
            let value = match get_pass_mode(fx.mcx, &layout) {
                PassMode::NoPass => continue,
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
                    let meta = fx.bcx.append_block_param(start_block, fx.ptr_type);

                    value::Value::new_ref_meta(ptr::Pointer::addr(ptr), meta, layout.clone())
                }
            };

            let ssa = ssa_map[&arg.id] == codegen::analyze::SsaKind::Ssa;
            let place = local_place(fx, arg.id, layout, ssa);

            place.store(fx, value);
        }

        for local in &fx.body.locals {
            if let mir::LocalKind::Var | mir::LocalKind::Tmp = local.kind {
                let ssa = ssa_map[&local.id] == codegen::analyze::SsaKind::Ssa;
                let layout = fx.db.layout_of(fx.lib, local.ty.clone());

                local_place(fx, local.id, layout, ssa);
            }
        }

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
                    let idx = Self::trans_op(fx, idx);

                    res = res.index(fx, idx);
                }
                mir::PlaceElem::Downcast(idx) => res = res.downcast_variant(fx, *idx),
            }
        }

        res
    }

    fn trans_const(fx: &mut FunctionCtx<Self>, const_: &mir::Const, ty: &mir::Ty) -> Self::Value {
        let layout = fx.db.layout_of(fx.lib, ty.clone());

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
            _ => unimplemented!(),
        }
    }

    fn trans_rvalue(fx: &mut FunctionCtx<Self>, place: Self::Place, rvalue: &mir::RValue) {
        match rvalue {
            mir::RValue::Use(op) => {
                let value = Self::trans_op(fx, op);

                place.store(fx, value);
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
                let val = Self::trans_op(fx, op).load_scalar(fx);

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
                    .map(|a| Self::trans_op(fx, a))
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
