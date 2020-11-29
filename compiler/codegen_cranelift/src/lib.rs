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
        let func = mcx
            .module
            .declare_function(&name, Linkage::Export, &sig)
            .unwrap();

        func
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
                unimplemented!();
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
                unimplemented!();
            }
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
        None
    }

    fn ir_pair_type(
        layout: &layout::TyLayout<check::ty::Ty>,
        mcx: &ModuleCtx<Self::Backend>,
    ) -> Option<(Self::Raw, Self::Raw)> {
        None
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
