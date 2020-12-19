use crate::ptr::Pointer;
use crate::value::{Value, ValueKind};
use crate::{ClifBackend, FunctionCtx};
use check::ty::Ty;
use codegen::Value as _;
use cranelift::codegen::ir as cir;
use cranelift::frontend::Variable;
use cranelift::prelude::InstBuilder;
use layout::{Abi, Scalar, TyLayout};
use std::convert::{TryFrom, TryInto};
use std::marker::PhantomData;

#[derive(Debug, Clone)]
pub struct Place<'ctx> {
    pub kind: PlaceKind,
    pub layout: TyLayout<Ty>,
    _marker: PhantomData<&'ctx cranelift::codegen::Context>,
}

#[derive(Debug, Clone, Copy)]
pub enum PlaceKind {
    Var(Variable),
    VarPair(Variable, Variable),
    Addr(Pointer, Option<cir::Value>),
}

impl<'ctx> Place<'ctx> {
    pub fn new_var(fx: &mut FunctionCtx<ClifBackend>, layout: TyLayout<Ty>) -> Self {
        let var = Variable::with_u32(fx.next_ssa_var());

        fx.bcx.declare_var(var, fx.ir_type(&layout).unwrap());

        Place {
            kind: PlaceKind::Var(var),
            layout,
            _marker: PhantomData,
        }
    }

    pub fn new_var_pair(fx: &mut FunctionCtx<ClifBackend>, layout: TyLayout<Ty>) -> Self {
        let var1 = Variable::with_u32(fx.next_ssa_var());
        let var2 = Variable::with_u32(fx.next_ssa_var());
        let (ty1, ty2) = fx.ir_pair_type(&layout).unwrap();

        fx.bcx.declare_var(var1, ty1);
        fx.bcx.declare_var(var2, ty2);

        Place {
            kind: PlaceKind::VarPair(var1, var2),
            layout,
            _marker: PhantomData,
        }
    }

    pub fn no_place(layout: TyLayout<Ty>) -> Self {
        Place {
            kind: PlaceKind::Addr(Pointer::dangling(layout.align), None),
            layout,
            _marker: PhantomData,
        }
    }

    pub fn new_stack(fx: &mut FunctionCtx<ClifBackend>, layout: TyLayout<Ty>) -> Self {
        if layout.is_zst() {
            return Place::no_place(layout);
        }

        let slot = fx.bcx.create_stack_slot(cir::StackSlotData {
            kind: cir::StackSlotKind::ExplicitSlot,
            size: layout.size.bytes() as u32,
            offset: None,
        });

        Place {
            kind: PlaceKind::Addr(Pointer::stack(slot), None),
            layout,
            _marker: PhantomData,
        }
    }

    pub fn new_ref(ptr: Pointer, layout: TyLayout<Ty>) -> Self {
        Place {
            kind: PlaceKind::Addr(ptr, None),
            layout,
            _marker: PhantomData,
        }
    }

    pub fn new_ref_meta(ptr: Pointer, meta: cir::Value, layout: TyLayout<Ty>) -> Self {
        Place {
            kind: PlaceKind::Addr(ptr, Some(meta)),
            layout,
            _marker: PhantomData,
        }
    }

    #[track_caller]
    pub fn as_ptr(&self) -> Pointer {
        match self.as_ptr_maybe_unsized() {
            (ptr, None) => ptr,
            (_, Some(_)) => unreachable!(),
        }
    }

    #[track_caller]
    pub fn as_ptr_maybe_unsized(&self) -> (Pointer, Option<cir::Value>) {
        match self.kind {
            PlaceKind::Addr(ptr, meta) => (ptr, meta),
            PlaceKind::Var(_) | PlaceKind::VarPair(_, _) => unreachable!(),
        }
    }
}

impl<'ctx> codegen::Place for Place<'ctx> {
    type Backend = ClifBackend<'ctx>;

    fn layout(&self) -> &TyLayout<Ty> {
        &self.layout
    }

    fn to_value(self, fx: &mut FunctionCtx<Self::Backend>) -> Value<'ctx> {
        match self.kind {
            PlaceKind::Var(var) => {
                let val = fx.bcx.use_var(var);

                Value::new_val(val, self.layout)
            }
            PlaceKind::VarPair(var1, var2) => {
                let val1 = fx.bcx.use_var(var1);
                let val2 = fx.bcx.use_var(var2);

                Value::new_val_pair(val1, val2, self.layout)
            }
            PlaceKind::Addr(ptr, meta) => {
                if let Some(meta) = meta {
                    Value::new_ref_meta(ptr, meta, self.layout)
                } else {
                    Value::new_ref(ptr, self.layout)
                }
            }
        }
    }

    fn deref(self, fx: &mut FunctionCtx<Self::Backend>) -> Self {
        let pointee = self.layout.pointee(fx.lib, fx.db.to_layout_db());

        Self::new_ref(Pointer::addr(self.to_value(fx).load_scalar(fx)), pointee)
    }

    fn index(self, fx: &mut FunctionCtx<Self::Backend>, idx: Value<'ctx>) -> Self {
        let layout = self.layout.element(fx.lib, fx.db.to_layout_db());
        let idx = idx.load_scalar(fx);
        let new_idx = fx.bcx.ins().imul_imm(idx, layout.stride.bytes() as i64);
        let ptr = self.as_ptr();
        let new_ptr = ptr.offset_value(fx, new_idx);

        Place {
            kind: PlaceKind::Addr(new_ptr, None),
            layout,
            _marker: PhantomData,
        }
    }

    fn field(self, fx: &mut FunctionCtx<Self::Backend>, idx: usize) -> Self {
        let layout = self.layout.field(fx.lib, fx.db.to_layout_db(), idx);

        match self.kind {
            PlaceKind::Var(_) => unimplemented!(),
            PlaceKind::VarPair(var1, var2) => match idx {
                0 => {
                    return Place {
                        kind: PlaceKind::Var(var1),
                        layout,
                        _marker: PhantomData,
                    }
                }
                1 => {
                    return Place {
                        kind: PlaceKind::Var(var2),
                        layout,
                        _marker: PhantomData,
                    }
                }
                _ => unreachable!(),
            },
            _ => {}
        }

        let (base, extra) = self.as_ptr_maybe_unsized();
        let offset = self.layout.fields.offset(idx);
        let ptr = base.offset_i64(fx, i64::try_from(offset.bytes()).unwrap());

        Place {
            kind: PlaceKind::Addr(ptr, extra),
            layout,
            _marker: PhantomData,
        }
    }

    fn store(self, fx: &mut FunctionCtx<Self::Backend>, from: Value<'ctx>) {
        fn transmute_value(
            fx: &mut FunctionCtx<ClifBackend>,
            var: Variable,
            data: cir::Value,
            dst_ty: cir::Type,
        ) {
            let src_ty = fx.bcx.func.dfg.value_type(data);
            let data = match (src_ty, dst_ty) {
                (_, _) if src_ty == dst_ty => data,
                (cir::types::I32, cir::types::F32)
                | (cir::types::F32, cir::types::I32)
                | (cir::types::I64, cir::types::F64)
                | (cir::types::F64, cir::types::I64) => fx.bcx.ins().bitcast(dst_ty, data),
                _ if src_ty.is_vector() && dst_ty.is_vector() => {
                    fx.bcx.ins().raw_bitcast(dst_ty, data)
                }
                _ => unreachable!(),
            };

            fx.bcx.def_var(var, data);
        }

        let dst_layout = self.layout.clone();
        let to_ptr = match self.kind {
            PlaceKind::Var(var) => {
                let data = Value {
                    kind: from.kind,
                    layout: dst_layout,
                    _marker: PhantomData,
                }
                .load_scalar(fx);

                let dst_ty = fx.ir_type(&self.layout).unwrap();

                transmute_value(fx, var, data, dst_ty);

                return;
            }
            PlaceKind::VarPair(var1, var2) => {
                let (data1, data2) = Value {
                    kind: from.kind,
                    layout: dst_layout,
                    _marker: PhantomData,
                }
                .load_scalar_pair(fx);

                let (dst_ty1, dst_ty2) = fx.ir_pair_type(&self.layout).unwrap();

                transmute_value(fx, var1, data1, dst_ty1);
                transmute_value(fx, var2, data2, dst_ty2);

                return;
            }
            PlaceKind::Addr(ptr, None) => {
                if dst_layout.is_zst() {
                    return;
                }

                ptr
            }
            PlaceKind::Addr(_, Some(_)) => unreachable!(),
        };

        match from.layout.abi.clone() {
            Abi::Scalar(_) => {
                let val = from.load_scalar(fx);

                to_ptr.store(fx, val, cir::MemFlags::new());

                return;
            }
            Abi::ScalarPair(a, b) => {
                let (value, meta) = from.load_scalar_pair(fx);
                let b_offset = scalar_pair_calculate_b_offset(&fx.db.target(fx.lib), &a, &b);

                to_ptr.store(fx, value, cir::MemFlags::new());
                to_ptr
                    .offset(fx, b_offset)
                    .store(fx, meta, cir::MemFlags::new());

                return;
            }
            _ => {}
        }

        match from.kind {
            ValueKind::Val(val) => {
                to_ptr.store(fx, val, cir::MemFlags::new());
            }
            ValueKind::ValPair(_, _) => unreachable!(),
            ValueKind::Ref(val_ptr, None) => {
                use cranelift_module::Module;
                let from_addr = val_ptr.get_addr(fx);
                let to_addr = to_ptr.get_addr(fx);
                let src_layout = from.layout;
                let size = dst_layout.size.bytes();
                let src_align = src_layout.align.bytes() as u8;
                let dst_align = dst_layout.align.bytes() as u8;

                fx.bcx.emit_small_memory_copy(
                    fx.module.target_config(),
                    to_addr,
                    from_addr,
                    size,
                    dst_align,
                    src_align,
                    true,
                );
            }
            ValueKind::Ref(_, Some(_)) => unreachable!(),
        }
    }

    fn write_place_ref(self, fx: &mut FunctionCtx<Self::Backend>, dest: Self) {
        if let Abi::ScalarPair(_, _) = self.layout.abi {
            let (ptr, extra) = self.as_ptr_maybe_unsized();
            let ptr = if let Some(extra) = extra {
                Value::new_val_pair(ptr.get_addr(fx), extra, dest.layout.clone())
            } else {
                Value::new_val(ptr.get_addr(fx), dest.layout.clone())
            };

            dest.store(fx, ptr);
        } else {
            let ptr = Value::new_val(self.as_ptr().get_addr(fx), dest.layout.clone());

            dest.store(fx, ptr);
        }
    }

    fn downcast_variant(self, fx: &mut FunctionCtx<Self::Backend>, variant: usize) -> Self {
        let layout = self.layout.variant(fx.db.to_layout_db(), variant);

        Place {
            kind: self.kind,
            layout,
            _marker: PhantomData,
        }
    }
}

pub(crate) fn scalar_pair_calculate_b_offset(
    triple: &target_lexicon::Triple,
    a_scalar: &Scalar,
    b_scalar: &Scalar,
) -> cir::immediates::Offset32 {
    let b_offset = a_scalar
        .value
        .size(triple)
        .align_to(b_scalar.value.align(triple));

    cir::immediates::Offset32::new(b_offset.bytes().try_into().unwrap())
}
