use crate::ptr::Pointer;
use crate::value::{Value, ValueKind};
use crate::FunctionCtx;
use check::layout::{Abi, Scalar};
use check::ty::{Layout, Type};
use cranelift::codegen::entity::EntityRef;
use cranelift::codegen::ir::{self as cir, InstBuilder};
use cranelift::frontend::Variable;
use cranelift_module::Backend;
use std::convert::{TryFrom, TryInto};

#[derive(Debug, Clone, Copy)]
pub struct Place<'tcx> {
    pub kind: PlaceKind,
    pub layout: Layout<'tcx>,
}

#[derive(Debug, Clone, Copy)]
pub enum PlaceKind {
    Var(Variable),
    VarPair(Variable, Variable),
    Addr(Pointer, Option<cir::Value>),
}

impl<'tcx> Place<'tcx> {
    pub fn new_var<'a>(fx: &mut FunctionCtx<'a, 'tcx, impl Backend>, layout: Layout<'tcx>) -> Self {
        let var = Variable::with_u32(fx.next_ssa_var());

        fx.builder.declare_var(var, fx.clif_type(layout).unwrap());

        Place {
            kind: PlaceKind::Var(var),
            layout,
        }
    }

    pub fn new_pair(fx: &mut FunctionCtx<'_, 'tcx, impl Backend>, layout: Layout<'tcx>) -> Self {
        let var1 = Variable::with_u32(fx.next_ssa_var());
        let var2 = Variable::with_u32(fx.next_ssa_var());
        let (ty1, ty2) = fx.clif_pair_type(layout).unwrap();

        fx.builder.declare_var(var1, ty1);
        fx.builder.declare_var(var2, ty2);

        Place {
            kind: PlaceKind::VarPair(var1, var2),
            layout,
        }
    }

    pub fn no_place(layout: Layout<'tcx>) -> Self {
        Place {
            kind: PlaceKind::Addr(Pointer::dangling(layout.align), None),
            layout,
        }
    }

    pub fn new_stack(fx: &mut FunctionCtx<impl Backend>, layout: Layout<'tcx>) -> Self {
        if layout.is_zst() {
            return Place::no_place(layout);
        }

        let slot = fx.builder.create_stack_slot(cir::StackSlotData {
            kind: cir::StackSlotKind::ExplicitSlot,
            size: layout.size.bytes() as u32,
            offset: None,
        });

        Place {
            kind: PlaceKind::Addr(Pointer::stack(slot), None),
            layout,
        }
    }

    pub fn new_ref(ptr: Pointer, layout: Layout<'tcx>) -> Self {
        Place {
            kind: PlaceKind::Addr(ptr, None),
            layout,
        }
    }

    pub fn new_ref_meta(ptr: Pointer, meta: cir::Value, layout: Layout<'tcx>) -> Self {
        Place {
            kind: PlaceKind::Addr(ptr, Some(meta)),
            layout,
        }
    }

    #[track_caller]
    pub fn as_ptr(self) -> Pointer {
        match self.as_ptr_maybe_unsized() {
            (ptr, None) => ptr,
            (_, Some(_)) => unreachable!(),
        }
    }

    #[track_caller]
    pub fn as_ptr_maybe_unsized(self) -> (Pointer, Option<cir::Value>) {
        match self.kind {
            PlaceKind::Addr(ptr, meta) => (ptr, meta),
            PlaceKind::Var(_) | PlaceKind::VarPair(_, _) => unreachable!(),
        }
    }

    pub fn to_value(self, fx: &mut FunctionCtx<impl Backend>) -> Value<'tcx> {
        match self.kind {
            PlaceKind::Var(var) => {
                let val = fx.builder.use_var(var);

                Value::new_val(val, self.layout)
            }
            PlaceKind::VarPair(var1, var2) => {
                let val1 = fx.builder.use_var(var1);

                fx.builder
                    .set_val_label(val1, cir::ValueLabel::new(var1.index()));

                let val2 = fx.builder.use_var(var2);

                fx.builder
                    .set_val_label(val2, cir::ValueLabel::new(var2.index()));

                Value::new_pair(val1, val2, self.layout)
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

    pub fn deref<'a>(self, fx: &mut FunctionCtx<'a, 'tcx, impl Backend>) -> Self {
        let inner_layout = fx.tcx.layout(self.layout.ty.pointee(fx.tcx));

        Place::new_ref(
            Pointer::addr(self.to_value(fx).load_scalar(fx)),
            inner_layout,
        )
    }

    pub fn index<'a>(self, fx: &mut FunctionCtx<'a, 'tcx, impl Backend>, idx: cir::Value) -> Self {
        let layout = fx.tcx.layout(self.layout.ty.idx(fx.tcx));
        let new_idx = fx.builder.ins().imul_imm(idx, layout.stride.bytes() as i64);

        match &*self.layout.ty {
            Type::Ptr(check::ty::PtrKind::Multiple(_), _) => {
                let ptr = Pointer::addr(self.to_value(fx).load_scalar(fx));
                let new_ptr = ptr.offset_value(fx, new_idx);

                Place {
                    kind: PlaceKind::Addr(new_ptr, None),
                    layout,
                }
            }
            Type::Array(..) => {
                let ptr = self.as_ptr();
                let new_ptr = ptr.offset_value(fx, new_idx);

                Place {
                    kind: PlaceKind::Addr(new_ptr, None),
                    layout,
                }
            }
            Type::Slice(_) => {
                let ptr = self.field(fx, 0).deref(fx).as_ptr();
                let new_ptr = ptr.offset_value(fx, new_idx);

                Place {
                    kind: PlaceKind::Addr(new_ptr, None),
                    layout,
                }
            }
            _ => unreachable!(),
        }
    }

    pub fn field(self, fx: &mut FunctionCtx<'_, 'tcx, impl Backend>, idx: usize) -> Self {
        let layout = self.layout.field(fx.tcx, idx);

        match self.kind {
            PlaceKind::Var(_var) => {
                // TODO: handle vector types
            }
            PlaceKind::VarPair(var1, var2) => match idx {
                0 => {
                    return Place {
                        kind: PlaceKind::Var(var1),
                        layout,
                    }
                }
                1 => {
                    return Place {
                        kind: PlaceKind::Var(var2),
                        layout,
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
        }
    }

    pub fn store<'a>(self, fx: &mut FunctionCtx<'a, 'tcx, impl Backend>, from: Value<'tcx>) {
        fn transmute_value<'tcx>(
            fx: &mut FunctionCtx<'_, 'tcx, impl Backend>,
            var: Variable,
            data: cir::Value,
            dst_ty: cir::Type,
        ) {
            let src_ty = fx.builder.func.dfg.value_type(data);
            let data = match (src_ty, dst_ty) {
                (_, _) if src_ty == dst_ty => data,

                // This is a `write_cvalue_transmute`.
                (cir::types::I32, cir::types::F32)
                | (cir::types::F32, cir::types::I32)
                | (cir::types::I64, cir::types::F64)
                | (cir::types::F64, cir::types::I64) => fx.builder.ins().bitcast(dst_ty, data),
                _ if src_ty.is_vector() && dst_ty.is_vector() => {
                    fx.builder.ins().raw_bitcast(dst_ty, data)
                }
                _ => unreachable!("write_cvalue_transmute: {:?} -> {:?}", src_ty, dst_ty),
            };
            fx.builder
                .set_val_label(data, cir::ValueLabel::new(var.index()));
            fx.builder.def_var(var, data);
        }

        let dst_layout = self.layout;
        let to_ptr = match self.kind {
            PlaceKind::Var(var) => {
                let data = Value {
                    kind: from.kind,
                    layout: dst_layout,
                }
                .load_scalar(fx);

                let dst_ty = fx.clif_type(self.layout).unwrap();

                transmute_value(fx, var, data, dst_ty);

                return;
            }
            PlaceKind::VarPair(var1, var2) => {
                let (data1, data2) = Value {
                    kind: from.kind,
                    layout: dst_layout,
                }
                .load_scalar_pair(fx);

                let (dst_ty1, dst_ty2) = fx.clif_pair_type(self.layout).unwrap();

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

        match &from.layout.abi {
            Abi::Scalar(_) => {
                let val = from.load_scalar(fx);

                to_ptr.store(fx, val, cir::MemFlags::new());

                return;
            }
            Abi::ScalarPair(a, b) => {
                let (value, meta) = from.load_scalar_pair(fx);
                let b_offset = scalar_pair_calculate_b_offset(fx.module.isa().triple(), a, b);

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
            ValueKind::Pair(_, _) => unreachable!(),
            ValueKind::Ref(val_ptr, None) => {
                let from_addr = val_ptr.get_addr(fx);
                let to_addr = to_ptr.get_addr(fx);
                let src_layout = from.layout;
                let size = dst_layout.size.bytes();
                let src_align = src_layout.align.bytes() as u8;
                let dst_align = dst_layout.align.bytes() as u8;

                fx.builder.emit_small_memory_copy(
                    fx.module.target_config(),
                    to_addr,
                    from_addr,
                    size,
                    dst_align,
                    src_align,
                    true,
                );
            }
            ValueKind::Ref(_val_ptr, Some(_val_ty)) => {
                unreachable!();
                // let val_ty = Pointer::addr(val_ty);
                // let layout = fx.tcx.layout_of(&fx.tcx.lang_items.type_layout().unwrap());
                // let val_ty = Place::new_ref(val_ty, layout);
                // let val_size = val_ty.field(fx, 0).to_value(fx).load_scalar(fx);
                // let from_addr = val_ptr.get_addr(fx);
                // let to_addr = to_ptr.get_addr(fx);
                // let mut memcpy = fx.module.make_signature();
                //
                // memcpy.params.push(cir::AbiParam::new(fx.pointer_type));
                // memcpy.params.push(cir::AbiParam::new(fx.pointer_type));
                // memcpy.params.push(cir::AbiParam::new(fx.pointer_type));
                //
                // let memcpy = fx
                //     .module
                //     .declare_function("memcpy", cranelift_module::Linkage::Import, &memcpy)
                //     .unwrap();
                //
                // let memcpy = fx.module.declare_func_in_func(memcpy, fx.builder.func);
                //
                // fx.builder
                //     .ins()
                //     .call(memcpy, &[to_addr, from_addr, val_size]);
            }
        }
    }

    pub fn write_place_ref(self, fx: &mut FunctionCtx<'_, 'tcx, impl Backend>, dest: Place<'tcx>) {
        if let Abi::ScalarPair(_, _) = self.layout.abi {
            let (ptr, extra) = self.as_ptr_maybe_unsized();
            let ptr = if let Some(extra) = extra {
                Value::new_pair(ptr.get_addr(fx), extra, dest.layout)
            } else {
                Value::new_val(ptr.get_addr(fx), dest.layout)
            };

            dest.store(fx, ptr);
        } else {
            let ptr = Value::new_val(self.as_ptr().get_addr(fx), dest.layout);

            dest.store(fx, ptr);
        }
    }

    pub fn downcast_variant(
        self,
        fx: &FunctionCtx<'_, 'tcx, impl Backend>,
        variant: usize,
    ) -> Self {
        let layout = self.layout.for_variant(fx.tcx, variant);

        Place {
            kind: self.kind,
            layout,
        }
    }
}

fn scalar_pair_calculate_b_offset(
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
