use crate::ptr::Pointer;
use crate::value::{Value, ValueKind};
use crate::FunctionCtx;
use check::layout::{Abi, Scalar};
use check::ty::Layout;
use cranelift::codegen::ir::{self as cir, InstBuilder};
use cranelift::frontend::Variable;
use cranelift_module::Backend;
use std::convert::{TryFrom, TryInto};

#[derive(Clone, Copy)]
pub struct Place<'tcx> {
    pub kind: PlaceKind,
    pub layout: Layout<'tcx>,
}

#[derive(Clone, Copy)]
pub enum PlaceKind {
    Var(Variable),
    Addr(Pointer, Option<cir::Value>),
}

impl<'tcx> Place<'tcx> {
    pub fn new_var<'a>(
        fx: &mut FunctionCtx<'a, 'tcx, impl Backend>,
        id: mir::LocalId,
        layout: Layout<'tcx>,
    ) -> Self {
        let var = Variable::with_u32(id.as_u32());

        fx.builder.declare_var(var, fx.clif_type(layout).unwrap());

        Place {
            kind: PlaceKind::Var(var),
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

    pub fn as_ptr(self) -> Pointer {
        match self.as_ptr_maybe_unsized() {
            (ptr, None) => ptr,
            (_, Some(_)) => unreachable!(),
        }
    }

    pub fn as_ptr_maybe_unsized(self) -> (Pointer, Option<cir::Value>) {
        match self.kind {
            PlaceKind::Addr(ptr, meta) => (ptr, meta),
            PlaceKind::Var(_) => unreachable!(),
        }
    }

    pub fn to_value(self, fx: &mut FunctionCtx<impl Backend>) -> Value<'tcx> {
        match self.kind {
            PlaceKind::Var(var) => {
                let val = fx.builder.use_var(var);

                Value::new_val(val, self.layout)
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
        let inner_layout = fx.tcx.layout(self.layout.ty.pointee());

        Place::new_ref(
            Pointer::addr(self.to_value(fx).load_scalar(fx)),
            inner_layout,
        )
    }

    pub fn index<'a>(self, fx: &mut FunctionCtx<'a, 'tcx, impl Backend>, idx: cir::Value) -> Self {
        let layout = fx.tcx.layout(self.layout.ty.idx(fx.tcx));
        let new_idx = fx.builder.ins().imul_imm(idx, layout.size.bytes() as i64);

        match &*self.layout.ty {
            // Type::Array(..) => {
            //     let ptr = self.as_ptr(fx);
            //     let new_ptr = ptr.offset_value(fx, new_idx);
            //     Place {
            //         kind: PlaceKind::Addr(new_ptr),
            //         layout,
            //     }
            // }
            _ => {
                let ptr = self.field(fx, 0).deref(fx).as_ptr();
                let new_ptr = ptr.offset_value(fx, new_idx);

                Place {
                    kind: PlaceKind::Addr(new_ptr, None),
                    layout,
                }
            }
        }
    }

    pub fn field(self, fx: &mut FunctionCtx<'_, 'tcx, impl Backend>, idx: usize) -> Self {
        let offset = self.layout.fields.offset(idx);
        let layout = self.layout.field(fx.tcx, idx);
        let ptr = self.as_ptr();
        let new_ptr = ptr.offset_i64(fx, i64::try_from(offset.bytes()).unwrap());

        Place {
            kind: PlaceKind::Addr(new_ptr, None),
            layout,
        }
    }

    pub fn store<'a>(self, fx: &mut FunctionCtx<'a, 'tcx, impl Backend>, from: Value<'tcx>) {
        let dst_layout = self.layout;
        let to_ptr = match self.kind {
            PlaceKind::Var(var) => {
                let data = Value {
                    kind: from.kind,
                    layout: dst_layout,
                }
                .load_scalar(fx);

                let src_ty = fx.builder.func.dfg.value_type(data);
                let dst_ty = fx.clif_type(dst_layout).unwrap();
                let data = match (src_ty, dst_ty) {
                    (_, _) if src_ty == dst_ty => data,
                    (cir::types::I32, cir::types::F32)
                    | (cir::types::F32, cir::types::I32)
                    | (cir::types::I64, cir::types::F64)
                    | (cir::types::F64, cir::types::I64) => fx.builder.ins().bitcast(dst_ty, data),
                    _ if src_ty.is_vector() && dst_ty.is_vector() => {
                        fx.builder.ins().raw_bitcast(dst_ty, data)
                    }
                    _ => unreachable!("{} != {}", src_ty, dst_ty),
                };

                fx.builder.def_var(var, data);

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
            ValueKind::Ref(_, Some(_)) => unimplemented!(),
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
