use crate::place::Place;
use crate::ptr::Pointer;
use crate::FunctionCtx;
use cranelift::codegen::ir as cir;
use cranelift::prelude::InstBuilder;
use mir::layout::{Abi, Layout, Primitive};
use std::convert::TryFrom;
use std::sync::Arc;

#[derive(Debug, Clone)]
pub(crate) struct Value {
    pub(crate) kind: ValueKind,
    pub(crate) layout: Arc<Layout>,
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum ValueKind {
    Ref(Pointer, Option<cir::Value>),
    Val(cir::Value),
    ValPair(cir::Value, cir::Value),
}

impl Value {
    pub(crate) fn new_ref(ptr: Pointer, layout: Arc<Layout>) -> Self {
        Value {
            kind: ValueKind::Ref(ptr, None),
            layout,
        }
    }

    pub(crate) fn new_ref_meta(ptr: Pointer, meta: cir::Value, layout: Arc<Layout>) -> Self {
        Value {
            kind: ValueKind::Ref(ptr, Some(meta)),
            layout,
        }
    }

    pub(crate) fn new_val(val: cir::Value, layout: Arc<Layout>) -> Self {
        Value {
            kind: ValueKind::Val(val),
            layout,
        }
    }

    pub(crate) fn new_val_pair(val1: cir::Value, val2: cir::Value, layout: Arc<Layout>) -> Self {
        Value {
            kind: ValueKind::ValPair(val1, val2),
            layout,
        }
    }

    pub(crate) fn new_const(val: u128, fx: &mut FunctionCtx, layout: Arc<Layout>) -> Self {
        let ty = fx.ir_type(&layout).unwrap();
        let val = match &layout.abi {
            | Abi::Scalar(scalar) => match scalar.value {
                | Primitive::F32 => fx.bcx.ins().f32const(f64::from_bits(val as u64) as f32),
                | Primitive::F64 => fx.bcx.ins().f64const(val as u64),
                | _ => fx.bcx.ins().iconst(ty, val as i64),
            },
            | _ => unimplemented!(),
        };

        Value::new_val(val, layout)
    }

    pub(crate) fn new_unit() -> Self {
        Value::new_val(cir::Value::with_number(0).unwrap(), Arc::new(Layout::UNIT))
    }

    pub(crate) fn on_stack(self, fx: &mut FunctionCtx) -> (Pointer, Option<cir::Value>) {
        match self.kind {
            | ValueKind::Ref(ptr, meta) => (ptr, meta),
            | ValueKind::Val(_) | ValueKind::ValPair(_, _) => {
                let place = Place::new_stack(fx, self.layout.clone());

                place.clone().store(fx, self);

                (place.as_ptr(), None)
            },
        }
    }

    #[track_caller]
    pub(crate) fn load_scalar(self, fx: &mut FunctionCtx) -> cir::Value {
        match self.kind {
            | ValueKind::Ref(ptr, None) => {
                let ty = fx.ir_type(&self.layout).unwrap();

                ptr.load(fx, ty, cir::MemFlags::new())
            },
            | ValueKind::Val(val) => val,
            | ValueKind::Ref(_, Some(_)) => unreachable!(),
            | ValueKind::ValPair(_, _) => unreachable!(),
        }
    }

    pub(crate) fn load_scalar_pair(self, fx: &mut FunctionCtx) -> (cir::Value, cir::Value) {
        match self.kind {
            | ValueKind::Ref(ptr, None) => {
                let (a, b) = match &self.layout.abi {
                    | Abi::ScalarPair(a, b) => (a, b),
                    | _ => unreachable!(),
                };

                let b_offset = crate::place::scalar_pair_calculate_b_offset(&fx.triple, a, b);
                let ty1 = fx.scalar_type(a);
                let ty2 = fx.scalar_type(b);
                let val1 = ptr.load(fx, ty1, cir::MemFlags::new());
                let val2 = ptr.offset(fx, b_offset).load(fx, ty2, cir::MemFlags::new());

                (val1, val2)
            },
            | ValueKind::Ref(ptr, Some(meta)) => (ptr.get_addr(fx), meta),
            | ValueKind::Val(_) => unreachable!(),
            | ValueKind::ValPair(a, b) => (a, b),
        }
    }

    pub(crate) fn cast(self, _fx: &mut FunctionCtx, layout: Arc<Layout>) -> Self {
        Value {
            kind: self.kind,
            layout,
        }
    }

    pub(crate) fn field(self, fx: &mut FunctionCtx, idx: usize) -> Self {
        match self.kind {
            | ValueKind::Val(_) => {
                if idx == 0 {
                    self
                } else {
                    unreachable!();
                }
            },
            | ValueKind::Ref(ptr, None) => {
                let (field_ptr, field_layout) = gen_field(fx, ptr, None, self.layout, idx);

                Value::new_ref(field_ptr, field_layout)
            },
            | ValueKind::Ref(ptr, Some(meta)) => {
                let (field_ptr, field_layout) = gen_field(fx, ptr, Some(meta), self.layout, idx);

                Value::new_ref_meta(field_ptr, meta, field_layout)
            },
            | ValueKind::ValPair(a, b) => {
                let field_layout = self.layout.field(fx.db.upcast(), idx).unwrap();

                if idx == 0 {
                    Value::new_val(a, field_layout)
                } else {
                    Value::new_val(b, field_layout)
                }
            },
        }
    }

    pub(crate) fn deref(self, fx: &mut FunctionCtx) -> Self {
        let pointee = self.layout.elem(fx.db.upcast()).unwrap();
        let ptr = self.load_scalar(fx);

        // if is_box {
        //     use crate::clif::Module;
        //     let ptr_type = fx.module.target_config().pointer_type();
        //     let ptr = Pointer::addr(ptr);
        //     let ptr = ptr.load(fx, ptr_type, crate::clif::MemFlags::trusted());
        //
        //     Value::new_ref(Pointer::addr(ptr), pointee)
        // } else {
        Value::new_val(ptr, pointee)
        // }
    }
}

fn gen_field(
    fx: &mut FunctionCtx,
    base: Pointer,
    meta: Option<cir::Value>,
    layout: Arc<Layout>,
    field: usize,
) -> (Pointer, Arc<Layout>) {
    let field_offset = layout.fields.offset(field);
    let field_layout = layout.field(fx.db.upcast(), field).unwrap();
    let simple = |fx| {
        (
            base.offset_i64(fx, i64::try_from(field_offset.bytes()).unwrap()),
            field_layout.clone(),
        )
    };

    if let Some(_meta) = meta {
        if !field_layout.abi.is_unsized() {
            return simple(fx);
        }

        unimplemented!()
    } else {
        simple(fx)
    }
}
