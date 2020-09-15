use crate::place::Place;
use crate::ptr::Pointer;
use crate::FunctionCtx;
use check::layout::{Abi, Primitive, Scalar};
use check::ty::{Layout, Type};
use cranelift::codegen::ir::{self as cir, InstBuilder};
use cranelift_module::Backend;
use std::convert::{TryFrom, TryInto};

#[derive(Clone, Copy)]
pub struct Value<'tcx> {
    pub kind: ValueKind,
    pub layout: Layout<'tcx>,
}

#[derive(Debug, Clone, Copy)]
pub enum ValueKind {
    Ref(Pointer, Option<cir::Value>),
    Val(cir::Value),
    Pair(cir::Value, cir::Value),
}

impl<'tcx> Value<'tcx> {
    pub fn new_ref(ptr: Pointer, layout: Layout<'tcx>) -> Self {
        Value {
            kind: ValueKind::Ref(ptr, None),
            layout,
        }
    }

    pub fn new_ref_meta(ptr: Pointer, meta: cir::Value, layout: Layout<'tcx>) -> Self {
        Value {
            kind: ValueKind::Ref(ptr, Some(meta)),
            layout,
        }
    }

    pub fn new_val(value: cir::Value, layout: Layout<'tcx>) -> Self {
        Value {
            kind: ValueKind::Val(value),
            layout,
        }
    }

    pub fn new_pair(a: cir::Value, b: cir::Value, layout: Layout<'tcx>) -> Self {
        Value {
            kind: ValueKind::Pair(a, b),
            layout,
        }
    }

    pub fn new_const<'a>(
        fx: &mut FunctionCtx<'a, 'tcx, impl Backend>,
        val: u128,
        layout: Layout<'tcx>,
    ) -> Self {
        let clif_type = fx.clif_type(layout).unwrap();

        let val = match &layout.abi {
            Abi::Scalar(scalar) => match scalar.value {
                Primitive::F32 => fx.builder.ins().f32const(f32::from_bits(val as u32)),
                Primitive::F64 => fx.builder.ins().f64const(val as u64),
                _ => fx.builder.ins().iconst(clif_type, val as i64),
            },
            _ => unimplemented!(),
        };

        Value::new_val(val, layout)
    }

    pub fn new_unit(layout: Layout<'tcx>) -> Value<'tcx> {
        Value::new_val(cir::Value::with_number(0).unwrap(), layout)
    }

    pub fn on_stack<'a>(
        self,
        fx: &mut FunctionCtx<'a, 'tcx, impl Backend>,
    ) -> (Pointer, Option<cir::Value>) {
        match self.kind {
            ValueKind::Ref(ptr, meta) => (ptr, meta),
            ValueKind::Val(_) | ValueKind::Pair(_, _) => {
                let place = Place::new_stack(fx, self.layout);

                place.store(fx, self);
                (place.as_ptr(), None)
            }
        }
    }

    #[track_caller]
    pub fn load_scalar<'a>(self, fx: &mut FunctionCtx<'a, 'tcx, impl Backend>) -> cir::Value {
        match self.kind {
            ValueKind::Ref(ptr, None) => {
                let clif_type = fx.clif_type(self.layout).unwrap();

                ptr.load(fx, clif_type, cir::MemFlags::new())
            }
            ValueKind::Val(val) => val,
            ValueKind::Ref(_, Some(_)) => unreachable!(),
            ValueKind::Pair(_, _) => unreachable!(),
        }
    }

    pub fn load_scalar_pair<'a>(
        self,
        fx: &mut FunctionCtx<'a, 'tcx, impl Backend>,
    ) -> (cir::Value, cir::Value) {
        match self.kind {
            ValueKind::Ref(ptr, None) => {
                let (a, b) = match &self.layout.abi {
                    Abi::ScalarPair(a, b) => (a, b),
                    _ => unreachable!(),
                };

                let b_offset = scalar_pair_calculate_b_offset(fx.module.isa().triple(), a, b);
                let clif_ty1 = super::scalar_clif_type(fx.module, a);
                let clif_ty2 = super::scalar_clif_type(fx.module, b);
                let val1 = ptr.load(fx, clif_ty1, cir::MemFlags::new());
                let val2 = ptr
                    .offset(fx, b_offset)
                    .load(fx, clif_ty2, cir::MemFlags::new());

                (val1, val2)
            }
            ValueKind::Ref(ptr, Some(meta)) => {
                // TODO: test if this always works as expected
                (ptr.get_addr(fx), meta)
            }
            ValueKind::Val(_) => unreachable!(),
            ValueKind::Pair(a, b) => (a, b),
        }
    }

    pub fn cast(self, layout: Layout<'tcx>) -> Self {
        Value {
            kind: self.kind,
            layout,
        }
    }

    pub fn field<'a>(self, fx: &mut FunctionCtx<'a, 'tcx, impl Backend>, idx: usize) -> Self {
        match self.kind {
            ValueKind::Val(_val) => unimplemented!(),
            ValueKind::Ref(ptr, None) => {
                let (field_ptr, field_layout) = gen_field(fx, ptr, None, self.layout, idx);

                Value::new_ref(field_ptr, field_layout)
            }
            ValueKind::Ref(ptr, Some(meta)) => {
                let (field_ptr, field_layout) = gen_field(fx, ptr, Some(meta), self.layout, idx);

                Value::new_ref_meta(field_ptr, meta, field_layout)
            }
            ValueKind::Pair(a, b) => {
                let field_layout = self.layout.field(fx.tcx, idx);

                if idx == 0 {
                    Value::new_val(a, field_layout)
                } else {
                    Value::new_val(b, field_layout)
                }
            }
        }
    }

    pub fn deref<'a>(self, fx: &mut FunctionCtx<'a, 'tcx, impl Backend>) -> Self {
        let inner_layout = fx.tcx.layout(self.layout.ty.pointee(fx.tcx));
        let ptr = self.load_scalar(fx);

        Value::new_val(ptr, inner_layout)
    }
}

fn gen_field<'tcx>(
    fx: &mut FunctionCtx<'_, 'tcx, impl Backend>,
    base: Pointer,
    meta: Option<cir::Value>,
    layout: Layout<'tcx>,
    field: usize,
) -> (Pointer, Layout<'tcx>) {
    let field_offset = layout.fields.offset(field);
    let field_layout = layout.field(fx.tcx, field);
    let simple = |fx: &mut FunctionCtx<'_, '_, _>| {
        (
            base.offset_i64(fx, i64::try_from(field_offset.bytes()).unwrap()),
            field_layout,
        )
    };

    if let Some(_meta) = meta {
        if !field_layout.is_unsized() {
            return simple(fx);
        }

        match field_layout.ty {
            Type::Str => simple(fx),
            _ => unimplemented!(),
        }
    } else {
        simple(fx)
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
