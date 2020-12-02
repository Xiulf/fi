use crate::place::Place;
use crate::ptr::Pointer;
use crate::{ClifBackend, FunctionCtx};
use check::ty::Ty;
use cranelift::codegen::ir as cir;
use cranelift::prelude::InstBuilder;
use layout::{Abi, Primitive, Scalar, TyLayout};
use std::convert::{TryFrom, TryInto};
use std::marker::PhantomData;

#[derive(Debug, Clone)]
pub struct Value<'ctx> {
    pub kind: ValueKind,
    pub layout: TyLayout<Ty>,
    pub(crate) _marker: PhantomData<&'ctx cranelift::codegen::Context>,
}

#[derive(Debug, Clone, Copy)]
pub enum ValueKind {
    Ref(Pointer, Option<cir::Value>),
    Val(cir::Value),
    ValPair(cir::Value, cir::Value),
}

impl<'ctx> Value<'ctx> {
    pub fn new_ref(ptr: Pointer, layout: TyLayout<Ty>) -> Self {
        Value {
            kind: ValueKind::Ref(ptr, None),
            layout,
            _marker: PhantomData,
        }
    }

    pub fn new_ref_meta(ptr: Pointer, meta: cir::Value, layout: TyLayout<Ty>) -> Self {
        Value {
            kind: ValueKind::Ref(ptr, Some(meta)),
            layout,
            _marker: PhantomData,
        }
    }

    pub fn new_val(val: cir::Value, layout: TyLayout<Ty>) -> Self {
        Value {
            kind: ValueKind::Val(val),
            layout,
            _marker: PhantomData,
        }
    }

    pub fn new_val_pair(val1: cir::Value, val2: cir::Value, layout: TyLayout<Ty>) -> Self {
        Value {
            kind: ValueKind::ValPair(val1, val2),
            layout,
            _marker: PhantomData,
        }
    }

    pub fn new_const(val: u128, fx: &mut FunctionCtx<ClifBackend>, layout: TyLayout<Ty>) -> Self {
        let ty = fx.ir_type(&layout).unwrap();
        let val = match &layout.abi {
            Abi::Scalar(scalar) => match scalar.value {
                Primitive::F32 => fx.bcx.ins().f32const(f64::from_bits(val as u64) as f32),
                Primitive::F64 => fx.bcx.ins().f64const(val as u64),
                _ => fx.bcx.ins().iconst(ty, val as i64),
            },
            _ => unimplemented!(),
        };

        Value::new_val(val, layout)
    }

    pub fn new_unit(layout: TyLayout<Ty>) -> Self {
        Value::new_val(cir::Value::with_number(0).unwrap(), layout)
    }

    pub fn on_stack(
        self,
        fx: &mut FunctionCtx<ClifBackend<'ctx>>,
    ) -> (Pointer, Option<cir::Value>) {
        match self.kind {
            ValueKind::Ref(ptr, meta) => (ptr, meta),
            ValueKind::Val(_) | ValueKind::ValPair(_, _) => {
                use codegen::Place as _;
                let place = Place::new_stack(fx, self.layout.clone());

                place.clone().store(fx, self);

                (place.as_ptr(), None)
            }
        }
    }
}

impl<'ctx> codegen::Value for Value<'ctx> {
    type Backend = ClifBackend<'ctx>;
    type Raw = cir::Value;

    fn layout(&self) -> &TyLayout<Ty> {
        &self.layout
    }

    fn load_scalar(self, fx: &mut FunctionCtx<Self::Backend>) -> Self::Raw {
        match self.kind {
            ValueKind::Ref(ptr, None) => {
                let ty = fx.ir_type(&self.layout).unwrap();

                ptr.load(fx, ty, cir::MemFlags::new())
            }
            ValueKind::Val(val) => val,
            ValueKind::Ref(_, Some(_)) => unreachable!(),
            ValueKind::ValPair(_, _) => unreachable!(),
        }
    }

    fn load_scalar_pair(self, fx: &mut FunctionCtx<Self::Backend>) -> (Self::Raw, Self::Raw) {
        match self.kind {
            ValueKind::Ref(ptr, None) => {
                let (a, b) = match &self.layout.abi {
                    Abi::ScalarPair(a, b) => (a, b),
                    _ => unreachable!(),
                };

                let b_offset =
                    crate::place::scalar_pair_calculate_b_offset(&fx.db.target(fx.lib), a, b);
                let ty1 = fx.scalar_ty(a);
                let ty2 = fx.scalar_ty(b);
                let val1 = ptr.load(fx, ty1, cir::MemFlags::new());
                let val2 = ptr.offset(fx, b_offset).load(fx, ty2, cir::MemFlags::new());

                (val1, val2)
            }
            ValueKind::Ref(ptr, Some(meta)) => (ptr.get_addr(fx), meta),
            ValueKind::Val(_) => unreachable!(),
            ValueKind::ValPair(a, b) => (a, b),
        }
    }

    fn cast(self, _fx: &mut FunctionCtx<Self::Backend>, layout: TyLayout<Ty>) -> Self {
        Value {
            kind: self.kind,
            layout,
            _marker: PhantomData,
        }
    }

    fn field(self, fx: &mut FunctionCtx<Self::Backend>, idx: usize) -> Self {
        match self.kind {
            ValueKind::Val(_) => unimplemented!(),
            ValueKind::Ref(ptr, None) => {
                let (field_ptr, field_layout) = gen_field(fx, ptr, None, self.layout, idx);

                Value::new_ref(field_ptr, field_layout)
            }
            ValueKind::Ref(ptr, Some(meta)) => {
                let (field_ptr, field_layout) = gen_field(fx, ptr, Some(meta), self.layout, idx);

                Value::new_ref_meta(field_ptr, meta, field_layout)
            }
            ValueKind::ValPair(a, b) => {
                let field_layout = self.layout.field(fx.lib, fx.db.to_layout_db(), idx);

                if idx == 0 {
                    Value::new_val(a, field_layout)
                } else {
                    Value::new_val(b, field_layout)
                }
            }
        }
    }

    fn deref(self, fx: &mut FunctionCtx<Self::Backend>) -> Self {
        let pointee = self.layout.pointee(fx.lib, fx.db.to_layout_db());
        let ptr = self.load_scalar(fx);

        Value::new_val(ptr, pointee)
    }
}

fn gen_field<'tcx>(
    fx: &mut FunctionCtx<ClifBackend>,
    base: Pointer,
    meta: Option<cir::Value>,
    layout: TyLayout<Ty>,
    field: usize,
) -> (Pointer, TyLayout<Ty>) {
    let field_offset = layout.fields.offset(field);
    let field_layout = layout.field(fx.lib, fx.db.to_layout_db(), field);
    let simple = |fx: &mut FunctionCtx<'_, '_, ClifBackend>| {
        (
            base.offset_i64(fx, i64::try_from(field_offset.bytes()).unwrap()),
            field_layout.clone(),
        )
    };

    if let Some(_meta) = meta {
        if !field_layout.is_unsized() {
            return simple(fx);
        }

        match field_layout.ty {
            // Type::Str => simple(fx),
            _ => unimplemented!(),
        }
    } else {
        simple(fx)
    }
}
