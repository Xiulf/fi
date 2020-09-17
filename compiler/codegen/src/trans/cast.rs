use crate::{Backend, FunctionCtx};
use cranelift::codegen::ir::{self, InstBuilder};

impl<'a, 'tcx, B: Backend> FunctionCtx<'a, 'tcx, B> {
    pub fn trans_cast(
        &mut self,
        from: ir::Value,
        from_signed: bool,
        to_ty: ir::Type,
        to_signed: bool,
    ) -> ir::Value {
        let from_ty = self.builder.func.dfg.value_type(from);

        if from_ty.is_int() && to_ty.is_int() {
            if from_ty == to_ty {
                from
            } else if to_ty == ir::types::I128 {
                let lo = if from_ty == ir::types::I64 {
                    from
                } else if from_signed {
                    self.builder.ins().sextend(ir::types::I64, from)
                } else {
                    self.builder.ins().uextend(ir::types::I64, from)
                };

                let hi = if from_signed {
                    self.builder.ins().sshr_imm(lo, 64)
                } else {
                    self.builder.ins().iconst(ir::types::I64, 0)
                };

                self.builder.ins().iconcat(lo, hi)
            } else if to_ty.wider_or_equal(from_ty) {
                if from_signed {
                    self.builder.ins().sextend(to_ty, from)
                } else {
                    self.builder.ins().uextend(to_ty, from)
                }
            } else if from_ty == ir::types::I128 {
                let (lsb, _msb) = self.builder.ins().isplit(from);

                if to_ty == ir::types::I64 {
                    lsb
                } else {
                    self.builder.ins().ireduce(to_ty, lsb)
                }
            } else {
                self.builder.ins().ireduce(to_ty, from)
            }
        } else if from_ty.is_int() && to_ty.is_float() {
            if from_signed {
                self.builder.ins().fcvt_from_sint(to_ty, from)
            } else {
                self.builder.ins().fcvt_from_uint(to_ty, from)
            }
        } else if from_ty.is_float() && to_ty.is_int() {
            if to_ty == ir::types::I8 || to_ty == ir::types::I16 {
                let val = if to_signed {
                    self.builder.ins().fcvt_to_sint_sat(ir::types::I32, from)
                } else {
                    self.builder.ins().fcvt_to_uint_sat(ir::types::I32, from)
                };

                let (min, max) = if to_signed {
                    match to_ty {
                        ir::types::I8 => (-128, 127),
                        ir::types::I16 => (-32768, 32767),
                        _ => unreachable!(),
                    }
                } else {
                    match to_ty {
                        ir::types::I8 => (0, 255),
                        ir::types::I16 => (0, 65535),
                        _ => unreachable!(),
                    }
                };

                let min_val = self.builder.ins().iconst(ir::types::I32, min);
                let max_val = self.builder.ins().iconst(ir::types::I32, max);
                let val = if to_signed {
                    let has_underflow =
                        self.builder
                            .ins()
                            .icmp_imm(ir::condcodes::IntCC::SignedLessThan, val, min);
                    let has_overflow = self.builder.ins().icmp_imm(
                        ir::condcodes::IntCC::SignedGreaterThan,
                        val,
                        max,
                    );
                    let bottom_capped = self.builder.ins().select(has_underflow, min_val, val);

                    self.builder
                        .ins()
                        .select(has_overflow, max_val, bottom_capped)
                } else {
                    let has_overflow = self.builder.ins().icmp_imm(
                        ir::condcodes::IntCC::UnsignedGreaterThan,
                        val,
                        max,
                    );

                    self.builder.ins().select(has_overflow, max_val, val)
                };

                self.builder.ins().ireduce(to_ty, val)
            } else {
                if to_signed {
                    self.builder.ins().fcvt_to_sint_sat(to_ty, from)
                } else {
                    self.builder.ins().fcvt_to_uint_sat(to_ty, from)
                }
            }
        } else {
            match (from_ty, to_ty) {
                (ir::types::F32, ir::types::F64) => {
                    self.builder.ins().fpromote(ir::types::F64, from)
                }
                (ir::types::F64, ir::types::F32) => {
                    self.builder.ins().fdemote(ir::types::F32, from)
                }
                _ => from,
            }
        }
    }
}
