use crate::*;
use clif::InstBuilder;

impl FunctionCtx<'_, '_> {
    pub fn lower_intrinsic(&mut self, place: PlaceRef, name: &str, args: Vec<ValueRef>) -> Option<()> {
        let mut args = args.into_iter();
        let layout = place.layout.clone();

        match name {
            | "transmute" => {
                place.store(self, args.next()?);
            },
            | "memcpy" => {
                let dest = args.next()?.load_scalar(self);
                let src = args.next()?.load_scalar(self);
                let size = args.next()?.load_scalar(self);

                self.bcx.call_memcpy(self.module.target_config(), dest, src, size);
            },
            | "alloc" => {
                let size = args.next()?.load_scalar(self);
                let val = self.call_malloc(size);

                place.store(self, ValueRef::new_val(val, layout));
            },
            | "add_i32" => {
                let lhs = args.next()?.load_scalar(self);
                let rhs = args.next()?.load_scalar(self);
                let val = self.bcx.ins().iadd(lhs, rhs);

                place.store(self, ValueRef::new_val(val, layout));
            },
            | "sub_i32" => {
                let lhs = args.next()?.load_scalar(self);
                let rhs = args.next()?.load_scalar(self);
                let val = self.bcx.ins().isub(lhs, rhs);

                place.store(self, ValueRef::new_val(val, layout));
            },
            | "eq_i32" => {
                let lhs = args.next()?.load_scalar(self);
                let rhs = args.next()?.load_scalar(self);
                let val = self.bcx.ins().icmp(clif::IntCC::Equal, lhs, rhs);
                let val = self.bcx.ins().bint(clif::types::I8, val);

                place.store(self, ValueRef::new_val(val, layout));
            },
            | "u16_to_u8" | "u32_to_u8" | "u64_to_u8" | "u128_to_u8" | "i16_to_u8" | "i32_to_u8" | "i64_to_u8"
            | "i128_to_u8" | "u16_to_i8" | "u32_to_i8" | "u64_to_i8" | "u128_to_i8" | "i16_to_i8" | "i32_to_i8"
            | "i64_to_i8" | "i128_to_i8" => {
                let val = args.next()?.load_scalar(self);
                let val = self.bcx.ins().ireduce(clif::types::I8, val);

                place.store(self, ValueRef::new_val(val, layout));
            },
            | _ => panic!("unknown intrinsic '{}'", name),
        }

        Some(())
    }
}
