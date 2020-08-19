use crate::FunctionCtx;
use check::layout::Align;
use cranelift::codegen::ir::{self as cir, immediates::Offset32, InstBuilder};
use cranelift_module::Backend;
use std::convert::TryFrom;

#[derive(Clone, Copy)]
pub struct Pointer {
    pub kind: PointerKind,
    pub offset: Offset32,
}

#[derive(Clone, Copy)]
pub enum PointerKind {
    Addr(cir::Value),
    Stack(cir::StackSlot),
    Dangling(Align),
}

impl Pointer {
    pub fn addr(addr: cir::Value) -> Self {
        Pointer {
            kind: PointerKind::Addr(addr),
            offset: Offset32::new(0),
        }
    }

    pub fn stack(slot: cir::StackSlot) -> Self {
        Pointer {
            kind: PointerKind::Stack(slot),
            offset: Offset32::new(0),
        }
    }

    pub fn const_addr(fx: &mut FunctionCtx<impl Backend>, addr: i64) -> Self {
        let addr = fx.builder.ins().iconst(fx.pointer_type, addr);

        Pointer {
            kind: PointerKind::Addr(addr),
            offset: Offset32::new(0),
        }
    }

    pub fn dangling(align: Align) -> Self {
        Pointer {
            kind: PointerKind::Dangling(align),
            offset: Offset32::new(0),
        }
    }

    pub fn get_addr(self, fx: &mut FunctionCtx<impl Backend>) -> cir::Value {
        match self.kind {
            PointerKind::Addr(addr) => {
                let offset: i64 = self.offset.into();

                if offset == 0 {
                    addr
                } else {
                    fx.builder.ins().iadd_imm(addr, offset)
                }
            }
            PointerKind::Stack(slot) => {
                fx.builder
                    .ins()
                    .stack_addr(fx.pointer_type, slot, self.offset)
            }
            PointerKind::Dangling(align) => fx
                .builder
                .ins()
                .iconst(fx.pointer_type, i64::try_from(align.bytes()).unwrap()),
        }
    }

    /* pub fn try_get_addr_and_offset(self) -> Option<(cir::Value, Offset32)> {{{{
        match self.kind {
            PointerKind::Addr(addr) => Some((addr, self.offset)),
            PointerKind::Stack(_) => None,
        }
    } *///}}}

    pub fn offset(self, fx: &mut FunctionCtx<impl Backend>, extra_offset: Offset32) -> Self {
        self.offset_i64(fx, extra_offset.into())
    }

    pub fn offset_i64(self, fx: &mut FunctionCtx<impl Backend>, extra_offset: i64) -> Self {
        if let Some(new_offset) = self.offset.try_add_i64(extra_offset) {
            Pointer {
                kind: self.kind,
                offset: new_offset,
            }
        } else {
            let base_offset: i64 = self.offset.into();

            if let Some(new_offset) = base_offset.checked_add(extra_offset) {
                let base_addr = match self.kind {
                    PointerKind::Addr(addr) => addr,
                    PointerKind::Stack(slot) => {
                        fx.builder.ins().stack_addr(fx.pointer_type, slot, 0)
                    }
                    PointerKind::Dangling(align) => fx
                        .builder
                        .ins()
                        .iconst(fx.pointer_type, i64::try_from(align.bytes()).unwrap()),
                };

                let addr = fx.builder.ins().iadd_imm(base_addr, new_offset);

                Pointer {
                    kind: PointerKind::Addr(addr),
                    offset: Offset32::new(0),
                }
            } else {
                panic!(
                    "self.offset ({}) + extra_offset ({}) not representable in i64",
                    base_offset, extra_offset
                );
            }
        }
    }

    pub fn offset_value(
        self,
        fx: &mut FunctionCtx<impl Backend>,
        extra_offset: cir::Value,
    ) -> Self {
        match self.kind {
            PointerKind::Addr(addr) => Pointer {
                kind: PointerKind::Addr(fx.builder.ins().iadd(addr, extra_offset)),
                offset: self.offset,
            },
            PointerKind::Stack(slot) => {
                let addr = fx
                    .builder
                    .ins()
                    .stack_addr(fx.pointer_type, slot, self.offset);

                Pointer {
                    kind: PointerKind::Addr(fx.builder.ins().iadd(addr, extra_offset)),
                    offset: Offset32::new(0),
                }
            }
            PointerKind::Dangling(align) => {
                let addr = fx
                    .builder
                    .ins()
                    .iconst(fx.pointer_type, i64::try_from(align.bytes()).unwrap());

                Pointer {
                    kind: PointerKind::Addr(fx.builder.ins().iadd(addr, extra_offset)),
                    offset: self.offset,
                }
            }
        }
    }

    pub fn load(
        self,
        fx: &mut FunctionCtx<impl Backend>,
        ty: cir::Type,
        flags: cir::MemFlags,
    ) -> cir::Value {
        match self.kind {
            PointerKind::Addr(addr) => fx.builder.ins().load(ty, flags, addr, self.offset),
            PointerKind::Stack(slot) => fx.builder.ins().stack_load(ty, slot, self.offset),
            PointerKind::Dangling(_) => unreachable!(),
        }
    }

    pub fn store(
        self,
        fx: &mut FunctionCtx<impl Backend>,
        value: cir::Value,
        flags: cir::MemFlags,
    ) {
        match self.kind {
            PointerKind::Addr(addr) => {
                fx.builder.ins().store(flags, value, addr, self.offset);
            }
            PointerKind::Stack(slot) => {
                fx.builder.ins().stack_store(value, slot, self.offset);
            }
            PointerKind::Dangling(_) => unreachable!(),
        }
    }
}
