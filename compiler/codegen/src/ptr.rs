use crate::*;
use clif::ir::{self as cir, immediates::Offset32};
use clif::InstBuilder;
use clif::Module as _;
use mir::layout::Align;
use std::convert::TryFrom;

#[derive(Debug, Clone, Copy)]
pub(crate) struct Pointer {
    pub(crate) kind: PointerKind,
    pub(crate) offset: Offset32,
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum PointerKind {
    Addr(cir::Value),
    Stack(cir::StackSlot),
    Dangling(Align),
}

impl Pointer {
    pub(crate) fn addr(addr: cir::Value) -> Self {
        Pointer {
            kind: PointerKind::Addr(addr),
            offset: Offset32::new(0),
        }
    }

    pub(crate) fn stack(slot: cir::StackSlot) -> Self {
        Pointer {
            kind: PointerKind::Stack(slot),
            offset: Offset32::new(0),
        }
    }

    pub(crate) fn const_addr(fx: &mut FunctionCtx, addr: i64) -> Self {
        let ptr_type = fx.module.target_config().pointer_type();
        let addr = fx.bcx.ins().iconst(ptr_type, addr);

        Pointer {
            kind: PointerKind::Addr(addr),
            offset: Offset32::new(0),
        }
    }

    pub(crate) fn dangling(align: Align) -> Self {
        Pointer {
            kind: PointerKind::Dangling(align),
            offset: Offset32::new(0),
        }
    }

    pub(crate) fn get_addr(self, fx: &mut FunctionCtx) -> cir::Value {
        let ptr_type = fx.module.target_config().pointer_type();

        match self.kind {
            | PointerKind::Addr(addr) => {
                let offset: i64 = self.offset.into();

                if offset == 0 {
                    addr
                } else {
                    fx.bcx.ins().iadd_imm(addr, offset)
                }
            },
            | PointerKind::Stack(ss) => fx.bcx.ins().stack_addr(ptr_type, ss, self.offset),
            | PointerKind::Dangling(align) => fx.bcx.ins().iconst(ptr_type, i64::try_from(align.bytes()).unwrap()),
        }
    }

    pub(crate) fn offset(self, fx: &mut FunctionCtx, offset: Offset32) -> Self {
        self.offset_i64(fx, offset.into())
    }

    pub(crate) fn offset_i64(self, fx: &mut FunctionCtx, offset: i64) -> Self {
        if let Some(new_offset) = self.offset.try_add_i64(offset) {
            Pointer {
                kind: self.kind,
                offset: new_offset,
            }
        } else {
            let base_offset: i64 = self.offset.into();
            let ptr_type = fx.module.target_config().pointer_type();

            if let Some(new_offset) = base_offset.checked_add(offset) {
                let base_addr = match self.kind {
                    | PointerKind::Addr(addr) => addr,
                    | PointerKind::Stack(ss) => fx.bcx.ins().stack_addr(ptr_type, ss, 0),
                    | PointerKind::Dangling(align) => {
                        fx.bcx.ins().iconst(ptr_type, i64::try_from(align.bytes()).unwrap())
                    },
                };

                let addr = fx.bcx.ins().iadd_imm(base_addr, new_offset);

                Pointer {
                    kind: PointerKind::Addr(addr),
                    offset: Offset32::new(0),
                }
            } else {
                panic!("too large offset")
            }
        }
    }

    pub(crate) fn offset_value(self, fx: &mut FunctionCtx, offset: cir::Value) -> Self {
        let ptr_type = fx.module.target_config().pointer_type();

        match self.kind {
            | PointerKind::Addr(addr) => Pointer {
                kind: PointerKind::Addr(fx.bcx.ins().iadd(addr, offset)),
                offset: self.offset,
            },
            | PointerKind::Stack(slot) => {
                let addr = fx.bcx.ins().stack_addr(ptr_type, slot, self.offset);

                Pointer {
                    kind: PointerKind::Addr(fx.bcx.ins().iadd(addr, offset)),
                    offset: Offset32::new(0),
                }
            },
            | PointerKind::Dangling(align) => {
                let addr = fx.bcx.ins().iconst(ptr_type, i64::try_from(align.bytes()).unwrap());

                Pointer {
                    kind: PointerKind::Addr(fx.bcx.ins().iadd(addr, offset)),
                    offset: self.offset,
                }
            },
        }
    }

    pub(crate) fn load(self, fx: &mut FunctionCtx, ty: cir::Type, flags: cir::MemFlags) -> cir::Value {
        match self.kind {
            | PointerKind::Addr(addr) => fx.bcx.ins().load(ty, flags, addr, self.offset),
            | PointerKind::Stack(ss) => fx.bcx.ins().stack_load(ty, ss, self.offset),
            | PointerKind::Dangling(_) => unreachable!(),
        }
    }

    pub(crate) fn store(self, fx: &mut FunctionCtx, value: cir::Value, flags: cir::MemFlags) {
        match self.kind {
            | PointerKind::Addr(addr) => {
                fx.bcx.ins().store(flags, value, addr, self.offset);
            },
            | PointerKind::Stack(ss) => {
                fx.bcx.ins().stack_store(value, ss, self.offset);
            },
            | PointerKind::Dangling(_) => unreachable!(),
        }
    }
}
