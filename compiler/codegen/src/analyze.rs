use super::*;
use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SsaKind {
    Ssa,
    NotSsa,
}

pub fn analyze(fx: &FunctionCtx<impl Backend>) -> HashMap<mir::Local, SsaKind> {
    let mut map = fx
        .body
        .locals
        .iter()
        .map(|data| {
            let layout = fx.db.layout_of(fx.lib, data.ty.clone());

            if fx.ir_type(&layout).is_some() || fx.ir_pair_type(&layout).is_some() {
                (data.id, SsaKind::Ssa)
            } else {
                (data.id, SsaKind::NotSsa)
            }
        })
        .collect::<HashMap<_, _>>();

    for block in &fx.body.blocks {
        for stmt in &block.stmts {
            if let mir::Stmt::Assign(_, rvalue) = stmt {
                if let mir::RValue::AddrOf(place) = rvalue {
                    if let mir::PlaceBase::Local(local) = &place.base {
                        map.insert(*local, SsaKind::NotSsa);
                    }
                }
            }
        }

        if let mir::Term::Call(place, ..) = &block.term {
            if let mir::PlaceBase::Local(local) = &place.base {
                let dest_layout = fx.db.layout_of(fx.lib, fx.body.locals[*local].ty.clone());

                if !abi::can_return_to_ssa_var(fx, &dest_layout) {
                    map.insert(*local, SsaKind::NotSsa);
                }
            }
        }
    }

    map
}
