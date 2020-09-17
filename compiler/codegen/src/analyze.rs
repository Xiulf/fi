use crate::FunctionCtx;
use cranelift_module::Backend;
use std::collections::HashMap;

#[derive(Debug, PartialEq)]
pub enum SsaKind {
    Ssa,
    NotSsa,
}

pub fn analyze(fx: &FunctionCtx<impl Backend>) -> HashMap<mir::LocalId, SsaKind> {
    let mut map = fx
        .body
        .locals
        .iter()
        .map(|(id, local)| {
            let layout = fx.tcx.layout(local.ty);

            if fx.clif_type(layout).is_some() || fx.clif_pair_type(layout).is_some() {
                (*id, SsaKind::Ssa)
            } else {
                (*id, SsaKind::NotSsa)
            }
        })
        .collect::<HashMap<_, _>>();

    for block in fx.body.blocks.values() {
        for stmt in &block.stmts {
            match stmt {
                mir::Stmt::Assign(_, rvalue) => match rvalue {
                    mir::RValue::Ref(place) => {
                        if let mir::PlaceBase::Local(local) = &place.base {
                            map.insert(*local, SsaKind::NotSsa);
                        }
                    }
                    _ => {}
                },
                _ => {}
            }
        }
    }

    map
}
