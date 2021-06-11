use super::*;
use hir::arena::ArenaMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum SsaKind {
    NotSsa,
    Ssa,
}

pub(super) fn analyze(fx: &FunctionCtx) -> ArenaMap<ir::LocalId, SsaKind> {
    let mut map = ArenaMap::default();

    /* for (id, local) in fx.body.locals.iter() {
        let layout = fx.db.layout_of(local.ty.clone());

        if fx.ir_type(&layout).is_some() || fx.ir_pair_type(&layout).is_some() {
            map.insert(id, SsaKind::Ssa);
        } else {
            map.insert(id, SsaKind::NotSsa);
        }
    }

    for (_, block) in fx.body.blocks.iter() {
        for stmt in &block.stmts {
            if let ir::Stmt::Assign(_, rvalue) = stmt {
                if let ir::RValue::AddrOf(place) = rvalue {
                    if !place.elems.iter().any(|e| matches!(e, ir::PlaceElem::Deref)) {
                        map.insert(place.local, SsaKind::NotSsa);
                    }
                }
            } else if let ir::Stmt::Call(ret, ..) = stmt {
                let ret_lyt = fx.db.layout_of(fx.body.locals[ret.local].ty.clone());
                let mode = fx.pass_mode(&ret_lyt);

                if let abi::PassMode::ByRef { .. } = mode {
                    map.insert(ret.local, SsaKind::NotSsa);
                }
            }
        }
    } */

    map
}
