use arena::ArenaMap;
use mir::syntax::{Local, Projection, Rvalue, Stmt};
use rustc_hash::FxHashSet;

use crate::ctx::BodyCtx;
use crate::layout::Abi;

enum LocalKind {
    ZST,
    Unused,
    Memory,
    SSA,
}

pub fn analyze(ctx: &BodyCtx<'_, '_, '_>) -> FxHashSet<Local> {
    let mut locals = ArenaMap::default();
    let mut set = FxHashSet::default();

    for (id, local) in ctx.body.locals.iter() {
        let layout = crate::layout::layout_of(ctx.db, &local.repr);
        let kind = if layout.is_zst() {
            LocalKind::ZST
        } else if let Abi::Scalar(_) | Abi::ScalarPair(_, _) = layout.abi {
            LocalKind::Unused
        } else {
            LocalKind::Memory
        };

        locals.insert(id, kind);
    }

    let mut assign = |local: Local| {
        let kind = &mut locals[local.0];
        match kind {
            | LocalKind::ZST => {},
            | LocalKind::Memory => {},
            | LocalKind::Unused => {
                *kind = LocalKind::SSA;
            },
            | LocalKind::SSA => {
                *kind = LocalKind::Memory;
            },
        }
    };

    for (_, block) in ctx.body.blocks.iter() {
        for &param in &block.params {
            assign(param);
        }

        for stmt in &block.stmts {
            if let Stmt::Assign(place, rvalue) = stmt {
                if place.projection.is_empty() {
                    assign(place.local);
                }

                if let Rvalue::Ref(place) = rvalue {
                    if !place.projection.iter().any(|e| matches!(e, Projection::Deref)) {
                        assign(place.local);
                        assign(place.local);
                    }
                }
            }
        }
    }

    for (local, kind) in locals.iter() {
        if let LocalKind::Memory = kind {
            set.insert(Local(local));
        }
    }

    set
}
