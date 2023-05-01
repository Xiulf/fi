use arena::{ArenaMap, Idx};
use mir::graph::Dominators;
use mir::ir::{Local, LocalData, Location, Place, PlaceRef, Projection, RValue};
use mir::traversal;
use mir::visitor::{MutUseContext, PlaceContext, UseContext, Visitor};
use rustc_hash::FxHashSet;

use crate::ctx::BodyCtx;
use crate::layout::{repr_and_layout, Abi};

#[derive(Debug)]
enum LocalKind {
    ZST,
    Unused,
    Memory,
    SSA(DefLocation),
}

struct LocalAnalyzer<'a, 'b, 'c, 'ctx> {
    bcx: &'c BodyCtx<'a, 'b, 'ctx>,
    dominators: Dominators,
    locals: ArenaMap<Idx<LocalData>, LocalKind>,
}

#[derive(Debug)]
enum DefLocation {
    Arg,
    Body(Location),
}

pub fn analyze(bcx: &BodyCtx<'_, '_, '_>) -> FxHashSet<Local> {
    let dominators = bcx.body.blocks.dominators();
    let mut locals = ArenaMap::default();

    for (id, local) in bcx.body.locals.iter() {
        let repr = bcx.instance.subst_repr(bcx.db, local.repr);
        let layout = repr_and_layout(bcx.db, repr);
        let kind = if layout.is_zst() {
            LocalKind::ZST
        } else if let Abi::Scalar(_) | Abi::ScalarPair(_, _) = layout.abi {
            LocalKind::Unused
        } else {
            LocalKind::Memory
        };

        locals.insert(id, kind);
    }

    let mut analyzer = LocalAnalyzer {
        bcx,
        dominators,
        locals,
    };

    for (block, data) in traversal::reverse_postorder(bcx.body.blocks) {
        for &arg in data.params.iter() {
            analyzer.assign(arg, DefLocation::Arg);
        }

        analyzer.visit_block(block, data);
    }

    analyzer
        .locals
        .iter()
        .filter(|(_, kind)| matches!(kind, LocalKind::Memory))
        .map(|(local, _)| Local(local))
        .collect()
}

impl DefLocation {
    fn dominates(&self, loc: Location, dominators: &Dominators) -> bool {
        match self {
            | Self::Arg => true,
            | Self::Body(def) => def.next_stmt().dominates(loc, dominators),
        }
    }
}

impl LocalAnalyzer<'_, '_, '_, '_> {
    fn assign(&mut self, local: Local, loc: DefLocation) {
        let kind = &mut self.locals[local.0];
        match kind {
            | LocalKind::ZST => {},
            | LocalKind::Memory => {},
            | LocalKind::Unused => *kind = LocalKind::SSA(loc),
            | LocalKind::SSA(_) => *kind = LocalKind::Memory,
        }
    }

    fn process_place(&mut self, place: PlaceRef, ctx: PlaceContext, loc: Location) {
        if let Some((place_base, elem)) = place.last_projection() {
            let mut base_ctx = if matches!(ctx, PlaceContext::MutUse(_)) {
                PlaceContext::MutUse(MutUseContext::Projection)
            } else {
                PlaceContext::Use(UseContext::Projection)
            };

            if matches!(ctx, PlaceContext::Use(UseContext::Move | UseContext::Copy)) {
                let elem_lyt = self.bcx.place_ref_layout(place);

                if elem_lyt.is_zst() {
                    return;
                }
            }

            if let Projection::Deref = elem {
                base_ctx = PlaceContext::Use(UseContext::Copy);
            }

            self.process_place(place_base, base_ctx, loc);
        } else {
            self.visit_local(&place.local, ctx, loc);
        }
    }
}

impl Visitor for LocalAnalyzer<'_, '_, '_, '_> {
    fn visit_assign(&mut self, place: &Place, rvalue: &RValue, loc: Location) {
        if place.projection.is_empty() {
            self.assign(place.local, DefLocation::Body(loc));

            if !matches!(self.locals[place.local.0], LocalKind::Memory) {
                if !self.bcx.rvalue_creates_operand(place, rvalue) {
                    self.locals[place.local.0] = LocalKind::Memory;
                }
            }
        } else {
            self.visit_place(place, PlaceContext::MutUse(MutUseContext::Store), loc);
        }

        self.visit_rvalue(rvalue, loc);
    }

    fn visit_place(&mut self, place: &Place, ctx: PlaceContext, loc: Location) {
        self.process_place(place.as_ref(), ctx, loc);
    }

    fn visit_local(&mut self, &local: &Local, ctx: PlaceContext, loc: Location) {
        // tracing::info!("{}: {:?}, {:?}, {:?}", local, ctx, loc, self.locals[local.0]);
        match ctx {
            | PlaceContext::MutUse(MutUseContext::Call) | PlaceContext::MutUse(MutUseContext::Init) => {
                self.assign(local, DefLocation::Body(loc));
            },
            | PlaceContext::NonUse(_) => {},
            | PlaceContext::Use(UseContext::Copy | UseContext::Move) => match &mut self.locals[local.0] {
                | LocalKind::ZST | LocalKind::Memory => {},
                | LocalKind::SSA(def) if def.dominates(loc, &self.dominators) => {},
                | kind @ (LocalKind::Unused | LocalKind::SSA(_)) => {
                    *kind = LocalKind::Memory;
                },
            },
            | PlaceContext::MutUse(MutUseContext::Drop) => {
                let kind = &mut self.locals[local.0];
                if !matches!(*kind, LocalKind::Memory) {
                    let _repr = &self.bcx.body.locals[local.0].repr;
                    // TODO: check for drop
                }
            },
            | _ => {
                self.locals[local.0] = LocalKind::Memory;
            },
        }
    }
}
