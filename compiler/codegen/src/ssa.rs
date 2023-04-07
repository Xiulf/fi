use arena::{ArenaMap, Idx};
use mir::ir::{Block, Local, LocalData, Location, Place, RValue};
use mir::visitor::{MutUseContext, PlaceContext, UseContext, Visitor};
use rustc_hash::FxHashSet;

use crate::ctx::BodyCtx;
use crate::layout::{repr_and_layout, Abi};

#[derive(Debug)]
enum LocalKind {
    ZST,
    Unused,
    Memory,
    SSA(Location),
}

struct LocalAnalyzer<'a, 'b, 'c, 'ctx> {
    bcx: &'c BodyCtx<'a, 'b, 'ctx>,
    locals: ArenaMap<Idx<LocalData>, LocalKind>,
}

pub fn analyze(bcx: &BodyCtx<'_, '_, '_>) -> FxHashSet<Local> {
    let mut locals = ArenaMap::default();

    for (id, local) in bcx.body.locals.iter() {
        let repr = bcx.instance.subst_repr(bcx.db, &local.repr);
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

    let mut analyzer = LocalAnalyzer { bcx, locals };

    for (block, data) in bcx.body.blocks.iter() {
        for &arg in data.params.iter() {
            analyzer.assign(arg, Block(block).start_location());
        }

        analyzer.visit_block(Block(block), data);
    }

    analyzer
        .locals
        .iter()
        .filter(|(_, kind)| matches!(kind, LocalKind::Memory))
        .map(|(local, _)| Local(local))
        .collect()
}

impl LocalAnalyzer<'_, '_, '_, '_> {
    fn assign(&mut self, local: Local, loc: Location) {
        let kind = &mut self.locals[local.0];
        match kind {
            | LocalKind::ZST => {},
            | LocalKind::Memory => {},
            | LocalKind::Unused => *kind = LocalKind::SSA(loc),
            | LocalKind::SSA(_) => *kind = LocalKind::Memory,
        }
    }
}

impl Visitor for LocalAnalyzer<'_, '_, '_, '_> {
    fn visit_assign(&mut self, place: &Place, rvalue: &RValue, loc: Location) {
        if place.projection.is_empty() {
            self.assign(place.local, loc);

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

    fn visit_local(&mut self, &local: &Local, ctx: PlaceContext, loc: Location) {
        match ctx {
            | PlaceContext::MutUse(MutUseContext::Call) => {
                self.assign(local, loc);
            },
            | PlaceContext::NonUse(_) => {},
            | PlaceContext::Use(UseContext::Copy | UseContext::Move) => match &mut self.locals[local.0] {
                | LocalKind::ZST | LocalKind::Memory => {},
                | LocalKind::SSA(_) if true => {},
                | kind @ (LocalKind::Unused | LocalKind::SSA(_)) => {
                    *kind = LocalKind::Memory;
                },
            },
            | _ => {
                self.locals[local.0] = LocalKind::Memory;
            },
        }
    }
}
