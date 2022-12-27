use arena::{ArenaMap, Idx};
use mir::db::MirDatabase;
use mir::layout::Abi;
use mir::syntax::{Block, Local, LocalData, Location, Place, Rvalue};
use mir::visitor::{MutUseContext, PlaceContext, UseContext, Visitor};
use rustc_hash::FxHashSet;

use crate::ctx::BodyCtx;

enum LocalKind {
    ZST,
    Unused,
    Memory,
    SSA(Location),
}

struct LocalAnalyzer<'db> {
    _db: &'db dyn MirDatabase,
    locals: ArenaMap<Idx<LocalData>, LocalKind>,
}

pub fn analyze(ctx: &BodyCtx<'_, '_, '_>) -> FxHashSet<Local> {
    let mut locals = ArenaMap::default();
    let mut set = FxHashSet::default();

    for (id, local) in ctx.body.locals.iter() {
        let repr = ctx.instance.subst_repr(ctx.db, &local.repr);
        let layout = ctx.db.layout_of(repr);
        let kind = if layout.is_zst() {
            LocalKind::ZST
        } else if let Abi::Scalar(_) | Abi::ScalarPair(_, _) = layout.abi {
            LocalKind::Unused
        } else {
            LocalKind::Memory
        };

        locals.insert(id, kind);
    }

    let mut analyzer = LocalAnalyzer { _db: ctx.db, locals };

    for &arg in &ctx.body.blocks[Block::ENTRY.0].params {
        analyzer.assign(arg, Block::ENTRY.start_location());
    }

    for (block, data) in ctx.body.blocks.iter() {
        analyzer.visit_block(Block(block), data);
    }

    for (local, kind) in analyzer.locals.iter() {
        if matches!(kind, LocalKind::Memory) {
            set.insert(Local(local));
        }
    }

    set
}

impl<'db> LocalAnalyzer<'db> {
    fn assign(&mut self, local: Local, loc: Location) {
        let kind = &mut self.locals[local.0];
        match kind {
            | LocalKind::ZST => {},
            | LocalKind::Memory => {},
            | LocalKind::Unused => {
                *kind = LocalKind::SSA(loc);
            },
            | LocalKind::SSA(_) => {
                *kind = LocalKind::Memory;
            },
        }
    }
}

impl<'db> Visitor for LocalAnalyzer<'db> {
    fn visit_assign(&mut self, place: &Place, rvalue: &Rvalue, loc: Location) {
        if place.projection.is_empty() {
            self.assign(place.local, loc);

            if !matches!(self.locals[place.local.0], LocalKind::Memory) {
                self.locals[place.local.0] = LocalKind::Memory;
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
