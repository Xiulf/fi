use arena::{ArenaMap, Idx};

use super::Builder;
use crate::ir::{LocalData, Location, Operand, Place, Statement};
use crate::traversal;
use crate::visitor::{MutUseContext, PlaceContext, Visitor};

pub fn run_copy_analyzer(builder: &mut Builder) {
    let mut analyzer = CopyAnalyzer::default();
    let mut rewriter = CopyRewriter::default();

    for (block, data) in traversal::reverse_postorder(&builder.blocks) {
        analyzer.visit_block(block, data);
    }

    for (_, mut locs) in analyzer.locals {
        if let Some((ptr, loc, to_move)) = locs.pop() {
            rewriter.to_move.push((ptr, loc, to_move));
        }

        for (ptr, loc, to_move) in locs {
            if builder.blocks[loc.block.0].is_terminator() {
                rewriter.to_move.push((ptr, loc, to_move));
            }
        }
    }

    rewriter.rewrite(builder);
}

#[derive(Default, Debug)]
pub struct CopyAnalyzer {
    locals: ArenaMap<Idx<LocalData>, Vec<(*const Operand, Location, bool)>>,
}

#[derive(Default, Debug)]
pub struct CopyRewriter {
    to_move: Vec<(*const Operand, Location, bool)>,
}

impl Visitor for CopyAnalyzer {
    fn visit_operand(&mut self, op: &Operand, loc: Location) {
        if let Operand::Copy(place) = op {
            if self.locals.get(place.local.0).is_none() {
                self.locals.insert(place.local.0, Vec::new());
            }

            let locs = &mut self.locals[place.local.0];

            if let Some(idx) = locs.iter().position(|(ptr, _, _)| *ptr == op) {
                let loc = locs.remove(idx);
                locs.push(loc);
            } else {
                let to_move = place.projection.is_empty();
                self.locals[place.local.0].push((op, loc, to_move));
            }
        }
    }

    fn visit_place(&mut self, place: &Place, ctx: PlaceContext, _loc: Location) {
        if let PlaceContext::MutUse(MutUseContext::Store) = ctx {
            if let Some(locs) = self.locals.get_mut(place.local.0) {
                if let Some(loc) = locs.iter_mut().rfind(|l| l.2) {
                    loc.2 = false;
                }
            }
        }
    }
}

impl CopyRewriter {
    fn rewrite(self, builder: &mut Builder) {
        let mut to_drop = Vec::new();

        for (ptr, loc, to_move) in self.to_move {
            let op = unsafe { &mut *(ptr as *mut Operand) };

            if let Operand::Copy(place) = op {
                if to_move {
                    *op = Operand::Move(place.clone());
                } else {
                    to_drop.push((place.local, loc));
                }
            }
        }

        for (local, loc) in to_drop {
            builder.blocks.arena[loc.block.0]
                .statements
                .push(Statement::Drop(Place::new(local)));
        }
    }
}
