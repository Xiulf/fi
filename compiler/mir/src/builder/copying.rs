use std::cell::OnceCell;

use arena::{ArenaMap, Idx};

use super::Builder;
use crate::ir::{Block, BlockData, Local, LocalData, Location, Operand, Place, RValue, Statement, Terminator};
use crate::repr::{needs_drop, ReprPos};
use crate::visitor::{PlaceContext, UseContext, Visitor};
use crate::{traversal, Db};

pub fn run_copy_analyzer(builder: &mut Builder, db: &dyn Db) {
    let mut analyzer = CopyAnalyzer::default();
    let mut rewriter = CopyRewriter::default();

    for (block, data) in traversal::reverse_postorder(&builder.blocks) {
        analyzer.visit_block(block, data);
    }

    rewriter.usage = analyzer.usage;
    rewriter.run(builder, db);
}

#[derive(Default, Debug)]
pub struct CopyAnalyzer {
    usage: ArenaMap<Idx<LocalData>, Vec<Usage>>,
    op_idx: Option<usize>,
}

#[derive(Default, Debug)]
pub struct CopyRewriter {
    usage: ArenaMap<Idx<LocalData>, Vec<Usage>>,
}

#[derive(Debug)]
struct Usage {
    loc: Location,
    op_idx: Option<usize>,
}

impl Visitor for CopyAnalyzer {
    fn visit_block(&mut self, block: Block, data: &BlockData) {
        for (i, stmt) in data.statements.iter().enumerate() {
            self.op_idx = None;
            self.visit_stmt(stmt, Location { block, statement: i });
        }

        self.op_idx = None;
        self.visit_terminator(&data.terminator, data.terminator_location(block));
    }

    fn visit_operand(&mut self, op: &Operand, loc: Location) {
        match self.op_idx {
            | None => self.op_idx = Some(0),
            | Some(i) => self.op_idx = Some(i + 1),
        }

        self.super_operand(op, loc);
    }

    fn visit_local(&mut self, local: &Local, ctx: PlaceContext, loc: Location) {
        if self.usage.get(local.0).is_none() {
            self.usage.insert(local.0, Vec::new());
        }

        let op_idx = if matches!(ctx, PlaceContext::Use(UseContext::Projection)) {
            None
        } else {
            self.op_idx
        };

        self.usage[local.0].push(Usage { loc, op_idx });
    }
}

impl CopyRewriter {
    fn run(self, builder: &mut Builder, db: &dyn Db) {
        let mut to_drop = Vec::new();
        let terminators = OnceCell::new();
        let terminators = || {
            terminators.get_or_init(|| {
                let mut res = Vec::new();
                for (block, data) in builder.blocks.iter() {
                    if data.is_terminator() {
                        res.push(Block(block));
                    }
                }
                res
            })
        };

        for (local, _) in builder.locals.iter() {
            if self.usage.get(local).is_none() {
                for &block in terminators() {
                    to_drop.push((block, Local(local)));
                }
            }
        }

        for (local, mut usages) in self.usage {
            let mut moved = None;

            while let Some(usage) = usages.pop() {
                if builder.blocks[usage.loc.block.0].is_terminator() && moved != Some(usage.loc.block) {
                    moved = None;
                }

                if let Some(op_idx) = usage.op_idx && moved.is_none() {
                    moved = Some(usage.loc.block);
                    Self::to_move(builder, usage.loc, op_idx);
                } else if moved.is_none() {
                    moved = Some(usage.loc.block);
                    to_drop.push((usage.loc.block, Local(local)));
                }
            }
        }

        for (block, local) in to_drop {
            if needs_drop(db, builder.locals[local.0].repr, ReprPos::Argument) {
                let block = &mut builder.blocks[block.0];
                block.statements.push(Statement::Drop(Place::new(local)));
            }
        }
    }

    fn to_move(builder: &mut Builder, loc: Location, idx: usize) {
        let block = &mut builder.blocks[loc.block.0];

        if loc.statement == block.statements.len() {
            Self::to_move_term(&mut block.terminator, idx);
        } else {
            Self::to_move_stmt(&mut block.statements[loc.statement], idx);
        }
    }

    fn to_move_op(op: &mut Operand) {
        if let Operand::Copy(place) = op {
            *op = Operand::Move(place.clone());
        }
    }

    fn to_move_term(term: &mut Terminator, mut idx: usize) {
        match term {
            | Terminator::Return(op) => Self::to_move_op(op),
            | Terminator::Jump(target) => Self::to_move_op(&mut target.args[idx]),
            | Terminator::Switch { discr, .. } if idx == 0 => Self::to_move_op(discr),
            | Terminator::Switch { targets, .. } => {
                for target in targets {
                    if idx >= target.args.len() {
                        idx -= target.args.len();
                        continue;
                    }

                    Self::to_move_op(&mut target.args[idx]);
                    break;
                }
            },
            | _ => {},
        }
    }

    fn to_move_stmt(stmt: &mut Statement, idx: usize) {
        match stmt {
            | Statement::Assign(_, rvalue) => match rvalue {
                | RValue::Use(op) => Self::to_move_op(op),
                | RValue::Cast(_, op) => Self::to_move_op(op),
                | RValue::BinOp(_, op, _) if idx == 0 => Self::to_move_op(op),
                | RValue::BinOp(_, _, op) => Self::to_move_op(op),
                | _ => {},
            },
            | Statement::Call { func, .. } if idx == 0 => Self::to_move_op(func),
            | Statement::Call { args, .. } => Self::to_move_op(&mut args[idx - 1]),
            | Statement::Intrinsic { args, .. } => Self::to_move_op(&mut args[idx]),
            | _ => {},
        }
    }
}
