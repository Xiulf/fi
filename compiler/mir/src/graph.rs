use std::cell::OnceCell;

use arena::{ArenaMap, Idx};
use smallvec::SmallVec;

pub use self::dominators::Dominators;
use crate::ir::{BasicBlocks, Block, BlockData, Terminator};
use crate::traversal::Postorder;

mod dominators;

pub type Predecessors = ArenaMap<Idx<BlockData>, SmallVec<[Block; 4]>>;

pub struct Successors<'a> {
    term: &'a Terminator,
    idx: usize,
}

#[derive(Default, Debug, Clone)]
pub struct Cache {
    predecessors: OnceCell<Predecessors>,
    postorder: OnceCell<Vec<Block>>,
}

impl BasicBlocks {
    pub fn successors(&self, block: Block) -> Successors {
        self.blocks[block.0].terminator.successors()
    }

    pub fn predecessors(&self) -> &Predecessors {
        self.cache.predecessors.get_or_init(|| {
            let mut preds = ArenaMap::from_iter(self.blocks.iter().map(|(idx, _)| (idx, SmallVec::new())));
            for (block, data) in self.blocks.iter() {
                for succ in data.terminator.successors() {
                    preds[succ.0].push(Block(block));
                }
            }
            preds
        })
    }

    pub fn postorder(&self) -> &[Block] {
        self.cache.postorder.get_or_init(|| {
            Postorder::new(&self.blocks, Block::ENTRY)
                .map(|(block, _)| block)
                .collect()
        })
    }

    pub fn dominators(&self) -> Dominators {
        dominators::dominators(self)
    }
}

impl Terminator {
    pub fn successors(&self) -> Successors {
        Successors { term: self, idx: 0 }
    }
}

impl Iterator for Successors<'_> {
    type Item = Block;

    fn next(&mut self) -> Option<Self::Item> {
        let block = match self.term {
            | Terminator::Jump(target) if self.idx == 0 => target.block,
            | Terminator::Switch { targets, .. } if self.idx < targets.len() => targets[self.idx].block,
            | _ => return None,
        };

        self.idx += 1;
        Some(block)
    }
}

impl PartialEq for Cache {
    fn eq(&self, _: &Self) -> bool {
        true
    }
}

impl Eq for Cache {
}

impl std::hash::Hash for Cache {
    fn hash<H: std::hash::Hasher>(&self, _: &mut H) {
    }
}
