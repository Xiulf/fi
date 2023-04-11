use arena::Arena;
use rustc_hash::FxHashSet;

use crate::graph::Successors;
use crate::ir::{BasicBlocks, Block, BlockData};

pub fn postorder(blocks: &BasicBlocks) -> Postorder {
    Postorder::new(&blocks.blocks, Block::ENTRY)
}

pub fn reverse_postorder(block_data: &BasicBlocks) -> ReversePostorderIter {
    let blocks = block_data.postorder();
    let len = blocks.len();

    ReversePostorderIter {
        block_data,
        blocks,
        idx: len,
    }
}

pub struct Postorder<'a> {
    blocks: &'a Arena<BlockData>,
    visited: FxHashSet<Block>,
    visit_stack: Vec<(Block, Successors<'a>)>,
    root_is_start_block: bool,
}

impl<'a> Postorder<'a> {
    pub fn new(blocks: &'a Arena<BlockData>, root: Block) -> Self {
        let mut po = Self {
            blocks,
            visited: FxHashSet::default(),
            visit_stack: Vec::new(),
            root_is_start_block: root == Block::ENTRY,
        };

        let data = &po.blocks[root.0];

        po.visited.insert(root);
        po.visit_stack.push((root, data.terminator.successors()));
        po.traverse_successor();
        po
    }

    fn traverse_successor(&mut self) {
        while let Some(&mut (_, ref mut iter)) = self.visit_stack.last_mut() && let Some(block) = iter.next() {
            if self.visited.insert(block) {
                let term = &self.blocks[block.0].terminator;
                self.visit_stack.push((block, term.successors()));
            }
        }
    }
}

impl<'a> Iterator for Postorder<'a> {
    type Item = (Block, &'a BlockData);

    fn next(&mut self) -> Option<Self::Item> {
        let next = self.visit_stack.pop();
        if next.is_some() {
            self.traverse_successor();
        }

        next.map(|(block, _)| (block, &self.blocks[block.0]))
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let upper = self.blocks.len() - self.visited.len();
        let lower = if self.root_is_start_block {
            upper
        } else {
            self.visit_stack.len()
        };

        (lower, Some(upper))
    }
}

#[derive(Clone)]
pub struct ReversePostorderIter<'a> {
    block_data: &'a Arena<BlockData>,
    blocks: &'a [Block],
    idx: usize,
}

impl<'a> Iterator for ReversePostorderIter<'a> {
    type Item = (Block, &'a BlockData);

    fn next(&mut self) -> Option<Self::Item> {
        if self.idx == 0 {
            return None;
        }

        self.idx -= 1;
        self.blocks
            .get(self.idx)
            .map(|&block| (block, &self.block_data[block.0]))
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.idx, Some(self.idx))
    }
}

impl ExactSizeIterator for ReversePostorderIter<'_> {
}
