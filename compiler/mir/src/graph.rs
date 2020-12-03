use crate::ir::*;
use index_vec::IndexVec;

pub struct Graph {
    pub root: Block,
    succ: IndexVec<Block, Vec<Block>>,
}

impl Graph {
    pub fn new(body: &Body) -> Self {
        let root = body.blocks.first().unwrap().id;
        let mut succ = IndexVec::with_capacity(body.blocks.len());

        for block in &body.blocks {
            match &block.term {
                Term::Jump(to) => {
                    succ.insert(block.id, vec![*to]);
                }
                Term::Switch(_, _, blocks) => {
                    succ.insert(block.id, blocks.to_vec());
                }
                Term::Call(_, _, _, to) => {
                    succ.insert(block.id, vec![*to]);
                }
                _ => {
                    succ.insert(block.id, Vec::new());
                }
            }
        }

        Graph { root, succ }
    }

    pub fn successors(&self, block: Block) -> impl Iterator<Item = Block> + '_ {
        self.succ[block].iter().copied()
    }
}
