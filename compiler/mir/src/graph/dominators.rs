use index_vec::{IndexSlice, IndexVec};

use super::*;

#[derive(Debug, Clone)]
pub struct Dominators {
    post_order_rank: ArenaMap<Idx<BlockData>, usize>,
    immediate_dominators: ArenaMap<Idx<BlockData>, Block>,
}

pub struct Iter<'a> {
    dominators: &'a Dominators,
    block: Option<Block>,
}

impl Dominators {
    pub fn is_reachable(&self, block: Block) -> bool {
        self.immediate_dominators.get(block.0).is_some()
    }

    pub fn immediate_dominator(&self, block: Block) -> Block {
        assert!(self.is_reachable(block));
        self.immediate_dominators[block.0]
    }

    pub fn dominators(&self, block: Block) -> Iter {
        assert!(self.is_reachable(block));
        Iter {
            dominators: self,
            block: Some(block),
        }
    }

    pub fn dominates(&self, dom: Block, block: Block) -> bool {
        self.dominators(block).any(|b| b == dom)
    }

    pub fn rank_partial_cmp(&self, lhs: Block, rhs: Block) -> Option<std::cmp::Ordering> {
        self.post_order_rank[rhs.0].partial_cmp(&self.post_order_rank[lhs.0])
    }
}

impl Iterator for Iter<'_> {
    type Item = Block;

    fn next(&mut self) -> Option<Self::Item> {
        let block = self.block?;
        let dom = self.dominators.immediate_dominator(block);
        self.block = if dom == block { None } else { Some(dom) };
        Some(block)
    }
}

index_vec::define_index_type! {
    struct PreorderIndex = u32;
}

struct PreOrderFrame<'a> {
    pre_order_idx: PreorderIndex,
    iter: Successors<'a>,
}

pub fn dominators(graph: &BasicBlocks) -> Dominators {
    let mut post_order_rank = ArenaMap::from_iter(graph.iter().map(|(idx, _)| (idx, 0)));
    let mut parent = IndexVec::<PreorderIndex, PreorderIndex>::with_capacity(graph.len());
    let mut stack = vec![PreOrderFrame {
        pre_order_idx: PreorderIndex::new(0),
        iter: graph.successors(Block::ENTRY),
    }];
    let mut pre_order_to_real = IndexVec::<PreorderIndex, Block>::with_capacity(graph.len());
    let mut real_to_pre_order = ArenaMap::<Idx<BlockData>, PreorderIndex>::default();

    pre_order_to_real.push(Block::ENTRY);
    parent.push(PreorderIndex::new(0));
    real_to_pre_order.insert(Block::ENTRY.0, PreorderIndex::new(0));

    let mut post_order_idx = 0;

    'recurse: while let Some(frame) = stack.last_mut() {
        while let Some(successor) = frame.iter.next() {
            if real_to_pre_order.get(successor.0).is_none() {
                let pre_order_idx = pre_order_to_real.push(successor);
                real_to_pre_order.insert(successor.0, pre_order_idx);
                parent.push(frame.pre_order_idx);
                stack.push(PreOrderFrame {
                    pre_order_idx,
                    iter: graph.successors(successor),
                });

                continue 'recurse;
            }
        }

        post_order_rank.insert(pre_order_to_real[frame.pre_order_idx].0, post_order_idx);
        post_order_idx += 1;
        stack.pop().unwrap();
    }

    let reachable_vertices = pre_order_to_real.len();
    let mut idom = IndexVec::with_capacity(reachable_vertices);
    let mut semi = IndexVec::from_iter((0..reachable_vertices).map(PreorderIndex::new));
    let mut label = semi.clone();
    let mut bucket = IndexVec::with_capacity(reachable_vertices);
    let mut last_linked = None;

    idom.resize(reachable_vertices, PreorderIndex::new(0));
    bucket.resize(reachable_vertices, Vec::new());

    for w in (1..reachable_vertices).rev() {
        let w = PreorderIndex::new(w);
        let z = parent[w];

        for &v in bucket[z].iter() {
            let y = eval(&mut parent, last_linked, &semi, &mut label, v);
            idom[v] = if semi[y] < z { y } else { z };
        }

        semi[w] = w;

        for v in graph.successors(pre_order_to_real[w]) {
            let Some(v) = real_to_pre_order.get(v.0) else {
                continue;
            };

            let x = eval(&mut parent, last_linked, &semi, &mut label, *v);
            semi[w] = std::cmp::min(semi[w], semi[x]);
        }

        if parent[w] != semi[w] {
            bucket[semi[w]].push(w);
        } else {
            idom[w] = parent[w];
        }

        last_linked = Some(w);
    }

    for w in 1..reachable_vertices {
        let w = PreorderIndex::new(w);
        if idom[w] != semi[w] {
            idom[w] = idom[idom[w]];
        }
    }

    let mut immediate_dominators = ArenaMap::default();

    for (idx, node) in pre_order_to_real.iter_enumerated() {
        immediate_dominators.insert(node.0, pre_order_to_real[idom[idx]]);
    }

    Dominators {
        immediate_dominators,
        post_order_rank,
    }
}

#[inline]
fn eval(
    ancestor: &mut IndexSlice<PreorderIndex, [PreorderIndex]>,
    last_linked: Option<PreorderIndex>,
    semi: &IndexSlice<PreorderIndex, [PreorderIndex]>,
    label: &mut IndexSlice<PreorderIndex, [PreorderIndex]>,
    node: PreorderIndex,
) -> PreorderIndex {
    if is_processed(node, last_linked) {
        compress(ancestor, last_linked, semi, label, node);
        label[node]
    } else {
        node
    }
}

#[inline]
fn is_processed(v: PreorderIndex, last_linked: Option<PreorderIndex>) -> bool {
    if let Some(ll) = last_linked {
        v >= ll
    } else {
        false
    }
}

#[inline]
fn compress(
    ancestor: &mut IndexSlice<PreorderIndex, [PreorderIndex]>,
    last_linked: Option<PreorderIndex>,
    semi: &IndexSlice<PreorderIndex, [PreorderIndex]>,
    label: &mut IndexSlice<PreorderIndex, [PreorderIndex]>,
    v: PreorderIndex,
) {
    assert!(is_processed(v, last_linked));
    let mut stack: SmallVec<[_; 8]> = smallvec::smallvec![v];
    let mut u = ancestor[v];

    while is_processed(u, last_linked) {
        stack.push(u);
        u = ancestor[u];
    }

    for w in stack.windows(2).rev() {
        let &[v, u] = w else { unreachable!() };
        if semi[label[u]] < semi[label[v]] {
            label[v] = label[u];
        }
        ancestor[v] = ancestor[u];
    }
}
