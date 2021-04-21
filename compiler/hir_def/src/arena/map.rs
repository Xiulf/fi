use super::Idx;
use std::marker::PhantomData;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ArenaMap<IDX, V> {
    v: Vec<Option<V>>,
    _marker: PhantomData<IDX>,
}

impl<IDX, V> Default for ArenaMap<IDX, V> {
    fn default() -> Self {
        ArenaMap {
            v: Vec::new(),
            _marker: PhantomData,
        }
    }
}

impl<T, V> ArenaMap<Idx<T>, V> {
    pub fn insert(&mut self, idx: Idx<T>, t: V) {
        let idx = Self::to_idx(idx);

        self.v.resize_with((idx + 1).max(self.v.len()), || None);
        self.v[idx] = Some(t);
    }

    pub fn get(&self, idx: Idx<T>) -> Option<&V> {
        self.v.get(Self::to_idx(idx)).and_then(Option::as_ref)
    }

    pub fn get_mut(&mut self, idx: Idx<T>) -> Option<&mut V> {
        self.v.get_mut(Self::to_idx(idx)).and_then(Option::as_mut)
    }

    pub fn values(&self) -> impl Iterator<Item = &V> {
        self.v.iter().filter_map(Option::as_ref)
    }

    pub fn values_mut(&mut self) -> impl Iterator<Item = &mut V> {
        self.v.iter_mut().filter_map(Option::as_mut)
    }

    pub fn iter(&self) -> impl Iterator<Item = (Idx<T>, &V)> {
        self.v
            .iter()
            .enumerate()
            .filter_map(|(idx, o)| Some((Self::from_idx(idx), o.as_ref()?)))
    }

    fn to_idx(idx: Idx<T>) -> usize {
        u32::from(idx.into_raw()) as usize
    }

    fn from_idx(idx: usize) -> Idx<T> {
        Idx::from_raw((idx as u32).into())
    }
}

impl<T, V> std::ops::Index<Idx<T>> for ArenaMap<Idx<T>, V> {
    type Output = V;

    fn index(&self, idx: Idx<T>) -> &V {
        self.v[Self::to_idx(idx)].as_ref().unwrap()
    }
}
