use std::hash::Hash;
use std::marker::PhantomData;
use std::ops::{Index, IndexMut};

use anymap::Map;
use rustc_hash::FxHashMap;

pub struct Key<K, V, P = (K, V)> {
    _marker: PhantomData<(K, V, P)>,
}

pub trait Policy {
    type K;
    type V;

    fn insert(map: &mut DynMap, key: Self::K, value: Self::V);
    fn get<'a>(map: &'a DynMap, key: &Self::K) -> Option<&'a Self::V>;
}

pub struct DynMap {
    pub(crate) map: Map,
}

#[repr(transparent)]
pub struct KeyMap<KEY> {
    map: DynMap,
    _marker: PhantomData<KEY>,
}

impl Default for DynMap {
    fn default() -> Self {
        DynMap { map: Map::new() }
    }
}

impl<P: Policy> KeyMap<Key<P::K, P::V, P>> {
    pub fn insert(&mut self, key: P::K, value: P::V) {
        P::insert(&mut self.map, key, value)
    }

    pub fn get(&self, key: &P::K) -> Option<&P::V> {
        P::get(&self.map, key)
    }
}

impl<K, V, P> Key<K, V, P> {
    pub const fn new() -> Self {
        Key { _marker: PhantomData }
    }
}

impl<K, V, P> Clone for Key<K, V, P> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<K, V, P> Copy for Key<K, V, P> {
}

impl<K: Hash + Eq + 'static, V: 'static> Policy for (K, V) {
    type K = K;
    type V = V;

    fn insert(map: &mut DynMap, key: Self::K, value: Self::V) {
        map.map
            .entry::<FxHashMap<K, V>>()
            .or_insert_with(Default::default)
            .insert(key, value);
    }

    fn get<'a>(map: &'a DynMap, key: &Self::K) -> Option<&'a Self::V> {
        map.map.get::<FxHashMap<K, V>>()?.get(key)
    }
}

impl<P: Policy> Index<Key<P::K, P::V, P>> for DynMap {
    type Output = KeyMap<Key<P::K, P::V, P>>;

    fn index(&self, _: Key<P::K, P::V, P>) -> &Self::Output {
        unsafe { std::mem::transmute::<&DynMap, &KeyMap<Key<P::K, P::V, P>>>(self) }
    }
}

impl<P: Policy> IndexMut<Key<P::K, P::V, P>> for DynMap {
    fn index_mut(&mut self, _: Key<P::K, P::V, P>) -> &mut Self::Output {
        unsafe { std::mem::transmute::<&mut DynMap, &mut KeyMap<Key<P::K, P::V, P>>>(self) }
    }
}
