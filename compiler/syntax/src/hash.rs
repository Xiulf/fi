use std::hash::{Hash, Hasher};

pub fn hash<H: StableHash + ?Sized>(val: &H) -> u32 {
    let mut h = fxhash::FxHasher32::default();
    val.stable_hash(&mut h);
    h.finish() as u32
}

pub trait StableHash {
    fn stable_hash<H: Hasher>(&self, h: &mut H);
}

impl<T> StableHash for T
where
    T: Hash,
{
    #[inline(always)]
    fn stable_hash<H: Hasher>(&self, h: &mut H) {
        self.hash(h);
    }
}
