use crate::input::SourceRootId;
use rustc_hash::FxHashMap;
use std::ops::Index;

#[derive(Debug, Clone)]
pub struct LibData {
    pub source_root: SourceRootId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LibId(pub u32);

#[derive(Default, Debug, Clone)]
pub struct LibSet {
    libs: FxHashMap<LibId, LibData>,
}

impl LibSet {
    pub fn add_lib(&mut self, source_root: SourceRootId) -> LibId {
        let data = LibData { source_root };
        let lib_id = LibId(self.libs.len() as u32);

        self.libs.insert(lib_id, data);
        lib_id
    }

    pub fn iter(&self) -> impl Iterator<Item = LibId> + '_ {
        self.libs.keys().copied()
    }
}

impl Index<LibId> for LibSet {
    type Output = LibData;

    fn index(&self, index: LibId) -> &Self::Output {
        &self.libs[&index]
    }
}
