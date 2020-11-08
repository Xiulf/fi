use std::collections::HashSet;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FileId(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SourceRootId(pub u32);

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct SourceRoot {
    files: HashSet<FileId>,
}

impl SourceRoot {
    pub fn new() -> Self {
        SourceRoot {
            files: HashSet::new(),
        }
    }

    pub fn insert_file(&mut self, file_id: FileId) {
        self.files.insert(file_id);
    }

    pub fn remove_file(&mut self, file_id: FileId) {
        self.files.remove(&file_id);
    }

    pub fn files(&self) -> impl Iterator<Item = FileId> + '_ {
        self.files.iter().copied()
    }
}
