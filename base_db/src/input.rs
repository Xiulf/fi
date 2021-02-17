use relative_path::{RelativePath, RelativePathBuf};
use rustc_hash::FxHashMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FileId(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SourceRootId(pub u32);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SourceRoot {
    pub is_lib: bool,
    files: FxHashMap<FileId, RelativePathBuf>,
}

impl SourceRoot {
    pub fn new_local() -> Self {
        SourceRoot {
            is_lib: false,
            files: FxHashMap::default(),
        }
    }

    pub fn new_library() -> Self {
        SourceRoot {
            is_lib: true,
            files: FxHashMap::default(),
        }
    }

    pub fn insert_file(&mut self, file_id: FileId, path: impl AsRef<RelativePath>) {
        self.files.insert(file_id, path.as_ref().to_relative_path_buf());
    }

    pub fn remove_file(&mut self, file_id: FileId) -> bool {
        self.files.remove(&file_id).is_some()
    }

    pub fn relative_path(&self, file_id: FileId) -> &RelativePath {
        &self.files[&file_id]
    }

    pub fn files(&self) -> impl Iterator<Item = FileId> + '_ {
        self.files.keys().copied()
    }
}
