pub mod file_set;
mod vfs_path;

use std::hash::BuildHasherDefault;

use indexmap::IndexSet;
use rustc_hash::FxHasher;
pub use vfs_path::VfsPath;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FileId(pub u32);

#[derive(Default)]
pub struct VirtualFileSystem {
    interner: PathInterner,
    file_contents: Vec<Option<Box<[u8]>>>,
    changes: Vec<ChangedFile>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ChangedFile {
    pub file_id: FileId,
    pub kind: ChangeKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ChangeKind {
    Create,
    Modify,
    Delete,
}

#[derive(Default)]
struct PathInterner {
    map: IndexSet<VfsPath, BuildHasherDefault<FxHasher>>,
}

impl ChangedFile {
    pub fn is_created_or_deleted(&self) -> bool {
        matches!(self.kind, ChangeKind::Create | ChangeKind::Delete)
    }
}

impl VirtualFileSystem {
    pub fn has_changes(&self) -> bool {
        !self.changes.is_empty()
    }

    pub fn take_changes(&mut self) -> Vec<ChangedFile> {
        std::mem::take(&mut self.changes)
    }

    pub fn file_id(&self, path: &VfsPath) -> Option<FileId> {
        self.interner.get(path).filter(|&file_id| self.get(file_id).is_some())
    }

    pub fn file_path(&self, file_id: FileId) -> &VfsPath {
        self.interner.lookup(file_id)
    }

    pub fn file_content(&self, file_id: FileId) -> Option<&[u8]> {
        self.get(file_id).as_deref()
    }

    pub fn iter(&self) -> impl Iterator<Item = (FileId, &VfsPath)> + '_ {
        self.file_contents
            .iter()
            .enumerate()
            .filter(|(_, content)| content.is_some())
            .map(move |(id, _)| {
                let file_id = FileId(id as u32);
                let path = self.interner.lookup(file_id);

                (file_id, path)
            })
    }

    pub fn set_file_content(&mut self, path: VfsPath, content: Option<Box<[u8]>>) -> (FileId, bool) {
        let file_id = self.alloc_file_id(path);
        let kind = match (self.get(file_id), &content) {
            | (None, None) => return (file_id, false),
            | (None, Some(_)) => ChangeKind::Create,
            | (Some(_), None) => ChangeKind::Delete,
            | (Some(old), Some(new)) if old == new => return (file_id, false),
            | (Some(_), Some(_)) => ChangeKind::Modify,
        };

        *self.get_mut(file_id) = content;
        self.changes.push(ChangedFile { file_id, kind });

        (file_id, true)
    }

    pub fn alloc_file_id(&mut self, path: VfsPath) -> FileId {
        let file_id = self.interner.intern(path);
        let idx = file_id.0 as usize;
        let len = self.file_contents.len().max(idx + 1);

        self.file_contents.resize(len, None);
        file_id
    }

    fn get(&self, file_id: FileId) -> &Option<Box<[u8]>> {
        &self.file_contents[file_id.0 as usize]
    }

    fn get_mut(&mut self, file_id: FileId) -> &mut Option<Box<[u8]>> {
        &mut self.file_contents[file_id.0 as usize]
    }
}

impl PathInterner {
    fn get(&self, path: &VfsPath) -> Option<FileId> {
        self.map.get_index_of(path).map(|i| FileId(i as u32))
    }

    fn intern(&mut self, path: VfsPath) -> FileId {
        let (id, _) = self.map.insert_full(path);

        FileId(id as u32)
    }

    fn lookup(&self, id: FileId) -> &VfsPath {
        self.map.get_index(id.0 as usize).unwrap()
    }
}

impl ra_ap_stdx::hash::NoHashHashable for FileId {
}
