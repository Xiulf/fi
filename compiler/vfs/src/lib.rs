pub mod file_set;
pub mod in_file;
mod vfs_path;

use std::collections::hash_map::Entry;

pub use file_set::FileSet;
pub use in_file::InFile;
use rustc_hash::FxHashMap;
pub use vfs_path::VfsPath;

pub trait Db: salsa::DbWithJar<Jar> {}

impl<T: ?Sized + salsa::DbWithJar<Jar>> Db for T {
}

#[salsa::jar(db = Db)]
pub struct Jar(File);

#[salsa::input]
pub struct File {
    #[return_ref]
    pub path: VfsPath,
    #[return_ref]
    pub text: Option<Box<str>>,
}

#[derive(Default)]
pub struct VirtualFileSystem {
    files: FxHashMap<VfsPath, File>,
    changes: Vec<ChangedFile>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ChangedFile {
    pub file: File,
    pub kind: ChangeKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ChangeKind {
    Create,
    Modify,
    Delete,
}

impl File {
    pub const SOURCE_FILE_EXTENSION: &'static str = "fi";
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

    pub fn file(&self, path: &VfsPath) -> Option<File> {
        self.files.get(path).copied()
    }

    pub fn iter<'a>(&'a self, db: &'a dyn Db) -> impl Iterator<Item = (File, &'a VfsPath)> + 'a {
        self.files
            .iter()
            .filter(|(_, f)| f.text(db).is_some())
            .map(|(p, f)| (*f, p))
    }

    pub fn set_file_content(&mut self, db: &mut dyn Db, path: VfsPath, content: Option<Box<str>>) -> (File, bool) {
        let file = self.alloc_file(db, path);
        let old = file.text(db);
        let kind = match (old, &content) {
            | (None, None) => return (file, false),
            | (None, Some(_)) => ChangeKind::Create,
            | (Some(_), None) => ChangeKind::Delete,
            | (Some(old), Some(new)) if old == new => return (file, false),
            | (Some(_), Some(_)) => ChangeKind::Modify,
        };

        file.set_text(db).to(content);
        self.changes.push(ChangedFile { file, kind });
        (file, true)
    }

    pub fn alloc_file(&mut self, db: &dyn Db, path: VfsPath) -> File {
        match self.files.entry(path.clone()) {
            | Entry::Occupied(e) => *e.get(),
            | Entry::Vacant(e) => {
                let file = File::new(db, path, None);
                e.insert(file);
                file
            },
        }
    }
}

impl nohash_hasher::IsEnabled for File {
}
