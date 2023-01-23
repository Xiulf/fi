use vfs::file_set::FileSet;
pub use vfs::File;
use vfs::VfsPath;

use crate::Db;

#[salsa::input]
pub struct SourceRoot {
    #[return_ref]
    file_set: FileSet,
}

impl SourceRoot {
    pub fn path_for_file(self, db: &dyn Db, file: File) -> Option<&VfsPath> {
        self.file_set(db).path_for_file(file)
    }

    pub fn file_for_path(self, db: &dyn Db, path: &VfsPath) -> Option<File> {
        self.file_set(db).file_for_path(path)
    }

    pub fn iter(self, db: &dyn Db) -> impl Iterator<Item = File> + '_ {
        self.file_set(db).iter()
    }

    pub fn source_files(self, db: &dyn Db) -> impl Iterator<Item = File> + '_ {
        let file_set = self.file_set(db);

        file_set.iter().filter_map(|file| {
            let path = file_set.path_for_file(file)?;
            let ext = path.name_and_extension()?.1?;

            if ext == File::SOURCE_FILE_EXTENSION {
                Some(file)
            } else {
                None
            }
        })
    }
}
