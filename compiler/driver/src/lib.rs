use std::io;
use std::path::{Path, PathBuf};

use base_db::input::SourceRoot;
use base_db::libs::{LibId, LibKind};

mod db;
mod diagnostics;

#[derive(Default)]
pub struct Driver {
    db: db::Database,
    vfs: vfs::VirtualFileSystem,
    libs: base_db::libs::LibSet,
}

impl Driver {
    pub fn load_file(&mut self, path: impl AsRef<Path>) -> io::Result<vfs::File> {
        let path = paths::AbsPathBuf::assert(path.as_ref().canonicalize()?);
        let content = std::fs::read_to_string(&path)?;
        let (file, _) = self
            .vfs
            .set_file_content(&mut self.db, path.into(), Some(content.into_boxed_str()));

        Ok(file)
    }

    pub fn load_files(&mut self, files: Vec<PathBuf>) -> io::Result<vfs::FileSet> {
        let mut set = vfs::FileSet::default();

        for path in files {
            let file = self.load_file(&path)?;
            let path = file.path(&self.db).clone();
            set.insert(file, path);
        }

        Ok(set)
    }

    pub fn create_lib(&mut self, file_set: vfs::FileSet) -> LibId {
        let source_root = SourceRoot::new(&self.db, file_set);

        self.libs.add_lib(&self.db, "", LibKind::Executable, source_root)
    }

    pub fn debug(&self, lib: LibId) {
        use salsa::debug::DebugWithDb;
        let def_map = hir::def_map::query(&self.db, lib);

        // eprintln!("{:#?}", lib.debug_all(&self.db));
        eprintln!("{:#?}", def_map.debug_all(&self.db));
    }
}
