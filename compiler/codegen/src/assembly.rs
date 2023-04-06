use triomphe::Arc;

#[derive(Debug, PartialEq, Eq)]
pub struct Assembly {
    lib: hir::id::LibId,
    objects: Vec<Arc<ObjectFile>>,
}

#[derive(Debug)]
pub struct ObjectFile {
    path: tempfile::TempPath,
}

impl Assembly {
    pub fn new(lib: hir::id::LibId, objects: Vec<Arc<ObjectFile>>) -> Self {
        Self { lib, objects }
    }
}

impl ObjectFile {
    pub fn new(path: tempfile::TempPath) -> Self {
        Self { path }
    }

    #[inline]
    pub fn path(&self) -> &std::path::Path {
        self.path.as_ref()
    }
}

impl PartialEq for ObjectFile {
    fn eq(&self, other: &Self) -> bool {
        self.path() == other.path()
    }
}

impl Eq for ObjectFile {
}
