use std::path::Path;
use tempfile::NamedTempFile;

pub struct ObjectFile {
    obj_file: NamedTempFile,
}

impl ObjectFile {
    pub fn new() -> Self {
        ObjectFile {
            obj_file: NamedTempFile::new().unwrap(),
        }
    }

    pub fn write(&mut self, bytes: &[u8]) {
        use std::io::Write;
        self.obj_file.write(bytes).unwrap();
    }

    pub fn path(&self) -> &Path {
        self.obj_file.path()
    }

    pub fn copy(&self, path: &Path) {
        std::fs::copy(self.path(), path).unwrap();
    }
}
